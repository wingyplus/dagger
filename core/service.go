package core

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"net"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/dagger/dagger/core/pipeline"
	"github.com/dagger/dagger/core/resourceid"
	"github.com/dagger/dagger/engine"
	"github.com/dagger/dagger/engine/buildkit"
	"github.com/dagger/dagger/network"
	bkgw "github.com/moby/buildkit/frontend/gateway/client"
	"github.com/moby/buildkit/solver/pb"
	"github.com/opencontainers/go-digest"
	"github.com/pkg/errors"
	"github.com/vito/progrock"
	"golang.org/x/sync/errgroup"
)

type Service struct {
	// Container is the container to run as a service.
	Container *Container `json:"container"`

	// Upstream is the service that this service is proxying to.
	ProxyUpstream *Service `json:"upstream,omitempty"`
	// ProxyUpstreamPort is the port for the proxy to send traffic to.
	ProxyUpstreamPort int `json:"proxy_port,omitempty"`
	// ProxyListenAddress is the host address that the proxy listens on.
	ProxyListenAddress string `json:"proxy_listen_address,omitempty"`
	// ProxyProtocol is the protocol for traffic proxied to upstream.
	ProxyProtocol NetworkProtocol `json:"proxy_protocol,omitempty"`

	// ReverseProxyUpstreamAddr is the address for the reverse proxy to request
	// through the host.
	ReverseProxyUpstreamAddr string `json:"reverse_proxy_upstream_addr,omitempty"`
	// ReverseProxyExposedPort is the port for the reverse proxy service to expose
	ReverseProxyExposedPort int `json:"reverse_proxy_exposed_port,omitempty"`
	// ReverseProxyProtocol is the protocol for traffic proxied to upstream.
	ReverseProxyProtocol NetworkProtocol `json:"reverse_proxy_protocol,omitempty"`
}

func NewContainerService(ctr *Container) *Service {
	return &Service{
		Container: ctr,
	}
}

func NewProxyService(upstream *Service, addr string, port int, proto NetworkProtocol) *Service {
	return &Service{
		ProxyUpstream:      upstream,
		ProxyUpstreamPort:  port,
		ProxyListenAddress: addr,
		ProxyProtocol:      proto,
	}
}

func NewReverseProxyService(upstreamAddr string, exposedPort int, proto NetworkProtocol) *Service {
	return &Service{
		ReverseProxyUpstreamAddr: upstreamAddr,
		ReverseProxyExposedPort:  exposedPort,
		ReverseProxyProtocol:     proto,
	}
}

type ServiceID string

func (id ServiceID) String() string {
	return string(id)
}

// ServiceID is digestible so that smaller hashes can be displayed in
// --debug vertex names.
var _ Digestible = ServiceID("")

func (id ServiceID) Digest() (digest.Digest, error) {
	svc, err := id.ToService()
	if err != nil {
		return "", err
	}
	return svc.Digest()
}

func (id ServiceID) ToService() (*Service, error) {
	var service Service

	if id == "" {
		// scratch
		return &service, nil
	}

	if err := resourceid.Decode(&service, id); err != nil {
		return nil, err
	}

	return &service, nil
}

// ID marshals the service into a content-addressed ID.
func (svc *Service) ID() (ServiceID, error) {
	return resourceid.Encode[ServiceID](svc)
}

var _ pipeline.Pipelineable = (*Service)(nil)

// Clone returns a deep copy of the container suitable for modifying in a
// WithXXX method.
func (svc *Service) Clone() *Service {
	cp := *svc
	if cp.Container != nil {
		cp.Container = cp.Container.Clone()
	}
	if cp.ProxyUpstream != nil {
		cp.ProxyUpstream = cp.ProxyUpstream.Clone()
	}
	return &cp
}

// PipelinePath returns the service's pipeline path.
func (svc *Service) PipelinePath() pipeline.Path {
	switch {
	case svc.Container != nil:
		return svc.Container.Pipeline
	case svc.ProxyUpstream != nil:
		return svc.ProxyUpstream.PipelinePath()
	default:
		return pipeline.Path{}
	}
}

// Service is digestible so that it can be recorded as an output of the
// --debug vertex that created it.
var _ Digestible = (*Service)(nil)

// Digest returns the service's content hash.
func (svc *Service) Digest() (digest.Digest, error) {
	return stableDigest(svc)
}

func (svc *Service) Hostname(ctx context.Context) (string, error) {
	switch {
	case svc.ProxyUpstream != nil:
		upstream, err := AllServices.Get(ctx, svc)
		if err != nil {
			return "", err
		}

		host, _, err := net.SplitHostPort(upstream.Addr)
		if err != nil {
			return "", err
		}

		return host, nil
	case svc.Container != nil,
		svc.ReverseProxyUpstreamAddr != "":
		dig, err := svc.Digest()
		if err != nil {
			return "", err
		}

		return network.HostHash(dig), nil
	default:
		return "", errors.New("unknown service type")
	}
}

func (svc *Service) Endpoint(ctx context.Context, port int, scheme string) (string, error) {
	var host string
	var err error
	switch {
	case svc.Container != nil:
		host, err = svc.Hostname(ctx)
		if err != nil {
			return "", err
		}

		if port == 0 {
			if len(svc.Container.Ports) == 0 {
				return "", fmt.Errorf("no ports exposed")
			}

			port = svc.Container.Ports[0].Port
		}
	case svc.ProxyUpstream != nil:
		proxy, err := AllServices.Get(ctx, svc)
		if err != nil {
			return "", err
		}

		var portStr string
		host, portStr, err = net.SplitHostPort(proxy.Addr)
		if err != nil {
			return "", err
		}

		if port == 0 {
			port, err = strconv.Atoi(portStr)
			if err != nil {
				return "", err
			}
		}
	case svc.ReverseProxyUpstreamAddr != "":
		host, err = svc.Hostname(ctx)
		if err != nil {
			return "", err
		}

		if port == 0 {
			port = svc.ReverseProxyExposedPort
		}
	default:
		return "", fmt.Errorf("unknown service type")
	}

	endpoint := fmt.Sprintf("%s:%d", host, port)
	if scheme != "" {
		endpoint = scheme + "://" + endpoint
	}

	return endpoint, nil
}

func (svc *Service) Start(ctx context.Context, bk *buildkit.Client) (running *RunningService, err error) {
	switch {
	case svc.Container != nil:
		return svc.startContainer(ctx, bk)
	case svc.ProxyUpstream != nil:
		return svc.startProxy(ctx, bk)
	case svc.ReverseProxyUpstreamAddr != "":
		return svc.startReverseProxy(ctx, bk)
	default:
		return nil, fmt.Errorf("unknown service type")
	}
}

func (svc *Service) startContainer(ctx context.Context, bk *buildkit.Client) (running *RunningService, err error) {
	dig, err := svc.Digest()
	if err != nil {
		return nil, err
	}

	host, err := svc.Hostname(ctx)
	if err != nil {
		return nil, err
	}

	rec := progrock.RecorderFromContext(ctx).WithGroup(
		fmt.Sprintf("service %s", host),
		progrock.Weak(),
	)

	clientMetadata, err := engine.ClientMetadataFromContext(ctx)
	if err != nil {
		return nil, err
	}

	ctr := svc.Container

	dag, err := defToDAG(ctr.FS)
	if err != nil {
		return nil, err
	}

	if dag.GetOp() == nil && len(dag.inputs) == 1 {
		dag = dag.inputs[0]
	} else {
		// i mean, theoretically this should never happen, but it's better to
		// notice it
		return nil, fmt.Errorf("what in tarnation? that's too many inputs! (%d) %v", len(dag.inputs), dag.GetInputs())
	}

	execOp, ok := dag.AsExec()
	if !ok {
		return nil, fmt.Errorf("expected exec op, got %T", dag.GetOp())
	}

	detachDeps, _, err := StartServices(ctx, bk, ctr.Services)
	if err != nil {
		return nil, fmt.Errorf("start dependent services: %w", err)
	}

	defer func() {
		if err != nil {
			detachDeps()
		}
	}()

	vtx := rec.Vertex(dig, "start "+strings.Join(execOp.Meta.Args, " "))
	defer func() {
		if err != nil {
			vtx.Error(err)
		}
	}()

	fullHost := host + "." + network.ClientDomain(clientMetadata.ClientID)

	health := newHealth(bk, fullHost, ctr.Ports)

	pbPlatform := pb.PlatformFromSpec(ctr.Platform)

	mounts := make([]bkgw.Mount, len(execOp.Mounts))
	for i, m := range execOp.Mounts {
		mount := bkgw.Mount{
			Selector:  m.Selector,
			Dest:      m.Dest,
			ResultID:  m.ResultID,
			Readonly:  m.Readonly,
			MountType: m.MountType,
			CacheOpt:  m.CacheOpt,
			SecretOpt: m.SecretOpt,
			SSHOpt:    m.SSHOpt,
			// TODO(vito): why is there no TmpfsOpt? PR upstream?
			// TmpfsOpt  *TmpfsOpt   `protobuf:"bytes,19,opt,name=TmpfsOpt,proto3" json:"TmpfsOpt,omitempty"`
		}

		if m.Input > -1 {
			input := execOp.Input(m.Input)
			def, err := input.Marshal()
			if err != nil {
				return nil, fmt.Errorf("marshal mount %s: %w", m.Dest, err)
			}

			res, err := bk.Solve(ctx, bkgw.SolveRequest{
				Definition: def,
			})
			if err != nil {
				return nil, fmt.Errorf("solve mount %s: %w", m.Dest, err)
			}

			mount.Ref = res.Ref
		}

		mounts[i] = mount
	}

	gc, err := bk.NewContainer(ctx, bkgw.NewContainerRequest{
		Mounts:   mounts,
		Hostname: fullHost,
		Platform: &pbPlatform,
	})
	if err != nil {
		return nil, fmt.Errorf("new container: %w", err)
	}

	defer func() {
		if err != nil {
			gc.Release(context.Background())
		}
	}()

	checked := make(chan error, 1)
	go func() {
		checked <- health.Check(ctx)
	}()

	outBuf := new(bytes.Buffer)
	svcProc, err := gc.Start(ctx, bkgw.StartRequest{
		Args:         execOp.Meta.Args,
		Env:          append(execOp.Meta.Env, proxyEnvList(execOp.Meta.ProxyEnv)...),
		Cwd:          execOp.Meta.Cwd,
		User:         execOp.Meta.User,
		SecretEnv:    execOp.Secretenv,
		Tty:          false,
		Stdout:       nopCloser{io.MultiWriter(vtx.Stdout(), outBuf)},
		Stderr:       nopCloser{io.MultiWriter(vtx.Stderr(), outBuf)},
		SecurityMode: execOp.Security,
	})
	if err != nil {
		return nil, fmt.Errorf("start container: %w", err)
	}

	exited := make(chan error, 1)
	go func() {
		exited <- svcProc.Wait()

		// detach dependent services when process exits
		detachDeps()
	}()

	stopSvc := func(ctx context.Context) (stopErr error) {
		defer func() {
			vtx.Done(stopErr)
		}()

		// TODO(vito): graceful shutdown?
		if err := svcProc.Signal(ctx, syscall.SIGKILL); err != nil {
			return fmt.Errorf("signal: %w", err)
		}

		if err := gc.Release(ctx); err != nil {
			// TODO(vito): returns context.Canceled, which is a bit strange, because
			// that's the goal
			if !errors.Is(err, context.Canceled) {
				return fmt.Errorf("release: %w", err)
			}
		}

		return nil
	}

	select {
	case err := <-checked:
		if err != nil {
			return nil, fmt.Errorf("health check errored: %w", err)
		}

		return &RunningService{
			Service: svc,
			Addr:    fullHost,
			Key: ServiceKey{
				Digest:   dig,
				ClientID: clientMetadata.ClientID,
			},
			Stop: stopSvc,
		}, nil
	case err := <-exited:
		if err != nil {
			return nil, fmt.Errorf("exited: %w\noutput: %s", err, outBuf.String())
		}

		return nil, fmt.Errorf("service exited before healthcheck")
	}
}

func proxyEnvList(p *pb.ProxyEnv) []string {
	out := []string{}
	if v := p.HttpProxy; v != "" {
		out = append(out, "HTTP_PROXY="+v, "http_proxy="+v)
	}
	if v := p.HttpsProxy; v != "" {
		out = append(out, "HTTPS_PROXY="+v, "https_proxy="+v)
	}
	if v := p.FtpProxy; v != "" {
		out = append(out, "FTP_PROXY="+v, "ftp_proxy="+v)
	}
	if v := p.NoProxy; v != "" {
		out = append(out, "NO_PROXY="+v, "no_proxy="+v)
	}
	if v := p.AllProxy; v != "" {
		out = append(out, "ALL_PROXY="+v, "all_proxy="+v)
	}
	return out
}

func (svc *Service) startProxy(ctx context.Context, bk *buildkit.Client) (running *RunningService, err error) {
	svcCtx, stop := context.WithCancel(context.Background())

	clientMetadata, err := engine.ClientMetadataFromContext(ctx)
	if err != nil {
		stop()
		return nil, err
	}
	svcCtx = engine.ContextWithClientMetadata(svcCtx, clientMetadata)

	rec := progrock.RecorderFromContext(ctx)
	svcCtx = progrock.RecorderToContext(svcCtx, rec)

	upstream, err := AllServices.Start(svcCtx, bk, svc.ProxyUpstream)
	if err != nil {
		stop()
		return nil, fmt.Errorf("start upstream: %w", err)
	}

	res, closeListener, err := bk.ListenHostToContainer(
		svcCtx,
		svc.ProxyListenAddress,
		svc.ProxyProtocol.Network(),
		fmt.Sprintf("%s:%d", upstream.Addr, svc.ProxyUpstreamPort),
	)
	if err != nil {
		stop()
		return nil, fmt.Errorf("host to container: %w", err)
	}

	dig, err := svc.Digest()
	if err != nil {
		stop()
		return nil, err
	}

	return &RunningService{
		Service: svc,
		Key: ServiceKey{
			Digest:   dig,
			ClientID: clientMetadata.ClientID,
		},
		Addr: res.GetAddr(),
		Stop: func(context.Context) error {
			stop()
			// HACK(vito): do this async to prevent deadlock (this is called in Detach)
			go AllServices.Detach(svcCtx, upstream)
			return closeListener()
		},
	}, nil
}

func (svc *Service) startReverseProxy(ctx context.Context, bk *buildkit.Client) (running *RunningService, err error) {
	dig, err := svc.Digest()
	if err != nil {
		return nil, err
	}

	host, err := svc.Hostname(ctx)
	if err != nil {
		return nil, err
	}

	clientMetadata, err := engine.ClientMetadataFromContext(ctx)
	if err != nil {
		return nil, err
	}

	rec := progrock.RecorderFromContext(ctx)

	svcCtx, stop := context.WithCancel(context.Background())
	svcCtx = engine.ContextWithClientMetadata(svcCtx, clientMetadata)
	svcCtx = progrock.RecorderToContext(svcCtx, rec)

	fullHost := host + "." + network.ClientDomain(clientMetadata.ClientID)

	proxy := newC2HProxy(
		bk,
		fullHost,
		svc.ReverseProxyProtocol,
		svc.ReverseProxyUpstreamAddr,
		svc.ReverseProxyExposedPort,
	)

	check := newHealth(
		bk,
		fullHost,
		[]ContainerPort{
			{
				Port:     svc.ReverseProxyExposedPort,
				Protocol: svc.ReverseProxyProtocol,
			},
		},
	)

	exited := make(chan error, 1)
	go func() {
		exited <- proxy.Proxy(svcCtx)
	}()

	checked := make(chan error, 1)
	go func() {
		checked <- check.Check(svcCtx)
	}()

	select {
	case err := <-checked:
		if err != nil {
			stop()
			return nil, fmt.Errorf("health check errored: %w", err)
		}

		return &RunningService{
			Service: svc,
			Key: ServiceKey{
				Digest:   dig,
				ClientID: clientMetadata.ClientID,
			},
			Addr: fullHost,
			Stop: func(context.Context) error {
				stop()
				return nil
			},
		}, nil
	case err := <-exited:
		stop()
		return nil, fmt.Errorf("proxy exited: %w", err)
	}
}

type RunningService struct {
	Service *Service
	Key     ServiceKey
	Addr    string
	Stop    func(context.Context) error
}

type Services struct {
	starting map[ServiceKey]*sync.WaitGroup
	running  map[ServiceKey]*RunningService
	bindings map[ServiceKey]int
	l        sync.Mutex
}

type ServiceKey struct {
	Digest   digest.Digest
	ClientID string
}

// AllServices is a pesky global variable storing the state of all running
// services.
var AllServices = &Services{
	starting: map[ServiceKey]*sync.WaitGroup{},
	running:  map[ServiceKey]*RunningService{},
	bindings: map[ServiceKey]int{},
}

func (ss *Services) Get(ctx context.Context, svc *Service) (*RunningService, error) {
	clientMetadata, err := engine.ClientMetadataFromContext(ctx)
	if err != nil {
		return nil, err
	}

	dig, err := svc.Digest()
	if err != nil {
		return nil, err
	}

	key := ServiceKey{
		Digest:   dig,
		ClientID: clientMetadata.ClientID,
	}

	notRunningErr := fmt.Errorf("service %s is not running", network.HostHash(dig))

	ss.l.Lock()
	starting, isStarting := ss.starting[key]
	running, isRunning := ss.running[key]
	switch {
	case !isStarting && !isRunning:
		return nil, notRunningErr
	case isRunning:
		ss.l.Unlock()
		return running, nil
	case isStarting:
		ss.l.Unlock()
		starting.Wait()
		ss.l.Lock()
		running, isRunning = ss.running[key]
		ss.l.Unlock()
		if isRunning {
			return running, nil
		}
		return nil, notRunningErr
	default:
		return nil, fmt.Errorf("internal error: unexpected state")
	}
}

func (ss *Services) Start(ctx context.Context, bk *buildkit.Client, svc *Service) (*RunningService, error) {
	clientMetadata, err := engine.ClientMetadataFromContext(ctx)
	if err != nil {
		return nil, err
	}

	dig, err := svc.Digest()
	if err != nil {
		return nil, err
	}

	key := ServiceKey{
		Digest:   dig,
		ClientID: clientMetadata.ClientID,
	}

	ss.l.Lock()
	starting, isStarting := ss.starting[key]
	running, isRunning := ss.running[key]
	switch {
	case !isStarting && !isRunning:
		// not starting or running; start it
		starting = new(sync.WaitGroup)
		starting.Add(1)
		defer starting.Done()
		ss.starting[key] = starting
	case isRunning:
		// already running; increment binding count and return
		ss.bindings[key]++
		ss.l.Unlock()
		return running, nil
	case isStarting:
		// already starting; wait for the attempt to finish and check if it
		// succeeded
		ss.l.Unlock()
		starting.Wait()
		ss.l.Lock()
		running, didStart := ss.running[key]
		if didStart {
			// starting succeeded as normal; return the isntance
			ss.l.Unlock()
			return running, nil
		}
		// starting didn't work; give it another go (this might just error again)
	}
	ss.l.Unlock()

	svcCtx, stop := context.WithCancel(context.Background())
	svcCtx = progrock.RecorderToContext(svcCtx, progrock.RecorderFromContext(ctx))
	if clientMetadata, err := engine.ClientMetadataFromContext(ctx); err == nil {
		svcCtx = engine.ContextWithClientMetadata(svcCtx, clientMetadata)
	}

	running, err = svc.Start(svcCtx, bk)
	if err != nil {
		stop()
		ss.l.Lock()
		delete(ss.starting, key)
		ss.l.Unlock()
		return nil, err
	}

	ss.l.Lock()
	delete(ss.starting, key)
	ss.running[key] = running
	ss.bindings[key] = 1
	ss.l.Unlock()

	_ = stop // leave it running

	return running, nil
}

func (ss *Services) Stop(ctx context.Context, bk *buildkit.Client, svc *Service) error {
	clientMetadata, err := engine.ClientMetadataFromContext(ctx)
	if err != nil {
		return err
	}

	dig, err := svc.Digest()
	if err != nil {
		return err
	}

	key := ServiceKey{
		Digest:   dig,
		ClientID: clientMetadata.ClientID,
	}

	ss.l.Lock()
	defer ss.l.Unlock()

	starting, isStarting := ss.starting[key]
	running, isRunning := ss.running[key]
	switch {
	case isRunning:
		// already running; increment binding count and return
		return ss.stop(ctx, running)
	case isStarting:
		// already starting; wait for the attempt to finish and then stop it
		ss.l.Unlock()
		starting.Wait()
		ss.l.Lock()

		running, didStart := ss.running[key]
		if didStart {
			// starting succeeded as normal; return the isntance
			return ss.stop(ctx, running)
		}

		// starting didn't work; nothing to do
		return nil
	default:
		// not starting or running; nothing to do
		return nil
	}
}

func (ss *Services) Detach(ctx context.Context, svc *RunningService) error {
	ss.l.Lock()
	defer ss.l.Unlock()

	running, found := ss.running[svc.Key]
	if !found {
		// not even running; ignore
		return nil
	}

	ss.bindings[svc.Key]--

	if ss.bindings[svc.Key] > 0 {
		// detached, but other instances still active
		return nil
	}

	return ss.stop(ctx, running)
}

func (ss *Services) stop(ctx context.Context, running *RunningService) error {
	if err := running.Stop(ctx); err != nil {
		return fmt.Errorf("stop: %w", err)
	}

	delete(ss.bindings, running.Key)
	delete(ss.running, running.Key)

	return nil
}

type ServiceBindings []ServiceBinding

type ServiceBinding struct {
	Service  *Service `json:"service"`
	Hostname string   `json:"hostname"`
	Aliases  AliasSet `json:"aliases"`
}

type AliasSet []string

func (set AliasSet) String() string {
	if len(set) == 0 {
		return "no aliases"
	}

	return fmt.Sprintf("aliased as %s", strings.Join(set, ", "))
}

func (set AliasSet) With(alias string) AliasSet {
	for _, a := range set {
		if a == alias {
			return set
		}
	}
	return append(set, alias)
}

func (set AliasSet) Union(other AliasSet) AliasSet {
	for _, a := range other {
		set = set.With(a)
	}
	return set
}

func (bndp *ServiceBindings) Merge(other ServiceBindings) {
	if *bndp == nil {
		*bndp = ServiceBindings{}
	}

	merged := *bndp

	indices := map[string]int{}
	for i, b := range merged {
		indices[b.Hostname] = i
	}

	for _, bnd := range other {
		i, has := indices[bnd.Hostname]
		if !has {
			merged = append(merged, bnd)
			continue
		}

		merged[i].Aliases = merged[i].Aliases.Union(bnd.Aliases)
	}

	*bndp = merged
}

// NetworkProtocol is a string deriving from NetworkProtocol enum
type NetworkProtocol string

const (
	NetworkProtocolTCP NetworkProtocol = "TCP"
	NetworkProtocolUDP NetworkProtocol = "UDP"
)

// Network returns the value appropriate for the "network" argument to Go
// net.Dial, and for appending to the port number to form the key for the
// ExposedPorts object in the OCI image config.
func (p NetworkProtocol) Network() string {
	return strings.ToLower(string(p))
}

func StartServices(ctx context.Context, bk *buildkit.Client, bindings ServiceBindings) (_ func(), _ []*RunningService, err error) {
	running := []*RunningService{}
	detach := func() {
		go func() {
			<-time.After(10 * time.Second)

			for _, svc := range running {
				AllServices.Detach(ctx, svc)
			}
		}()
	}

	defer func() {
		if err != nil {
			detach()
		}
	}()

	// NB: don't use errgroup.WithCancel; we don't want to cancel on Wait
	eg := new(errgroup.Group)

	started := make(chan *RunningService, len(bindings))
	for _, bnd := range bindings {
		bnd := bnd
		eg.Go(func() error {
			runningSvc, err := AllServices.Start(ctx, bk, bnd.Service)
			if err != nil {
				return fmt.Errorf("start %s (%s): %w", bnd.Hostname, bnd.Aliases, err)
			}
			started <- runningSvc
			return nil
		})
	}

	startErr := eg.Wait()

	close(started)

	if startErr != nil {
		return nil, nil, startErr
	}

	for svc := range started {
		running = append(running, svc)
	}

	return detach, running, nil
}

// WithServices runs the given function with the given services started,
// detaching from each of them after the function completes.
func WithServices[T any](ctx context.Context, bk *buildkit.Client, bindings ServiceBindings, fn func() (T, error)) (T, error) {
	var zero T

	detach, _, err := StartServices(ctx, bk, bindings)
	if err != nil {
		return zero, err
	}
	defer detach()

	return fn()
}
