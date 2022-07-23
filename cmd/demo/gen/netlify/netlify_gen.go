// Code generated by github.com/Khan/genqlient, DO NOT EDIT.

package netlify

import (
	"context"

	"github.com/Khan/genqlient/graphql"
	"github.com/dagger/cloak/sdk/go/dagger"
)

// DeployNetlify includes the requested fields of the GraphQL type Netlify.
type DeployNetlify struct {
	Deploy DeployNetlifyDeploy `json:"deploy"`
}

// GetDeploy returns DeployNetlify.Deploy, and is useful for accessing the field via an interface.
func (v *DeployNetlify) GetDeploy() DeployNetlifyDeploy { return v.Deploy }

// DeployNetlifyDeploy includes the requested fields of the GraphQL type Deploy.
type DeployNetlifyDeploy struct {
	Url       string `json:"url"`
	DeployUrl string `json:"deployUrl"`
}

// GetUrl returns DeployNetlifyDeploy.Url, and is useful for accessing the field via an interface.
func (v *DeployNetlifyDeploy) GetUrl() string { return v.Url }

// GetDeployUrl returns DeployNetlifyDeploy.DeployUrl, and is useful for accessing the field via an interface.
func (v *DeployNetlifyDeploy) GetDeployUrl() string { return v.DeployUrl }

// DeployResponse is returned by Deploy on success.
type DeployResponse struct {
	Netlify DeployNetlify `json:"netlify"`
}

// GetNetlify returns DeployResponse.Netlify, and is useful for accessing the field via an interface.
func (v *DeployResponse) GetNetlify() DeployNetlify { return v.Netlify }

// __DeployInput is used internally by genqlient
type __DeployInput struct {
	Contents dagger.FS `json:"contents"`
	Subdir   string    `json:"subdir"`
	SiteName string    `json:"siteName"`
	Token    string    `json:"token"`
}

// GetContents returns __DeployInput.Contents, and is useful for accessing the field via an interface.
func (v *__DeployInput) GetContents() dagger.FS { return v.Contents }

// GetSubdir returns __DeployInput.Subdir, and is useful for accessing the field via an interface.
func (v *__DeployInput) GetSubdir() string { return v.Subdir }

// GetSiteName returns __DeployInput.SiteName, and is useful for accessing the field via an interface.
func (v *__DeployInput) GetSiteName() string { return v.SiteName }

// GetToken returns __DeployInput.Token, and is useful for accessing the field via an interface.
func (v *__DeployInput) GetToken() string { return v.Token }

func Deploy(
	ctx context.Context,
	contents dagger.FS,
	subdir string,
	siteName string,
	token string,
) (*DeployResponse, error) {
	req := &graphql.Request{
		OpName: "Deploy",
		Query: `
query Deploy ($contents: FS!, $subdir: String, $siteName: String!, $token: String!) {
	netlify {
		deploy(contents: $contents, subdir: $subdir, siteName: $siteName, token: $token) {
			url
			deployUrl
		}
	}
}
`,
		Variables: &__DeployInput{
			Contents: contents,
			Subdir:   subdir,
			SiteName: siteName,
			Token:    token,
		},
	}
	var err error
	var client graphql.Client

	client, err = dagger.Client(ctx)
	if err != nil {
		return nil, err
	}

	var data DeployResponse
	resp := &graphql.Response{Data: &data}

	err = client.MakeRequest(
		ctx,
		req,
		resp,
	)

	return &data, err
}
