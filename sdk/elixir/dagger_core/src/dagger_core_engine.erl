-module(dagger_core_engine).

-export([connect/1, query/4]).

-define(ENGINE_VERSION, <<"0.9.3">>).

%% @doc Connect to the Dagger engine.
connect(Options) ->
    case from_session_env(Options) of
        {error, no_session} ->
            from_local_cli(Options);
        Else ->
            Else
    end.

%% @doc Send a query to the engine.
query(#{token := SessionToken} = Conn, Query, Variables, Options) ->
    Url = engine_url(Conn),
    dagger_core_graphql:request(
        dagger_core_graphql_httpc,
        Url,
        SessionToken,
        Query,
        Variables,
        Options
    ).

engine_url(#{port := Port}) ->
    list_to_binary([<<"http://127.0.0.1">>, ":", Port]).

engine_version() ->
    ?ENGINE_VERSION.

%% Using a session from `dagger run`.
from_session_env(Options) ->
    case session_env() of
        {Port, Token} when is_list(Port) andalso is_list(Token) ->
            case proplists:lookup(workdir, Options) of
                none ->
                    {ok, #{port => Port, token => Token}};
                _ ->
                    {error, workdir_configure_on_session}
            end;
        _ ->
            {error, no_session}
    end.

from_local_cli(Options) ->
    case os:getenv("_EXPERIMENTAL_DAGGER_CLI_BIN") of
        false ->
            {error, no_session};
        Bin ->
            case os:find_executable(Bin) of
                false ->
                    {error, no_session};
                BinPath ->
                    dagger_core_engine_session:start(BinPath, Options)
            end
    end.

session_env() ->
    {os:getenv("DAGGER_SESSION_PORT"), os:getenv("DAGGER_SESSION_TOKEN")}.
