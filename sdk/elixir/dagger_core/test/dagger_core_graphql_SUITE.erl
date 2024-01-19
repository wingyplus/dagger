-module(dagger_core_graphql_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([test_request/1, test_request_error/1]).

all() -> [test_request, test_request_error].

% This test requires to run under `dagger run`.
test_request(_Config) ->
    Port = os:getenv("DAGGER_SESSION_PORT"),
    Token = os:getenv("DAGGER_SESSION_TOKEN"),
    Url = iolist_to_binary(["http://127.0.0.1:", Port, "/query"]),
    %% erlfmt-ignore
    Query = <<
        "query {"
        "  container {"
        "    from(address:\"alpine\") {"
        "      withExec(args:[\"echo\",\"Hello\"]) {"
        "        stdout"
        "      }"
        "    }"
        "  }"
        "}"
    >>,
    {ok, Result} =
        dagger_core_graphql:request(dagger_core_graphql_httpc, Url, Token, Query, #{}, []),
    #{
        <<"data">> := #{
            <<"container">> := #{
                <<"from">> := #{<<"withExec">> := #{<<"stdout">> := <<"Hello\n">>}}
            }
        }
    } = Result.

test_request_error(_Config) ->
	Port = os:getenv("DAGGER_SESSION_PORT"),
    Token = os:getenv("DAGGER_SESSION_TOKEN"),
    Url = iolist_to_binary(["http://127.0.0.1:", Port, "/query"]),
    %% erlfmt-ignore
    Query = <<
        "query {" 
    >>,
    {error, Result} =
        dagger_core_graphql:request(dagger_core_graphql_httpc, Url, Token, Query, #{}, []),
    #{
        <<"data">> := null,
	<<"errors">> := [_ | _]
    } = Result.
