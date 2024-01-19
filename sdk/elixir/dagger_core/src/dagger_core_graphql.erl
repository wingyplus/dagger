-module(dagger_core_graphql).

-export([request/6]).

%% @doc Perform a GraphQL request to the Dagger engine.
request(Client, Url, SessionToken, Query, Variables, Options) ->
    Json = proplists:get_value(json_library, Options, thoas),
    Request = #{query => Query, variables => Variables},
    %% TODO: Make timeout configurable.
    case Client:request(Url, SessionToken, thoas:encode(Request), [{timeout, infinity}]) of
        {ok, 200, Result} ->
            Json:decode(Result);
        % Non-200 error.
        {ok, _, Error} ->
            {ok, Result} = Json:decode(Error),
            {error, Result};
        Otherwise ->
            Otherwise
    end.
