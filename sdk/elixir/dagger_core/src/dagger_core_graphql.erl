-module(dagger_core_graphql).

-export([request/6]).

%% @doc Perform a request into Dagger.
request(Client, Url, SessionToken, Query, Variables, Options) ->
    Request = #{query => Query, variables => Variables},
    %% TODO: Make timeout configurable.
    case Client:request(Url, SessionToken, thoas:encode(Request), [{timeout, infinity}]) of
        {ok, 200, Result} ->
            thoas:decode(Result);
        Otherwise ->
            Otherwise
    end.
