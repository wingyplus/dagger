-module(dagger_core_graphql_client).

-callback request(binary(), binary(), map(), [tuple()]) ->
    {ok, non_neg_integer(), map()} | {error, term()}.
