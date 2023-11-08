-module(dagger_core_graphql_httpc).

-behaviour(dagger_core_graphql_client).

-export([request/4]).

request(Url, SessionToken, RequestBody, HttpOpts) ->
    Headers =
        [{"Authorization", ["Basic ", base64:encode(list_to_binary([SessionToken, ":"]))]}],
    ContentType = "application/json",
    Request = {Url, Headers, ContentType, RequestBody},
    Options = [],
    %% TODO: Use `maybe` syntax once erlfmt support (Ref: https://github.com/WhatsApp/erlfmt/pull/351).
    case httpc:request(post, Request, HttpOpts, Options) of
        {ok, {{_, StatusCode, _}, _, Response}} ->
            {ok, StatusCode, Response};
        Otherwise ->
            Otherwise
    end.
