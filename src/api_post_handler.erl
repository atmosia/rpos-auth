-module(api_post_handler).

-export([init/2, is_authorized/2, allowed_methods/2]).
-export([content_types_accepted/2, from_json/2]).

init(Req, State) -> {cowboy_rest, Req, State}.

is_authorized(Req, State) ->
    % TODO: check auth
    {true, Req, State#{author => "mikeyhc"}}.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

from_json(Req, State) ->
    Author = maps:get(author, State),
    Key = rpos_api:gen_key(Author),
    ok = rpos_auth:add_api_key(Key, Author),
    {{true, "/api/" ++ Key}, Req, State}.
