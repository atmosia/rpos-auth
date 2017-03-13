-module(api_handler).

-export([init/2, is_authorized/2, allowed_methods/2]).
-export([content_types_provided/2, content_types_accepted/2]).
-export([to_json/2, from_json/2]).

init(Req, State) -> {cowboy_rest, Req, State}.

is_authorized(Req, State) ->
    % TODO: check auth if PUT
    {true, Req, State#{author => "mikeyhc"}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

to_json(Req, State) ->
    % TODO: fetch api permissions
    {<<>>, Req, State}.

from_json(Req, State) ->
    % TODO: update api permissions
    {true, Req, State}.
