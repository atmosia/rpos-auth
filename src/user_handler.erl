-module(user_handler).

-export([init/2, is_authorized/2, allowed_methods/2]).
-export([content_types_accepted/2, from_json/2]).

init(Req, _State) -> {cowboy_rest, Req, #{}}.

is_authorized(Req, State) ->
    %% TODO: check auth
    {true, Req, State#{author => "mikeyhc"}}.

allowed_methods(Req, State) ->
    {[<<"PUT">>, <<"OPTIONS">>], Req,State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

% TODO: check for errors
from_json(Req0, State) ->
    User = cowboy_req:binding(user, Req0),
    {Body, Req1} = read_all_body(Req0),
    JSON = jiffy:decode(Body, [return_maps]),
    Author = maps:get(author, State),
    AddTuples = get_tuples(<<"add">>, JSON),
    ok = rpos_auth:add_permissions(User, Author, AddTuples),
    RemoveTuples = get_tuples(<<"remove">>, JSON),
    ok = rpos_auth:remove_permissions(User, Author, RemoveTuples),
    {true, Req1, State}.

read_all_body(Req) ->
    read_all_body(cowboy_req:read_body(Req), <<>>).

read_all_body({ok, Data, Req}, Acc) ->
    {<<Acc/binary, Data/binary>>, Req};
read_all_body({more, Data, Req}, Acc) ->
    read_all_body(cowboy_req:read_body(Req),
                  <<Acc/binary, Data/binary>>).

to_tuples([A, B]) -> {A, B}.

get_tuples(Field, JSON) ->
    lists:map(fun to_tuples/1, maps:get(Field, JSON, [])).
