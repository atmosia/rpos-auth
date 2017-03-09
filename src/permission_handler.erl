-module(permission_handler).

-export([init/2, resource_exists/2, content_types_provided/2, to_json/2]).

init(Req, State) -> {cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
    Provided = [{{<<"application">>, <<"json">>, '*'}, to_json}],
    {Provided, Req, State}.

resource_exists(Req, State) ->
    Token = cowboy_req:binding(token, Req),
    case rpos_auth:get_permissions(Token) of
        {ok, Username, Perms, Session} ->
            {true, Req, {Username, Perms, Session}};
        {error, invalid_session_id} -> {false, Req, State}
    end.


to_json(Req, State = {Username, Perms, Session}) ->
    JSON = json:encode(#{username => Username,
                         permisions => Perms,
                         session_token => Session
                        }),
    {JSON, Req, State}.
