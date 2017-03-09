%%%-------------------------------------------------------------------
%% @doc rpos_auth public API
%% @end
%%%-------------------------------------------------------------------

-module(rpos_auth).

-behaviour(application).

%% Public API
-export([get_permissions/1, add_permissions/3, remove_permissions/2]).

%% application callbacks
-export([start/2, stop/1]).

-define(LISTENER, rpos_auth_listener).

%%====================================================================
%% Public API
%%====================================================================

get_permissions(Session) ->
    rpos_auth_server:get_permissions(server_pid(), Session).

add_permissions(User, Author, PermissionSet) ->
    rpos_auth_server:add_permissions(server_pid(), User, Author,
                                     PermissionSet).

remove_permissions(User, PermissionSet) ->
    rpos_auth_server:remove_permissions(server_pid(), User,
                                        PermissionSet).

%%====================================================================
%% application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", status_handler, []},
               {"/permissions/:token", permission_handler, []},
               {"/:user", user_handler, []}]}
    ]),
    Port = application:get_env(rpos_auth, port, 8080),
    {ok, _} = cowboy:start_clear(?LISTENER, 100,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    rpos_auth_sup:start_link().

stop(_State) -> cowboy:stop_listener(?LISTENER).

%%====================================================================
%% Internal functions
%%====================================================================

server_pid() ->
    [{_, Pid, _, _}] = supervisor:which_children(rpos_auth_sup),
    Pid.
