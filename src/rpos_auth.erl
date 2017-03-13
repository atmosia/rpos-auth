%%%-------------------------------------------------------------------
%% @doc rpos_auth public API
%% @end
%%%-------------------------------------------------------------------

-module(rpos_auth).

-behaviour(application).

%% Public API
-export([get_user_permissions/1, add_user_permissions/3]).
-export([remove_user_permissions/3]).

-export([add_api_key/2, remove_api_key/2, add_api_permissions/3]).
-export([remove_api_permissions/3, get_api_permissions/1]).

%% application callbacks
-export([start/2, stop/1]).

-define(LISTENER, rpos_auth_listener).

%%====================================================================
%% Public API
%%====================================================================

%% user api
get_user_permissions(Session) ->
    rpos_auth_server:get_user_permissions(server_pid(), Session).

add_user_permissions(User, Author, PermissionSet) ->
    rpos_auth_server:add_user_permissions(server_pid(), User, Author,
                                          PermissionSet).

remove_user_permissions(User, Author, PermissionSet) ->
    rpos_auth_server:remove_user_permissions(server_pid(), User, Author,
                                             PermissionSet).

%% apikey api
add_api_key(Key, Owner) ->
    rpos_auth_server:create_api_key(server_pid(), Key, Owner).

remove_api_key(Key, Author) ->
    rpos_auth_server:remove_api_key(server_pid(), Key, Author).

add_api_permissions(Key, Author, PermissionSet) ->
    rpos_auth_server:add_api_permissions(server_pid(), Key, Author,
                                         PermissionSet).

remove_api_permissions(Key, Author, PermissionSet) ->
    rpos_auth_server:remove_api_permissions(server_pid(), Key, Author,
                                            PermissionSet).

get_api_permissions(Key) ->
    rpos_auth_server:get_api_permissions(server_pid(), Key).

%%====================================================================
%% application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", status_handler, []},
               {"/session/:token", permission_handler, []},
               {"/user/:user", user_handler, []},
               {"/api", api_post_handler, []},
               {"/api/:key", api_handler, []}
              ]}
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
