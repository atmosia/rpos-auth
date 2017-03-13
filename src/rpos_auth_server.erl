-module(rpos_auth_server).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([get_user_permissions/2, add_user_permissions/4]).
-export([remove_user_permissions/4]).

-export([get_api_permissions/2, create_api_key/3, add_api_permissions/4]).
-export([remove_api_permissions/4, remove_api_key/3]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {connection, login}).

start_link() -> gen_server:start_link(?MODULE, [], []).

start_link(Config) ->
    gen_server:start_link(?MODULE, [{config, Config}], []).

get_user_permissions(Pid, Session) ->
    gen_server:call(Pid, {user_permissions, Session}).

add_user_permissions(Pid, User, Author, PermissionSet) ->
    gen_server:call(Pid, {add_user_permissions, User, Author, PermissionSet}).

remove_user_permissions(Pid, User, Author, PermissionSet) ->
    gen_server:call(Pid, {remove_user_permissions, User, Author,
                          PermissionSet}).

get_api_permissions(Pid, Key) ->
    gen_server:call(Pid, {api_permissions, Key}).

create_api_key(Pid, Key, Owner) ->
    gen_server:call(Pid, {create_api_key, Key, Owner}).

add_api_permissions(Pid, Key, Author, PermissionSet) ->
    gen_server:call(Pid, {add_api_permissions, Key, Author, PermissionSet}).

remove_api_permissions(Pid, Key, Author, PermissionSet) ->
    gen_server:call(Pid, {remove_api_permissions, Key, Author, PermissionSet}).

remove_api_key(Pid, Key, Author) ->
    gen_server:call(Pid, {remove_api_key, Key, Author}).

init(Args) ->
    ConfigPath = proplists:get_value(config, Args, "config.json"),
    case read_config(ConfigPath) of
        {ok, DBConfig, LoginConfig} ->
            gen_server:cast(self(), {db_connect, DBConfig}),
            {ok, #state{login=LoginConfig}};
        {error, Error} -> {stop, Error}
    end.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extras) -> {ok, State}.

handle_call({user_permissions, Session}, _From, State) ->
    case get_username(Session, State#state.login) of
        {ok, Username, Session} ->
            {reply,
             {ok,
              Username,
              get_user_permissions_(State#state.connection, Username),
              Session},
             State};
        Error -> {reply, Error, State}
    end;
handle_call({add_user_permissions, User, Author, PermissionSet}, _From,
            State) ->
    {reply,
     add_user_permissions_(State#state.connection, User, Author,
                           PermissionSet),
     State};
handle_call({remove_user_permissions, User, Author, PermissionSet}, _From,
            State) ->
    {reply,
     remove_user_permissions_(State#state.connection, User, Author,
                              PermissionSet),
     State};
handle_call({api_permissions, Key}, _From, State) ->
    {reply, rpos_api:permissions(State#state.connection, Key), State};
handle_call({add_api_key, Key, Owner}, _From, State) ->
    {reply, rpos_api:new(State#state.connection, Key, Owner), State};
handle_call({add_api_permissions, Key, Author, PermissionSet}, _From,
            State) ->
    Conn = State#state.connection,
    Fn = fun(P) -> rpos_api:add_permission(Conn, Key, Author, P) end,
    % TODO: handle failures
    _Resp = lists:map(Fn, PermissionSet),
    {reply, ok, State};
handle_call({remove_api_permissions, Key, Author, PermissionSet}, _From,
            State) ->
    Conn = State#state.connection,
    Fn = fun(P) -> rpos_api:remove_permission(Conn, Key, Author, P) end,
    % TODO: handle failures
    _Resp = lists:map(Fn, PermissionSet),
    {reply, ok, State};
handle_call({remove_api_key, Key, Author}, _From, State) ->
    {reply, rpos_api:delete(State#state.connection, Key, Author), State}.

handle_cast({db_connect, Config}, State) ->
    {ok, Connection} = epgsql:connect(Config),
    {noreply, State#state{connection=Connection}}.

handle_info(_Message, State) -> {noreply, State}.

read_config(File) ->
    case file:read_file(File) of
        {ok, Data} ->
            {DB, Login} = parse_json_config(jiffy:decode(Data, [])),
            {ok, DB, Login};
        {error, enoent} -> {error, no_config_file}
    end.

parse_json_config({Config}) ->
    {DBConfig} = proplists:get_value(<<"database">>, Config, []),
    DB = lists:foldl(fun parse_db_config/2, [], DBConfig),
    {LoginConfig} = proplists:get_value(<<"login">>, Config, []),
    Login = lists:foldl(fun parse_login_config/2, [], LoginConfig),
    {DB, Login}.

parse_db_config({<<"host">>, Host}, Acc) ->
    [{host, binary:bin_to_list(Host)}|Acc];
parse_db_config({<<"port">>, Port}, Acc) -> [{port, Port}|Acc];
parse_db_config({<<"user">>, User}, Acc) ->
    [{username, binary:bin_to_list(User)}|Acc];
parse_db_config({<<"pass">>, Pass}, Acc) ->
    [{password, binary:bin_to_list(Pass)}|Acc];
parse_db_config({<<"name">>, Name}, Acc) ->
    [{database, binary:bin_to_list(Name)}|Acc].

parse_login_config({<<"host">>, Host}, Acc) -> [{host, Host}|Acc];
parse_login_config({<<"port">>, Port}, Acc) -> [{port, Port}|Acc].

get_username(Session, LoginConfig) ->
    Host = proplists:get_value(host, LoginConfig, "http://localhost"),
    Port = proplists:get_value(port, LoginConfig, "80"),
    URL = lists:flatten(io_lib:format("~s:~w/session/~s",
                                      [Host, Port, Session])),
    {ok, {{_Version, Code, _Reason}, _Headers, Body}} =
        httpc:request(get, {URL, []}, [{autoredirect, true}], [{port, Port}]),
    if  Code =:= 200 ->
            JSON = jiffy:decode(Body, [return_maps]),
            {ok, maps:get(<<"email">>, JSON),
             maps:get(<<"session_key">>, JSON)};
        Code =:= 404 -> {error, invalid_session_id}
    end.

get_user_permissions_(Conn, Username) ->
    Query = "SELECT module,permission FROM user_permissions WHERE username=$1",
    {ok, _Cols, Rows} = epgsql:equery(Conn, Query, [Username]),
    Rows.

% TODO: allows for multiple inserts of same permission
add_user_permissions_(Conn, Username, Author, PermissionSet) ->
    Query = "INSERT INTO user_permissions(username, created_by, module,
             permission) VALUES ($1, $2, $3, $4)",
    Fn = fun({Mod, Perm}) ->
        epgsql:equery(Conn, Query, [Username, Author, Mod, Perm])
    end,
    % TODO: check for errors
    lists:foreach(Fn, PermissionSet),
    ok.

remove_user_permissions_(Conn, Username, Author, PermissionSet) ->
    Query = "UPDATE user_permissions
             SET deleted='t', deleted_by=$1, deleted_on=NOW()
             WHERE username=$2 AND module=$3 AND permission=$4
                   AND deleted='f'",
    Fn = fun({Mod, Perm}) ->
        epgsql:equery(Conn, Query, [Author, Username, Mod, Perm])
    end,
    % TODO: check for errors
    lists:foreach(Fn, PermissionSet),
    ok.
