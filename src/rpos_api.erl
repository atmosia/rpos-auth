-module(rpos_api).

-export([new/3, add_permission/4, remove_permission/4, delete/3]).
-export([permissions/2, gen_key/1]).

-spec new(epgsql:connection(), string(), string()) ->
    ok | {error, already_exists}.
new(Conn, Key, Owner) ->
    Query = "INSERT INTO apikey(key,created_by) VALUES($1, $2)",
    case epgsql:equery(Conn, Query, [Key, Owner]) of
        {ok, _Updated} -> ok;
        {error, {error, error, _ID, unique_violation, _Msg, _Args}} ->
            {error, already_exists}
    end.

%% TODO: supports multiple inserts
%% TODO: no security checking
-spec add_permission(epgsql:connection(), string(), string(),
                     {string(), string()}) ->
    ok | {error, no_key | permission_denied}.
add_permission(Conn, Key, Author, {Mod, Perm}) ->
    Query = "INSERT INTO apikey_permission(key, module, permission,
             created_by) VALUES ($1, $2, $3, $4)",
    case epgsql:equery(Conn, Query, [Key, Mod, Perm, Author]) of
        {ok, _Updated} -> ok;
        {error, {error, error, _ID, foreign_key_violation, _Msg,
                 [{constraint_name, <<"apikey_permission_key_fkey">>}|_]}} ->
            {error, no_key}
    end.

%% TODO: no security checking
-spec remove_permission(epgsql:connection(), string(), string(),
                        {string(), string()}) ->
    ok | {error, no_permission | permission_denied}.
remove_permission(Conn, Key, Author, {Mod, Perm}) ->
    Query = "UPDATE apikey_permission SET deleted='t', deleted_by=$2,
             deleted_on=NOW() WHERE key=$1 AND deleted='f' AND
             module=$3 AND permission=$4",
    case epgsql:equery(Conn, Query, [Key, Author, Mod, Perm]) of
        {ok, 0}  -> {error, no_permission};
        {ok, _N} -> ok
    end.

-spec delete(epgsql:connection(), string(), string()) ->
    ok | {error, no_key | permission_denied}.
delete(Conn, Key, Author) ->
    Query = "UPDATE apikey SET deleted='t', deleted_by=$2,
             deleted_on=NOW() WHERE key=$1",
    case epgsql:equery(Conn, Query, [Key, Author]) of
        {ok, 1} ->
            delete_all_permissions(Conn, Key, Author),
            ok;
        {ok, 0} -> {error, no_key}
    end.

-spec permissions(epgsql:connection(), string()) ->
    {ok, [string()]} | {error, no_key}.
permissions(Conn, Key) ->
    case key_exists(Conn, Key) of
        true  -> get_permissions(Conn, Key);
        false -> {error, no_key}
    end.

-spec gen_key(binary() | string()) -> binary().
gen_key(Seed) ->
    IOList = io_lib:format("~s~w", [Seed, erlang:monotonic_time()]),
    Hash = crypto:hash(sha256, IOList),
    Base64 = base64:encode(Hash),
    binary:replace(Base64, <<"/">>, <<"_">>, [global]).

key_exists(Conn, Key) ->
    Query = "SELECT COUNT(*) FROM apikey WHERE key=$1 AND deleted='f'",
    case epgsql:equery(Conn, Query, [Key]) of
        {ok, _Rows, [{1}]} -> true;
        _                  -> false
    end.

get_permissions(Conn, Key) ->
    Query = "SELECT module, permission FROM apikey_permission
             WHERE key=$1 AND deleted='f'",
    {ok, _Rows, Cols} = epgsql:equery(Conn, Query, [Key]),
    {ok, Cols}.

delete_all_permissions(Conn, Key, Author) ->
    Query = "UPDATE apikey_permission SET deleted='t', deleted_by=$2,
             deleted_on=NOW() WHERE key=$1 AND deleted='f'",
    {ok, _N} = epgsql:equery(Conn, Query, [Key, Author]).
