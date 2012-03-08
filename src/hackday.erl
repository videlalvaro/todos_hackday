%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc hackday startup code

-module(hackday).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    hackday_sup:start_link().

%% @spec start() -> ok
%% @doc Start the hackday server.
start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    ensure_started(emysql),
    Res = application:start(hackday),
    connect_mysql(),
    Res.

%% @spec stop() -> ok
%% @doc Stop the hackday server.
stop() ->
    Res = application:stop(hackday),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    close_mysql(),
    Res.

connect_mysql() ->
    emysql:add_pool(todo_pool, 1,
        get_env(mysql_user),
        get_env(mysql_pass),
        get_env(mysql_host),
        get_env(mysql_port),
        get_env(mysql_database),
        utf8).

close_mysql() ->
    emysql:remove_pool(todo_pool).

get_env(Key) ->
    {ok, Val} = application:get_env(hackday, Key),
    Val.