-module(todo_mysql).

-export([get_todos/0, save_todo/3, delete_todo/1]).

get_todos() ->
    {result_packet, _, _, Result, _} = emysql:execute(todo_pool, <<"SELECT * from todo">>),
    Result.

save_todo(Slug, Title, IsDone) ->
    emysql:prepare(add_todo_stmt, <<"INSERT INTO todo (slug, title, isDone) VALUES(?, ?, 0) ON DUPLICATE KEY UPDATE isDone = ?">>),
    emysql:execute(todo_pool, add_todo_stmt, [Slug, Title, IsDone]),
    [[LastInsertId]] = get_last_insert_id(),
    {ok, LastInsertId}.

get_last_insert_id() ->
    {result_packet, _, _, Result, _} = emysql:execute(todo_pool, <<"SELECT LAST_INSERT_ID();">>),
    Result.

delete_todo(Slug) ->
    emysql:prepare(del_todo_stmt, <<"DELETE FROM todo WHERE slug = ?">>),
    Result = emysql:execute(todo_pool, del_todo_stmt, [Slug]),
    io:format("Result: ~p~n", [Result]),
    ok.