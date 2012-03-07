-module(hackday_helpers).

-export([web_root/0, fromdb_to_json/1, is_done_from_qs/1]).

web_root() ->
    filename:join(code:priv_dir(hackday), "www").

fromdb_to_json(Data) ->
    fromdb_to_json(Data, []).

fromdb_to_json([], Acc) ->
    lists:reverse(Acc);
fromdb_to_json([[Slug, Title, IsDone]|T], Acc) ->
    Item = [{slug, Slug}, {title, Title}, {isDone, get_is_done(IsDone)}],
    fromdb_to_json(T, [Item|Acc]).

get_is_done(0) -> false;
get_is_done(1) -> true.

is_done_from_qs("true")  -> 1;
is_done_from_qs("false") -> 0.