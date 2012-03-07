-module(hackday_todos).

-export([init/1]).
-export([allowed_methods/2, content_types_provided/2, to_json/2]).
-export([post_is_create/2, create_path/2, delete_resource/2]).
-export([content_types_accepted/2, accept_content/2]).

-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-record(state, {root}).

init([]) ->
    Root = hackday_helpers:web_root(),
    {ok, #state{root=Root}}.

allowed_methods(ReqData, State) ->
    {['GET', 'POST', 'DELETE'], ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", accept_content}], ReqData, State}.

%% handle data insertion here
accept_content(ReqData, Context) ->
    Data = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    io:format("Data: ~p~n", [Data]),
    case proplists:get_value("title", Data) of
        undefined ->
            {false, ReqData, Context};
        Title ->
            case wrq:disp_path(ReqData) of
                [] ->
                    {false, ReqData, Context};
                Slug ->
                    IsDone = hackday_helpers:is_done_from_qs(proplists:get_value("isDone", Data, "false")),
                    io:format("IsDone: ~p~n", [IsDone]),
                    {ok, _Id} = todo_mysql:save_todo(Slug, Title, IsDone),
                    {true, ReqData, Context}
            end
    end.

%% retrieve data from MySQL here
to_json(ReqData, State) ->
    Result = todo_mysql:get_todos(),
    Struct = hackday_helpers:fromdb_to_json(Result),
    Json = mochijson2:encode({struct, [{todos, Struct}]}),
    {Json, ReqData, State}.

post_is_create(ReqData, State) ->
    {true, ReqData, State}.

%% after creating a todo we need to return the right id.
create_path(ReqData, State) ->
    {wrq:disp_path(ReqData), ReqData, State}.

delete_resource(ReqData, Context) ->
    case wrq:disp_path(ReqData) of
        [] ->
            {false, ReqData, Context};
        Slug ->
            case todo_mysql:delete_todo(Slug) of
                ok -> {true, ReqData, Context};
                _ -> {false, ReqData, Context}
            end
    end.