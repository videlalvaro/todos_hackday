-module(hackday_index).

-export([init/1]).
-export([allowed_methods/2, content_types_provided/2, to_html/2]).

-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-record(state, {root}).

init([]) ->
    Root = hackday_helpers:web_root(),
    {ok, #state{root=Root}}.

allowed_methods(ReqData, State) ->
    {['GET'], ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"text/html", to_html}], ReqData, State}.

to_html(ReqData, State) ->
    {ok, HBody} = file:read_file(filename:join([State#state.root, "index.html"])),
    {HBody, ReqData, State}.