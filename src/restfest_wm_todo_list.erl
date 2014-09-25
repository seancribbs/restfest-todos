-module(restfest_wm_todo_list).

-export([
         routes/0,
         init/1,
         to_html/2
        ]).

-include("todos.hrl").
-include_lib("webmachine/include/webmachine.hrl").


routes() ->
    [{["todo"], ?MODULE, []}].

init(_) ->
    {{trace, "/tmp"}, undefined}.

to_html(RD, Ctx) ->
    Todos = restfest_todos:all(),
    {ok, Body} = todos_dtl:render(restfest_todos:to_dtl(Todos)),
    {Body, RD, Ctx}.
