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
	Todos = dets:match_object(restfest_todos, '_'),
    {ok, Body} = todos_dtl:render([{items, [[{id, T#todo.id},
                                  {title, T#todo.title},
                                  {due, T#todo.dateDue},
                                  {notes, T#todo.notes},
                                  {created, T#todo.dateCreated},
                                  {updated, T#todo.dateUpdated},
                                  {complete, T#todo.complete}] || T <- Todos]}]),
    {ok, Layout} = layout_dtl:render([{content, Body}]),
    {Layout, RD, Ctx}.