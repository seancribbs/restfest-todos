-module(restfest_wm_todo).

-record(ctx, {id, todo}).

-export([
         routes/0,
         init/1,
         resource_exists/2,
         to_html/2
        ]).

-include("todos.hrl").
-include_lib("webmachine/include/webmachine.hrl").

routes() ->
    [{["todo", id], ?MODULE, []}].

init(_) ->
    {ok, #ctx{}}.

resource_exists(RD, Ctx) ->
    Id = wrq:path_info(id, RD),
    case restfest_todos:find(Id) of
        {ok, Todo} ->
            {true, RD, Ctx#ctx{todo=Todo}};
        false ->
            {false, RD, Ctx}
    end.

to_html(RD, #ctx{todo=T}=Ctx) ->
    {ok, Body} = todo_dtl:render([{id, T#todo.id},
                            {title, T#todo.id},
                            {due, T#todo.dateDue},
                            {notes, T#todo.notes},
                            {created, T#todo.dateCreated},
                            {updated, T#todo.dateUpdated},
                            {complete, T#todo.complete}]),
    {ok, Layout} = layout_dtl:render([{content, Body}]),
    {Layout, RD, Ctx}.
