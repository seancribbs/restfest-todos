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
    Body = todo_dtl:render([{id, T#todo.id},
                            {title, T#todo.id},
                            {due, format_date(T#todo.dateDue)},
                            {notes, T#todo.notes},
                            {created, format_date(T#todo.dateCreated)},
                            {updated, format_date(T#todo.dateUpdated)},
                            {complete, T#todo.complete}]),
    Layout = layout_dtl:render([{content, Body}]),
    {Layout, RD, Ctx}.

format_date(Date={_Y,_M,_D}) -> qdate:to_string("%Y-%m-%d", Date);
format_date(_) -> "".

