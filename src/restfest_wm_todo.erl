-module(restfest_wm_todo).

-record(ctx, {id, todo, url}).

-export([
         routes/0,
         init/1,
         resource_exists/2,
         to_html/2,

         %% Creating todos
         allowed_methods/2,
         allow_missing_post/2,
         post_is_create/2,
         accept_form/2,
         content_types_accepted/2,
         create_path/2
        ]).

-include("todos.hrl").
-include_lib("webmachine/include/webmachine.hrl").

routes() ->
    [{["todo"], fun is_create/1, ?MODULE, []},
     {["todo", id], ?MODULE, []}].

is_create(RD) ->
    wrq:method(RD) == 'POST'.

init(_) ->
    {{trace, "/tmp"}, #ctx{}}.

allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD', 'POST'], RD, Ctx}.

allow_missing_post(RD, Ctx) ->
    {true, RD, Ctx}.

post_is_create(RD, Ctx) ->
    {true, RD, Ctx}.

content_types_accepted(RD, Ctx) ->
    {[{"application/x-www-form-urlencoded", accept_form}], RD, Ctx}.

create_path(RD, Ctx) ->
    Id = restfest_todos:new_id(),
    Url = "/todo/"++ Id,
    {Url, RD, Ctx#ctx{id=Id, url=Url}}.

resource_exists(RD, Ctx) ->
    case wrq:path_info(id, RD) of
        undefined -> {false, RD, Ctx};
        Id ->
            case restfest_todos:find(Id) of
                {ok, Todo} ->
                    {true, RD, Ctx#ctx{todo=Todo}};
                false ->
                    {false, RD, Ctx}
            end
    end.

to_html(RD, #ctx{todo=T}=Ctx) ->
    {ok, Body} = todos_dtl:render([{items, [[{id, T#todo.id},
                                  {title, T#todo.title},
                                  {due, T#todo.dateDue},
                                  {notes, T#todo.notes},
                                  {created, T#todo.dateCreated},
                                  {updated, T#todo.dateUpdated},
                                  {complete, T#todo.complete}]]}]),
    {ok, Layout} = layout_dtl:render([{content, Body}]),
    {Layout, RD, Ctx}.

accept_form(RD, Ctx) ->
    PostData = mochiweb_util:parse_qs(wrq:req_body(RD)),
    Fields = [ Pair || Pair={_K, V} <- PostData, V /= "" ],
    Todo = #todo{id = proplists:get_value("todoid", Fields, Ctx#ctx.id),
                 title = proplists:get_value("todoTitle", Fields),
                 dateDue = coerce_date(proplists:get_value("todoDateDue", Fields, undefined)),
                 notes = proplists:get_value("todoNotes", Fields, "")
                },
    restfest_todos:insert(Todo),
    {Body, RD1, Ctx1} = to_html(RD, Ctx#ctx{todo=Todo}),
    {true, wrq:set_resp_body(Body, RD1), Ctx1}.

coerce_date(undefined) -> undefined;
coerce_date(DateStr) when is_list(DateStr) -> 
    element(1, qdate:to_date(DateStr)).
