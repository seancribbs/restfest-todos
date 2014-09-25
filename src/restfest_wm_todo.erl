-module(restfest_wm_todo).

-record(ctx, {id, todo, url, mode}).

-export([
         routes/0,
         init/1,
         resource_exists/2,
         
         %% Reading todos
         to_html/2,
         encodings_provided/2,
         generate_etag/2,

         %% Creating todos
         allowed_methods/2,
         allow_missing_post/2,
         post_is_create/2,
         accept_form/2,
         content_types_accepted/2,
         create_path/2,

         %% Update todo
         process_post/2
        ]).

-include("todos.hrl").
-include_lib("webmachine/include/webmachine.hrl").

routes() ->
    [{["todo"], fun is_create/1, ?MODULE, create},
     {["todo", id], ?MODULE, normal}].

is_create(RD) ->
    wrq:method(RD) == 'POST'.

init(Mode) ->
    Ctx = #ctx{mode=Mode},
    case application:get_env(restfest, tracing) of
        {ok, Path} when is_list(Path) ->
            {{trace, Path}, Ctx};
        _ ->
            {ok, Ctx}
    end.

encodings_provided(RD, Ctx) ->
    {[{"identity", fun(X) -> X end},
      {"gzip", fun zlib:gzip/1},
      {"deflate", fun zlib:zip/1}], RD, Ctx}.

allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD', 'POST'], RD, Ctx}.

allow_missing_post(RD, Ctx) ->
    {Ctx#ctx.mode == create, RD, Ctx}.

post_is_create(RD, Ctx) ->
    {Ctx#ctx.mode == create, RD, Ctx}.

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
                    {true, RD, Ctx#ctx{id=Id, todo=Todo}};
                false ->
                    {false, RD, Ctx}
            end
    end.

generate_etag(RD, Ctx=#ctx{todo=#todo{}=Todo}) ->
    {mochihex:to_hex(crypto:hash(sha, term_to_binary(Todo))), RD, Ctx}.

to_html(RD, #ctx{todo=T}=Ctx) ->
    {ok, Body} = todos_dtl:render(restfest_todos:to_dtl(T)),
    {Body, RD, Ctx}.

accept_form(RD, Ctx) ->
    Todo = extract_post(RD, Ctx, #todo{}),
    restfest_todos:insert(Todo),
    {Body, RD1, Ctx1} = to_html(RD, Ctx#ctx{todo=Todo}),
    {true, wrq:set_resp_body(Body, RD1), Ctx1}.

process_post(RD, Ctx=#ctx{todo=T}) ->
    Todo = extract_post(RD, Ctx, T),
    restfest_todos:update(Todo),
    {Body, RD1, Ctx1} = to_html(RD, Ctx#ctx{todo=Todo}),
    {true, wrq:set_resp_body(Body, RD1), Ctx1}.

coerce_date(undefined) -> undefined;
coerce_date(Tuple) when is_tuple(Tuple) -> Tuple;
coerce_date(DateStr) when is_list(DateStr) ->
    case element(1, qdate:to_date(DateStr)) of
        {0,0,0} -> undefined;
        Tuple -> Tuple
    end.

extract_post(RD, Ctx, Todo) ->
    PostData = mochiweb_util:parse_qs(wrq:req_body(RD)),
    Fields = [ Pair || Pair={_K, V} <- PostData, V /= "" ],
    Todo#todo{id = proplists:get_value("todoid", Fields, Ctx#ctx.id),
              title = proplists:get_value("todoTitle", Fields, Todo#todo.title),
              dateDue = coerce_date(proplists:get_value("todoDateDue", Fields, Todo#todo.dateDue)),
              notes = proplists:get_value("todoNotes", Fields, Todo#todo.notes),
              complete = to_boolean(proplists:get_value("todoComplete", Fields, Todo#todo.complete)),
              dateUpdated = element(1, calendar:local_time())
             }.

to_boolean(B) when is_boolean(B) -> B;
to_boolean("false") -> false;
to_boolean("true") ->  true.
