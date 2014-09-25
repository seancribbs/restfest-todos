-module(restfest_wm_todo_list).

-export([
         routes/0,
         init/1,
         to_html/2,
         resource_exists/2,
         encodings_provided/2
        ]).

-include("todos.hrl").
-include_lib("webmachine/include/webmachine.hrl").


routes() ->
    [{["todo"], ?MODULE, []}].

init(_) ->
    {{trace, "/tmp"}, []}.

resource_exists(RD, _Ctx) ->
    {true, RD, restfest_todos:all()}.

encodings_provided(RD, Ctx) ->
    {[{"identity", fun(X) -> X end},
      {"gzip", fun zlib:gzip/1},
      {"deflate", fun zlib:zip/1}], RD, Ctx}.

to_html(RD, Todos) ->
    {ok, Body} = todos_dtl:render(restfest_todos:to_dtl(Todos)),
    {Body, RD, Todos}.
