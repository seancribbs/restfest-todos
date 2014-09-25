-module(restfest_todos).
-include("todos.hrl").
-define(TAB, ?MODULE).

-export([
         open/0,
         insert/1,
         update/1,
         find/1,
         new_id/0
        ]).

open() ->
    dets:open_file(?TAB, [{keypos, #todo.id}]).

insert(T=#todo{}) ->
    dets:insert_new(?TAB, T).

update(T=#todo{id=ID}) ->
    case dets:lookup(?TAB, ID) of
        [_OtherTodo] -> dets:insert(?TAB, T);
        _ -> false
    end.

find(ID) ->
    case dets:lookup(?TAB, ID) of
        [Todo=#todo{}]-> {ok, Todo};
        _ -> false
    end.


new_id() ->
    mochihex:to_hex(crypto:hash(sha1, term_to_binary({erlang:now(), erlang:make_ref()}))).
