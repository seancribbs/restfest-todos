-module(restfest_todos).
-include("todos.hrl").
-define(TAB, ?MODULE).

-export([
         open/0,
         all/0,
         insert/1,
         update/1,
         find/1,
         new_id/0,
         to_dtl/1
        ]).

open() ->
    dets:open_file(?TAB, [{keypos, #todo.id}]).

all() ->
    dets:match_object(restfest_todos, '_').

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

to_dtl(T=#todo{}) ->
    to_dtl([T]);
to_dtl(Todos) when is_list(Todos) ->
    [{items, [[{id, T#todo.id},
               {title, T#todo.title},
               {due, T#todo.dateDue},
               {notes, T#todo.notes},
               {created, T#todo.dateCreated},
               {updated, T#todo.dateUpdated},
               {complete, T#todo.complete}] || T <- Todos]}].

new_id() ->
    mochihex:to_hex(crypto:hash(sha, term_to_binary({erlang:now(), erlang:make_ref()}))).
