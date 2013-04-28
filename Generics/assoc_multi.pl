:- module(
  assoc_multi,
  [
    get_assoc/3, % ?Key
                 % +Assoc
                 % ?Value
    put_assoc/4 % +Key
                % +OldAssoc
                % +Value
                % ?NewAssoc
  ]
).

/** <module> ASSOC MULTI

An association list with multiple values per keys, using ordered sets.

@author Wouter Beek
@version 2013/04
*/

:- reexport(library(assoc), except([get_assoc/3, put_assoc/4])).
:- use_module(library(ordsets)).



%% get_assoc(?Key, +Assoc, ?Value) is nondet.

get_assoc(Key, Assoc, Value):-
  assoc:get_assoc(Key, Assoc, Ordset),
  ord_member(Value, Ordset).

%% ord_member(?Member, ?List:list) is nondet.

ord_member(Value, Ordset):-
  nonvar(Value),
  !,
  ord_memberchk(Value, Ordset).
ord_member(Value, Ordset):-
  member(Value, Ordset).

%% put_assoc(+Key, +OldAssoc, +Value, ?NewAssoc) is semidet.

put_assoc(Key, OldAssoc, Value, NewAssoc):-
  assoc:get_assoc(Key, OldAssoc, OldOrdset),
  !,
  ord_add_element(OldOrdset, Value, NewOrdset),
  assoc:put_assoc(Key, OldAssoc, NewOrdset, NewAssoc).
put_assoc(Key, OldAssoc, Value, NewAssoc):-
  assoc:put_assoc(Key, OldAssoc, [Value], NewAssoc).
