:- module(
  assoc_multi,
  [
    get_assoc/3, % ?Key
                 % +Assoc
                 % ?Value
    put_assoc/3, % +Key
                 % +AssocName:atom
                 % +Value
    put_assoc/4, % +Key
                 % +OldAssoc
                 % +Value
                 % ?NewAssoc
    register_assoc/1, % ?Assoc
    write_assoc/1, % +Assoc
    write_assoc/3 % +Out
                  % +Indent:integer
                  % +Assoc
  ]
).

/** <module> ASSOC MULTI

An association list with multiple values per keys, using ordered sets.

This extends library assoc by overloading get_assoc/3 and put_assoc/4,
and by adding ord_member/2.

@author Wouter Beek
@version 2013/04-2013/05
*/

:- reexport(library(assoc), except([get_assoc/3, put_assoc/4])).

:- use_module(generics(meta_ext)).
:- use_module(generics(print_ext)).
:- use_module(library(debug)).
:- use_module(library(ordsets)).

:- dynamic(current_assoc(_Name, _Assoc)).

:- debug(assoc_multi).



%! get_assoc(?Key, +Assoc, ?Value) is nondet.

get_assoc(Key, Assoc, Value):-
  assoc:get_assoc(Key, Assoc, Ordset),
  ord_member(Value, Ordset).

%! ord_member(?Member, ?List:list) is nondet.

ord_member(Value, Ordset):-
  nonvar(Value),
  !,
  ord_memberchk(Value, Ordset).
ord_member(Value, Ordset):-
  member(Value, Ordset).

%! put_assoc(+Key, +AssocName:atom, +Value) is det.

put_assoc(Key, AssocName, Value):-
  retract(current_assoc(AssocName, OldAssoc)),
  put_assoc(Key, OldAssoc, Value, NewAssoc),
  assert(current_assoc(AssocName, NewAssoc)).

%! put_assoc(+Key, +OldAssoc, +Value, ?NewAssoc) is semidet.

% Put the given value into the existing ordset.
put_assoc(Key, OldAssoc, Value, NewAssoc):-
  assoc:get_assoc(Key, OldAssoc, OldOrdset),
  !,
  ord_add_element(OldOrdset, Value, NewOrdset),
  assoc:put_assoc(Key, OldAssoc, NewOrdset, NewAssoc),
  length(NewOrdset, NewOrdsetLength), %DEB
  debug(
    assoc_multi,
    'Added <~w,~w> to existing assoc of length ~w.',
    [Key, Value, NewOrdsetLength]
  ).
% Create a new ordset.
put_assoc(Key, OldAssoc, Value, NewAssoc):-
  assoc:put_assoc(Key, OldAssoc, [Value], NewAssoc),
  debug(assoc_multi, 'Added <~w,~w> to NEW assoc.', [Key, Value]).

register_assoc(Name):-
  empty_assoc(EmptyAssoc),
  assert(current_assoc(Name, EmptyAssoc)).

write_assoc(Assoc):-
  write_assoc(user_output, 0, Assoc).

write_assoc(Out, KeyIndent, Assoc):-
  is_assoc(Assoc),
  !,
  assoc_to_keys(Assoc, Keys),
  ValueIndent is KeyIndent + 1,
  forall(
    member(Key, Keys),
    (
      print_indent(Out, KeyIndent),
      rdf_resource_naming(Key, KeyName),
      format(Out, '~w:\n', [KeyName]),
      forall(
        get_assoc(Key, Assoc, Value),
        (
          print_indent(Out, ValueIndent),
          rdf_resource_naming(Value, ValueName),
          format(Out, '~w\n', [ValueName])
        )
      )
    )
  ).
write_assoc(Out, Indent, AssocName):-
  current_assoc(AssocName, Assoc),
  write_assoc(Out, Indent, Assoc).

