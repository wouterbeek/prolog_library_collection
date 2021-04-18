:- module(
  type_ext,
  [
    boolean/1,         % @Term
    positive_integer/1 % @Term
  ]
).

/** <module> Extended support for types

Extends support for types in the SWI-Prolog standard library.

This module introduces the following types:

  - `options`: A dictionary with tag `options` that is used to
               represent options passed to predicates.

*/

:- use_module(library(error)).
:- use_module(library(lists)).

:- use_module(library(dict)).

:- multifile
    error:has_type/2.

error:has_type(maybe(Type), Term) :-
  ( error:has_type(Type, Term)
  ; error:has_type(var, Term)
  ).
error:has_type(options, Term) :-
  error:has_type(dict, Term),
  dict_key(Term, options).
error:has_type(or(Types), Term) :-
  member(Type, Types),
  error:has_type(Type, Term), !.



%! boolean(@Term) is semidet.

boolean(false).
boolean(true).



%! positive_integer(@Term) is semidet.

positive_integer(Term) :-
  error:has_type(positive_integer, Term).
