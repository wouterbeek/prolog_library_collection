:- module(
  varnames,
  [
    c_symbol//1,
    prolog_variable//1,
    prolog_atom//1,
    prolog_symbol//1
  ]
).

/** <module> varnames

DCG's that match variable names in various programming languages

@author Anne Ogborn
@version 2013/09

*/

c_symbol([H|T]) -->
  code_in(csymf, H),
  codes_in(csym, T).

prolog_variable([H|T]) -->
  code_in(prolog_var_start, H),
  codes_in(prolog_identifier_continue, T).

prolog_atom([H|T]) -->
  code_in(prolog_atom_start, H),
  codes_in(prolog_identifier_continue, T).
prolog_atom(X) -->
  prolog_symbol(X).

prolog_symbol([H|T]) -->
  code_in(prolog_prolog_symbol, H),
  codes_in(prolog_prolog_symbol, T).

code_in(Type, Code) -->
  [Code],
  {
    code_type(Code, Type)
  }.

codes_in(Type, [H|T]) -->
  code_in(Type, H),
  codes_in(Type, T).
codes_in(_, []) --> [].

