:- encoding(utf8).
:- module(
  dcg,
  [
    atom_ci//1,             % ?Atom
    dcg_debug/2,            % +Flag, :Dcg_0
    dcg_default//3,         % :Dcg_0, -Arg1, +Default
    dcg_integer//2,         % :Dcg_1, ?Integer
    dcg_integer//3,         % :Dcg_1, +Base, ?Integer
    dcg_tab//0,
    dcg_tab//1,             % +N
    thousands//1            % +Integer
  ]
).
:- reexport(library(dcg/basics)).
:- reexport(library(dcg_abnf)).

/** <module> DCG extensions

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(aggregate)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(lists)).

:- use_module(library(atom_ext)).
:- use_module(library(code_ext)).
:- use_module(library(math_ext)).

:- meta_predicate
    dcg_atom(3, ?, ?, ?),
    dcg_debug(+, //),
    dcg_default(3, -, +, ?, ?),
    dcg_integer(3, ?, ?, ?),
    dcg_integer(3, +, ?, ?, ?),
    dcg_string(3, ?, ?, ?).





%! atom_ci(?Atom)// .
%
% ```prolog
% ?- phrase(atom_ci(http), Codes).
% Codes = "HTTP" ;
% Codes = "HTTp" ;
% Codes = "HTtP" ;
% Codes = "HTtp" ;
% Codes = "HtTP" ;
% Codes = "HtTp" ;
% Codes = "HttP" ;
% Codes = "Http" ;
% Codes = "hTTP" ;
% Codes = "hTTp" ;
% Codes = "hTtP" ;
% Codes = "hTtp" ;
% Codes = "htTP" ;
% Codes = "htTp" ;
% Codes = "httP" ;
% Codes = "http" ;
% false.
% ```

atom_ci(Atom) -->
  dcg_atom(*(code_ci), Atom).



%! code_ci(+Code:code, -CiCode:code) is nondet.
%! code_ci(+Code:code)// .
%
% Returns case-insensitive variants of the given code.
% This includes the code itself.

% Lowercase is a case-insensitive variant of uppercase.
code_ci(Upper, Lower) :-
  code_type(Upper, upper(Lower)).
% Uppercase is a case-insensitive variant of lowercase.
code_ci(Lower, Upper) :-
  code_type(Lower, lower(Upper)).
% Every code is a case-insensitive variant of itself.
code_ci(Code, Code).


code_ci(Code) -->
  {code_ci(Code, CiCode)},
  [CiCode].



%! dcg_debug(+Flag, :Dcg_0) is det.
%
% Write the first generation of Dcg_0 as a debug message under the
% given Flag.

dcg_debug(Flag, Dcg_0) :-
  debugging(Flag), !,
  phrase(Dcg_0, Codes),
  debug(Flag, "~s", [Codes]).
dcg_debug(_, _).



%! dcg_default(:Dcg_1, -Arg, +Def)// .

dcg_default(Dcg_1, Arg, _) -->
  dcg_call(Dcg_1, Arg), !.
dcg_default(_, Default, Default) --> "".



%! dcg_integer(:Dcg_1, ?Integer)// .
%! dcg_integer(:Dcg_1, +Base:nonneg, ?Integer)// .

dcg_integer(Dcg_1, N) -->
  dcg_integer(Dcg_1, 10, N).


dcg_integer(Dcg_1, Base, N) -->
  parsing, !,
  dcg_call(Dcg_1, Weights),
  {integer_weights(N, Base, Weights)}.
dcg_integer(Dcg_1, Base, N) -->
  {integer_weights(N, Base, Weights)},
  dcg_call(Dcg_1, Weights).



%! dcg_tab// is det.
%! dcg_tab(+N:nonneg)// is det.

dcg_tab -->
  "\t".


dcg_tab(N) -->
  dcg_once(#(N, dcg_tab)).



%! thousands(+I)// is det.

thousands(∞) --> !,
  "∞".
thousands(I) -->
  {format(atom(A), "~D", [I])},
  atom(A).
