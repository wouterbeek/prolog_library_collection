:- module(
  dcg_word,
  [
    dcg_atom//2, % :Dcg
                 % ?Atom:atom
    dcg_string//2 % :Dcg
                  % ?String:string
  ]
).

/** <module> DCG word

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(dcg/dcg_call)).

:- meta_predicate(dcg_atom(3,?,?,?)).
:- meta_predicate(dcg_string(//,?,?,?)).





%! dcg_atom(:Dcg_1, ?Word:atom)// .
% This meta-DCG rule handles the translation
% between the word and the character level of parsing/generating.
%
% Typically, grammar *A* specifies how words can be formed out of characters.
% A character is a code, and a word is a list of codes.
% Grammar *B* specifies how sentences can be built out of words.
% Now the word is an atom, and the sentences in a list of atoms.
%
% This means that at some point,
% words in grammar *A*, i.e. lists of codes,
% need to be translated to words in grammar *B*, i.e. atoms.
%
% This is where dcg_atom//2 comes in.
% We illustrate this with a schematic example:
% ```prolog
% sentence([W1,...,Wn]) -->
%   word2(W1),
%   ...,
%   word2(Wn).
%
% word2(W) -->
%   dcg_atom(word1, W).
%
% word1([C1, ..., Cn]) -->
%   char(C1),
%   ...,
%   char(Cn).
% ```
%
% @throws instantiation_error
% @throws type_error

dcg_atom(Dcg_1, A) -->
  {var(A)}, !,
  dcg_call(Dcg_1, Cs),
  {atom_codes(A, Cs)}.
dcg_atom(Dcg_1, A) -->
  {must_be(atom, A)}, !,
  {atom_codes(A, Cs)},
  dcg_call(Dcg_1, Cs).



%! dcg_string(:Dcg_1, ?Word:string)// .
% @see Variants of dcg_atom//2 that supports SWI7 strings.

dcg_string(Dcg_1, S) -->
  {var(S)}, !,
  dcg_call(Dcg_1, Cs),
  {string_codes(S, Cs)}.
dcg_string(Dcg_1, S) -->
  {must_be(string, S)}, !,
  {string_codes(S, Cs)},
  dcg_call(Dcg_1, Cs).
