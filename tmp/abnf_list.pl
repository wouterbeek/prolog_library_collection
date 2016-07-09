:- module(
  abnf_list,
  [
    '##'//3, '##'//4, '##'//5, '##'//6, '##'//7, '##'//8,
    '**'//2, '**'//3, '**'//4, '**'//5, '**'//6, '**'//7,
    '**n'//3, '**n'//4, '**n'//5, '**n'//6, '**n'//7, '**n'//8,
    '++'//2, '++'//3, '++'//4, '++'//5, '++'//6, '++'//7,
    '++n'//3, '++n'//4, '++n'//5, '++n'//6, '++n'//7, '++n'//8,
    'm**'//3, 'm**'//4, 'm**'//5, 'm**'//6, 'm**'//7, 'm**'//8,
    'm**n'//4, 'm**n'//5, 'm**n'//6, 'm**n'//7, 'm**n'//8, 'm**n'//9
  ]
).

/** <module> ABNF list extension

DCG implementation of the ABNF list extension
used in the HTTP 1.1 specification.

@author Wouter Beek
@compat RFC 7230
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(option)).
:- use_module(library(http/http11)).

:- meta_predicate('##'(?,//,:,?,?)).
:- meta_predicate('##'(?,3,?,:,?,?)).
:- meta_predicate('##'(?,4,?,?,:,?,?)).
:- meta_predicate('##'(?,5,?,?,?,:,?,?)).
:- meta_predicate('##'(?,6,?,?,?,?,:,?,?)).
:- meta_predicate('##'(?,7,?,?,?,?,?,:,?,?)).
:- meta_predicate('**'(//,:,?,?)).
:- meta_predicate('**'(3,?,:,?,?)).
:- meta_predicate('**'(4,?,?,:,?,?)).
:- meta_predicate('**'(5,?,?,?,:,?,?)).
:- meta_predicate('**'(6,?,?,?,?,:,?,?)).
:- meta_predicate('**'(7,?,?,?,?,?,:,?,?)).
:- meta_predicate('**n'(?,//,:,?,?)).
:- meta_predicate('**n'(?,3,?,:,?,?)).
:- meta_predicate('**n'(?,4,?,?,:,?,?)).
:- meta_predicate('**n'(?,5,?,?,?,:,?,?)).
:- meta_predicate('**n'(?,6,?,?,?,?,:,?,?)).
:- meta_predicate('**n'(?,7,?,?,?,?,?,:,?,?)).
:- meta_predicate('++'(//,:,?,?)).
:- meta_predicate('++'(3,?,:,?,?)).
:- meta_predicate('++'(4,?,?,:,?,?)).
:- meta_predicate('++'(5,?,?,?,:,?,?)).
:- meta_predicate('++'(6,?,?,?,?,:,?,?)).
:- meta_predicate('++'(7,?,?,?,?,?,:,?,?)).
:- meta_predicate('++n'(?,//,:,?,?)).
:- meta_predicate('++n'(?,3,?,:,?,?)).
:- meta_predicate('++n'(?,4,?,?,:,?,?)).
:- meta_predicate('++n'(?,5,?,?,?,:,?,?)).
:- meta_predicate('++n'(?,6,?,?,?,?,:,?,?)).
:- meta_predicate('++n'(?,7,?,?,?,?,?,:,?,?)).
:- meta_predicate('m**'(?,//,:,?,?)).
:- meta_predicate('m**'(?,3,?,:,?,?)).
:- meta_predicate('m**'(?,4,?,?,:,?,?)).
:- meta_predicate('m**'(?,5,?,?,?,:,?,?)).
:- meta_predicate('m**'(?,6,?,?,?,?,:,?,?)).
:- meta_predicate('m**'(?,7,?,?,?,?,?,:,?,?)).
:- meta_predicate('m**n'(?,?,//,:,?,?)).
:- meta_predicate('m**n'(?,?,3,?,:,?,?)).
:- meta_predicate('m**n'(?,?,4,?,?,:,?,?)).
:- meta_predicate('m**n'(?,?,5,?,?,?,:,?,?)).
:- meta_predicate('m**n'(?,?,6,?,?,?,?,:,?,?)).
:- meta_predicate('m**n'(?,?,7,?,?,?,?,?,:,?,?)).

:- predicate_options('##'//3, 3, [
     pass_to('m**n'//4, 4)
   ]).
:- predicate_options('##'//4, 4, [
     pass_to('m**n'//5, 5)
   ]).
:- predicate_options('##'//5, 5, [
     pass_to('m**n'//6, 6)
   ]).
:- predicate_options('##'//6, 6, [
     pass_to('m**n'//7, 7)
   ]).
:- predicate_options('##'//7, 7, [
     pass_to('m**n'//8, 8)
   ]).
:- predicate_options('##'//8, 8, [
     pass_to('m**n'//9, 9)
   ]).

:- predicate_options('**'//2, 2, [
     pass_to('m**n'//4, 4)
   ]).
:- predicate_options('**'//3, 3, [
     pass_to('m**n'//5, 5)
   ]).
:- predicate_options('**'//4, 4, [
     pass_to('m**n'//6, 6)
   ]).
:- predicate_options('**'//5, 5, [
     pass_to('m**n'//7, 7)
   ]).
:- predicate_options('**'//6, 6, [
     pass_to('m**n'//8, 8)
   ]).
:- predicate_options('**'//7, 7, [
     pass_to('m**n'//9, 9)
   ]).

:- predicate_options('**n'//3, 3, [
     pass_to('m**n'//4, 4)
   ]).
:- predicate_options('**n'//4, 4, [
     pass_to('m**n'//5, 5)
   ]).
:- predicate_options('**n'//5, 5, [
     pass_to('m**n'//6, 6)
   ]).
:- predicate_options('**n'//6, 6, [
     pass_to('m**n'//7, 7)
   ]).
:- predicate_options('**n'//7, 7, [
     pass_to('m**n'//8, 8)
   ]).
:- predicate_options('**n'//8, 8, [
     pass_to('m**n'//9, 9)
   ]).

:- predicate_options('++'//2, 2, [
     pass_to('m**n'//4, 4)
   ]).
:- predicate_options('++'//3, 3, [
     pass_to('m**n'//5, 5)
   ]).
:- predicate_options('++'//4, 4, [
     pass_to('m**n'//6, 6)
   ]).
:- predicate_options('++'//5, 5, [
     pass_to('m**n'//7, 7)
   ]).
:- predicate_options('++'//6, 6, [
     pass_to('m**n'//8, 8)
   ]).
:- predicate_options('++'//7, 7, [
     pass_to('m**n'//9, 9)
   ]).

:- predicate_options('++n'//3, 3, [
     pass_to('m**n'//4, 4)
   ]).
:- predicate_options('++n'//4, 4, [
     pass_to('m**n'//5, 5)
   ]).
:- predicate_options('++n'//5, 5, [
     pass_to('m**n'//6, 6)
   ]).
:- predicate_options('++n'//6, 6, [
     pass_to('m**n'//7, 7)
   ]).
:- predicate_options('++n'//7, 7, [
     pass_to('m**n'//8, 8)
   ]).
:- predicate_options('++n'//8, 8, [
     pass_to('m**n'//9, 9)
   ]).

:- predicate_options('m**'//3, 3, [
     pass_to('m**n'//4, 4)
   ]).
:- predicate_options('m**'//4, 4, [
     pass_to('m**n'//5, 5)
   ]).
:- predicate_options('m**'//5, 5, [
     pass_to('m**n'//6, 6)
   ]).
:- predicate_options('m**'//6, 6, [
     pass_to('m**n'//7, 7)
   ]).
:- predicate_options('m**'//7, 7, [
     pass_to('m**n'//8, 8)
   ]).
:- predicate_options('m**'//8, 8, [
     pass_to('m**n'//9, 9)
   ]).

:- predicate_options('m**n'//4, 4, [
     pass_to('m*n'//4, 4)
   ]).
:- predicate_options('m**n'//5, 5, [
     pass_to('m*n'//5, 5)
   ]).
:- predicate_options('m**n'//6, 6, [
     pass_to('m*n'//6, 6)
   ]).
:- predicate_options('m**n'//7, 7, [
     pass_to('m*n'//7, 7)
   ]).
:- predicate_options('m**n'//8, 8, [
     pass_to('m*n'//8, 8)
   ]).
:- predicate_options('m**n'//9, 9, [
     pass_to('m*n'//9, 9)
   ]).





%! '##'(?N:nonneg, :Dcg_0, :Options:list(compound))// .
%! '##'(?N:nonneg, :Dcg_1, ?Args1:list, :Options:list(compound))// .
%! '##'(?N:nonneg, :Dcg_2, ?Args1:list, ?Args2:list, :Options:list(compound))// .
%! '##'(?N:nonneg, :Dcg_3, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(compound))// .
%! '##'(?N:nonneg, :Dcg_4, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(compound))// .
%! '##'(?N:nonneg, :Dcg_5, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(compound))// .
%
% ### Module prefix
%
% Normally meta_options/3 must appear before other option predicates
%  (here: merge_options/3).
% In this module meta_options/3 occurs in 'm*n'//[4-9],
%  and must occur there because these are public predicates as well.
% Inserting meta_options/3 for each of the predicates '#'//[3-8] would
%  make the code slightly longer than needed and would perform the same
%  operation twice.
% This is why the module prefix `Mod` is explicitly carried over here.
%
% @see Wrappers around 'm**n'//[3-8] using `M =:= N`.

'##'(N, Dcg_0, Mod:Opts1) -->
  {merge_options([count(N)], Opts1, Opts2)},
  'm**n'(N, N, Dcg_0, Mod:Opts2).

'##'(N, Dcg_1, L1, Mod:Opts1) -->
  {merge_options([count(N)], Opts1, Opts2)},
  'm**n'(N, N, Dcg_1, L1, Mod:Opts2).

'##'(N, Dcg_2, L1, L2, Mod:Opts1) -->
  {merge_options([count(N)], Opts1, Opts2)},
  'm**n'(N, N, Dcg_2, L1, L2, Mod:Opts2).

'##'(N, Dcg_3, L1, L2, L3, Mod:Opts1) -->
  {merge_options([count(N)], Opts1, Opts2)},
  'm**n'(N, N, Dcg_3, L1, L2, L3, Mod:Opts2).

'##'(N, Dcg_4, L1, L2, L3, L4, Mod:Opts1) -->
  {merge_options([count(N)], Opts1, Opts2)},
  'm**n'(N, N, Dcg_4, L1, L2, L3, L4, Mod:Opts2).

'##'(N, Dcg_5, L1, L2, L3, L4, L5, Mod:Opts1) -->
  {merge_options([count(N)], Opts1, Opts2)},
  'm**n'(N, N, Dcg_5, L1, L2, L3, L4, L5, Mod:Opts2).



%! '**'(:Dcg_0, :Options:list(compound))// .
%! '**'(:Dcg_1, ?Args1:list, :Options:list(compound))// .
%! '**'(:Dcg_2, ?Args1:list, ?Args3:list, :Options:list(compound))// .
%! '**'(:Dcg_3, ?Args1:list, ?Args3:list, ?Args3:list, :Options:list(compound))// .
%! '**'(:Dcg_4, ?Args1:list, ?Args3:list, ?Args3:list, ?Args4:list, :Options:list(compound))// .
%! '**'(:Dcg_5, ?Args1:list, ?Args3:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(compound))// .
% Implements the Regular Expression operator `*` in a nondeterministic way.
%
% @see Wrapper around 'm**n'//[3-8] with `M = 0` and `N` uninstantiated.

'**'(Dcg_0, Opts) -->
  'm**n'(_, _, Dcg_0, Opts).

'**'(Dcg_1, L1, Opts) -->
  'm**n'(_, _, Dcg_1, L1, Opts).

'**'(Dcg_2, L1, L2, Opts) -->
  'm**n'(_, _, Dcg_2, L1, L2, Opts).

'**'(Dcg_3, L1, L2, L3, Opts) -->
  'm**n'(_, _, Dcg_3, L1, L2, L3, Opts).

'**'(Dcg_4, L1, L2, L3, L4, Opts) -->
  'm**n'(_, _, Dcg_4, L1, L2, L3, L4, Opts).

'**'(Dcg_5, L1, L2, L3, L4, L5, Opts) -->
  'm**n'(_, _, Dcg_5, L1, L2, L3, L4, L5, Opts).



%! '**n'(?N:nonneg, :Dcg_0, :Options:list(compound))// .
%! '**n'(?N:nonneg, :Dcg_1, ?Args1:list, :Options:list(compound))// .
%! '**n'(?N:nonneg, :Dcg_2, ?Args1:list, ?Args2:list, :Options:list(compound))// .
%! '**n'(?N:nonneg, :Dcg_3, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(compound))// .
%! '**n'(?N:nonneg, :Dcg_4, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(compound))// .
%! '**n'(?N:nonneg, :Dcg_5, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(compound))// .
% @see Wrappers around 'm**n'//[3-8] with `M = 0` and given `N`.

'**n'(N, Dcg_0, Opts) -->
  'm**n'(_, N, Dcg_0, Opts).

'**n'(N, Dcg_1, L1, Opts) -->
  'm**n'(_, N, Dcg_1, L1, Opts).

'**n'(N, Dcg_2, L1, L2, Opts) -->
  'm**n'(_, N, Dcg_2, L1, L2, Opts).

'**n'(N, Dcg_3, L1, L2, L3, Opts) -->
  'm**n'(_, N, Dcg_3, L1, L2, L3, Opts).

'**n'(N, Dcg_4, L1, L2, L3, L4, Opts) -->
  'm**n'(_, N, Dcg_4, L1, L2, L3, L4, Opts).

'**n'(N, Dcg_5, L1, L2, L3, L4, L5, Opts) -->
  'm**n'(_, N, Dcg_5, L1, L2, L3, L4, L5, Opts).



%! '++'(:Dcg_0, +Options:list(compound))// .
%! '++'(:Dcg_1, ?Args1:list, :Options:list(compound))// .
%! '++'(:Dcg_2, ?Args1:list, ?Args2:list, :Options:list(compound))// .
%! '++'(:Dcg_3, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(compound))// .
%! '++'(:Dcg_4, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(compound))// .
%! '++'(:Dcg_5, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(compound))// .
% @see Wrappers around 'm**n'//[3-8] with `M = 1` and unbound `N`.

'++'(Dcg_0, Opts) -->
  'm**n'(1, _, Dcg_0, Opts).

'++'(Dcg_1, L1, Opts) -->
  'm**n'(1, _, Dcg_1, L1, Opts).

'++'(Dcg_2, L1, L2, Opts) -->
  'm**n'(1, _, Dcg_2, L1, L2, Opts).

'++'(Dcg_3, L1, L2, L3, Opts) -->
  'm**n'(1, _, Dcg_3, L1, L2, L3, Opts).

'++'(Dcg_4, L1, L2, L3, L4, Opts) -->
  'm**n'(1, _, Dcg_4, L1, L2, L3, L4, Opts).

'++'(Dcg_5, L1, L2, L3, L4, L5, Opts) -->
  'm**n'(1, _, Dcg_5, L1, L2, L3, L4, L5, Opts).



%! '++n'(?N:nonneg, :Dcg_0, +Options:list(compound))// .
%! '++n'(?N:nonneg, :Dcg_1, ?Args1:list, :Options:list(compound))// .
%! '++n'(?N:nonneg, :Dcg_2, ?Args1:list, ?Args2:list, :Options:list(compound))// .
%! '++n'(?N:nonneg, :Dcg_3, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(compound))// .
%! '++n'(?N:nonneg, :Dcg_4, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(compound))// .
%! '++n'(?N:nonneg, :Dcg_5, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(compound))// .
% @see Wrappers around 'm**n'//[3-8] with `M = 1` and given `N`.

'++n'(N, Dcg_0, Opts) -->
  'm**n'(1, N, Dcg_0, Opts).

'++n'(N, Dcg_1, L1, Opts) -->
  'm**n'(1, N, Dcg_1, L1, Opts).

'++n'(N, Dcg_2, L1, L2, Opts) -->
  'm**n'(1, N, Dcg_2, L1, L2, Opts).

'++n'(N, Dcg_3, L1, L2, L3, Opts) -->
  'm**n'(1, N, Dcg_3, L1, L2, L3, Opts).

'++n'(N, Dcg_4, L1, L2, L3, L4, Opts) -->
  'm**n'(1, N, Dcg_4, L1, L2, L3, L4, Opts).

'++n'(N, Dcg_5, L1, L2, L3, L4, L5, Opts) -->
  'm**n'(1, N, Dcg_5, L1, L2, L3, L4, L5, Opts).



%! 'm**'(?M:nonneg, :Dcg_0, :Options:list(compound))// .
%! 'm**'(?M:nonneg, :Dcg_1, ?Args1:list, :Options:list(compound))// .
%! 'm**'(?M:nonneg, :Dcg_2, ?Args1:list, ?Args2:list, :Options:list(compound))// .
%! 'm**'(?M:nonneg, :Dcg_3, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(compound))// .
%! 'm**'(?M:nonneg, :Dcg_4, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(compound))// .
%! 'm**'(?M:nonneg, :Dcg_5, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(compound))// .
% @see Wrapper around 'm**n'//[3-8] with given `M` and unbounded `N`.

'm**'(M, Dcg_0, Opts) -->
  'm**n'(M, _, Dcg_0, Opts).

'm**'(M, Dcg_1, L1, Opts) -->
  'm**n'(M, _, Dcg_1, L1, Opts).

'm**'(M, Dcg_2, L1, L2, Opts) -->
  'm**n'(M, _, Dcg_2, L1, L2, Opts).

'm**'(M, Dcg_3, L1, L2, L3, Opts) -->
  'm**n'(M, _, Dcg_3, L1, L2, L3, Opts).

'm**'(M, Dcg_4, L1, L2, L3, L4, Opts) -->
  'm**n'(M, _, Dcg_4, L1, L2, L3, L4, Opts).

'm**'(M, Dcg_5, L1, L2, L3, L4, L5, Opts) -->
  'm**n'(M, _, Dcg_5, L1, L2, L3, L4, L5, Opts).



%! 'm**n'(?M:nonneg, ?N:nonneg, :Dcg_0, +Options:list(compound))// .
%! 'm**n'(?M:nonneg, ?N:nonneg, :Dcg_1, ?Args1:list, +Options:list(compound))// .
%! 'm**n'(?M:nonneg, ?N:nonneg, :Dcg_2, ?Args1:list, ?Args2:list, +Options:list(compound))// .
%! 'm**n'(?M:nonneg, ?N:nonneg, :Dcg_3, ?Args1:list, ?Args2:list, ?Args3:list, +Options:list(compound))// .
%! 'm**n'(?M:nonneg, ?N:nonneg, :Dcg_4, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, +Options:list(compound))// .
%! 'm**n'(?M:nonneg, ?N:nonneg, :Dcg_5, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, +Options:list(compound))// .
% Implements the ABNF `#rule`, as defined in RFC 2616 (HTTP 1.1).
%
% A construct `<m>#<n>` is defined, similar to `<m>*<n>`,
%  for defining lists of elements.
% The full form is `<n>#<m>element` indicating at least `n`
%  and at most `m` elements, each separated by one or more commas
%  and OPTIONAL `LWS`.
%
% ### Motivation & example
%
% This makes the notation of lists of grammar productions in HTTP 1.1
% very easy. For example the following rule:
%
% ```abnf
% ( *OWS element *( *OWS "," *OWS element ))
% ```
%
% ... can be written more concisely as:
%
% ```abnf
% 1#element
% ```
%
% ### Null elements
%
% Wherever this construct is used, null elements are allowed,
%  but do not contribute to the count of elements present.
% That is, `(element), , (element)` is permitted,
%  but counts as only two elements.
% Therefore, where at least one element is required,
%  at least one non-null element MUST be present.
%
% ### Default values
%
% Default values are 0 and infinity so that `#element` allows any number,
%  including zero;
%  `1#element` requires at least one;
%  and `1#2element` allows one or two.
%
% @compat RFC 7230

'm**n'(M, N, Dcg_0, Mod:Opts1) -->
  {merge_options([separator(abnf_list:'m##n_separator')], Opts1, Opts2)},
  'm*n'(M, N, Dcg_0, Mod:Opts2).

'm**n'(M, N, Dcg_1, L1, Mod:Opts1) -->
  {merge_options([separator(abnf_list:'m##n_separator')], Opts1, Opts2)},
  'm*n'(M, N, Dcg_1, L1, Mod:Opts2).

'm**n'(M, N, Dcg_2, L1, L2, Mod:Opts1) -->
  {merge_options([separator(abnf_list:'m##n_separator')], Opts1, Opts2)},
  'm*n'(M, N, Dcg_2, L1, L2, Mod:Opts2).

'm**n'(M, N, Dcg_3, L1, L2, L3, Mod:Opts1) -->
  {merge_options([separator(abnf_list:'m##n_separator')], Opts1, Opts2)},
  'm*n'(M, N, Dcg_3, L1, L2, L3, Mod:Opts2).

'm**n'(M, N, Dcg_4, L1, L2, L3, L4, Mod:Opts1) -->
  {merge_options([separator(abnf_list:'m##n_separator')], Opts1, Opts2)},
  'm*n'(M, N, Dcg_4, L1, L2, L3, L4, Mod:Opts2).

'm**n'(M, N, Dcg_5, L1, L2, L3, L4, L5, Mod:Opts1) -->
  {merge_options([separator(abnf_list:'m##n_separator')], Opts1, Opts2)},
  'm*n'(M, N, Dcg_5, L1, L2, L3, L4, L5, Mod:Opts2).





% HELPERS %

'm##n_separator' --> 'OWS', ",", 'OWS'.
