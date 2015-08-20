:- module(
  dcg_abnf,
  [
    '#'//3, '#'//4, '#'//5, '#'//6, '#'//7, '#'//8,
    '*'//2, '*'//3, '*'//4, '*'//5, '*'//6, '*'//7,
    '*n'//3, '*n'//4, '*n'//5, '*n'//6, '*n'//7, '*n'//8,
    '+'//2, '+'//3, '+'//4, '+'//5, '+'//6, '+'//7,
    '+n'//3, '+n'//4, '+n'//5, '+n'//6, '+n'//7, '+n'//8,
    '?'//2, '?'//3, '?'//4, '?'//5, '?'//6, '?'//7,
    'm*'//3, 'm*'//4, 'm*'//5, 'm*'//6, 'm*'//7, 'm*'//8,
    'm*n'//4, 'm*n'//5, 'm*n'//6, 'm*n'//7, 'm*n'//8, 'm*n'//9
  ]
).

/** <module> Augmented Backus-Naur Form (ABNF) in DCGs

@author Wouter Beek
@version 2015/07-2015/08
*/

:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(default)).
:- use_module(library(error)).
:- use_module(library(option)).

:- meta_predicate(#(?,//,:,?,?)).
:- meta_predicate(#(?,3,?,:,?,?)).
:- meta_predicate(#(?,4,?,?,:,?,?)).
:- meta_predicate(#(?,5,?,?,?,:,?,?)).
:- meta_predicate(#(?,6,?,?,?,?,:,?,?)).
:- meta_predicate(#(?,7,?,?,?,?,?,:,?,?)).
:- meta_predicate(*(//,:,?,?)).
:- meta_predicate(*(3,?,:,?,?)).
:- meta_predicate(*(4,?,?,:,?,?)).
:- meta_predicate(*(5,?,?,?,:,?,?)).
:- meta_predicate(*(6,?,?,?,?,:,?,?)).
:- meta_predicate(*(7,?,?,?,?,?,:,?,?)).
:- meta_predicate('*n'(?,//,:,?,?)).
:- meta_predicate('*n'(?,3,?,:,?,?)).
:- meta_predicate('*n'(?,4,?,?,:,?,?)).
:- meta_predicate('*n'(?,5,?,?,?,:,?,?)).
:- meta_predicate('*n'(?,6,?,?,?,?,:,?,?)).
:- meta_predicate('*n'(?,7,?,?,?,?,?,:,?,?)).
:- meta_predicate(+(//,:,?,?)).
:- meta_predicate(+(3,?,:,?,?)).
:- meta_predicate(+(4,?,?,:,?,?)).
:- meta_predicate(+(5,?,?,?,:,?,?)).
:- meta_predicate(+(6,?,?,?,?,:,?,?)).
:- meta_predicate(+(7,?,?,?,?,?,:,?,?)).
:- meta_predicate('+n'(?,//,:,?,?)).
:- meta_predicate('+n'(?,3,?,:,?,?)).
:- meta_predicate('+n'(?,4,?,?,:,?,?)).
:- meta_predicate('+n'(?,5,?,?,?,:,?,?)).
:- meta_predicate('+n'(?,6,?,?,?,?,:,?,?)).
:- meta_predicate('+n'(?,7,?,?,?,?,?,:,?,?)).
:- meta_predicate(?(//,+,?,?)).
:- meta_predicate(?(3,?,+,?,?)).
:- meta_predicate(?(4,?,?,+,?,?)).
:- meta_predicate(?(5,?,?,?,+,?,?)).
:- meta_predicate(?(6,?,?,?,?,+,?,?)).
:- meta_predicate(?(7,?,?,?,?,?,+,?,?)).
:- meta_predicate(call_dcg_sep(+,//,//,+,+,?,?)).
:- meta_predicate(call_dcg_sep(+,3,//,+,+,?,?)).
:- meta_predicate(call_dcg_sep(+,4,//,+,+,?,?)).
:- meta_predicate(call_dcg_sep(+,5,//,+,+,?,?)).
:- meta_predicate(call_dcg_sep(+,6,//,+,+,?,?)).
:- meta_predicate(call_dcg_sep(+,7,//,+,+,?,?)).
:- meta_predicate('m*'(?,//,:,?,?)).
:- meta_predicate('m*'(?,3,?,:,?,?)).
:- meta_predicate('m*'(?,4,?,?,:,?,?)).
:- meta_predicate('m*'(?,5,?,?,?,:,?,?)).
:- meta_predicate('m*'(?,6,?,?,?,?,:,?,?)).
:- meta_predicate('m*'(?,7,?,?,?,?,?,:,?,?)).
:- meta_predicate('m*n'(?,?,//,:,?,?)).
:- meta_predicate('m*n'(?,?,3,?,:,?,?)).
:- meta_predicate('m*n'(?,?,4,?,?,:,?,?)).
:- meta_predicate('m*n'(?,?,5,?,?,?,:,?,?)).
:- meta_predicate('m*n'(?,?,6,?,?,?,?,:,?,?)).
:- meta_predicate('m*n'(?,?,7,?,?,?,?,?,:,?,?)).
:- meta_predicate('m*n_generate'(?,?,+,-,//,//,+,-,?)).
:- meta_predicate('m*n_generate'(?,?,+,-,3,//,?,+,-,?)).
:- meta_predicate('m*n_generate'(?,?,+,-,4,//,?,?,+,-,?)).
:- meta_predicate('m*n_generate'(?,?,+,-,5,//,?,?,?,+,-,?)).
:- meta_predicate('m*n_generate'(?,?,+,-,6,//,?,?,?,?,+,-,?)).
:- meta_predicate('m*n_generate'(?,?,+,-,7,//,?,?,?,?,?,+,-,?)).
:- meta_predicate('m*n_parse'(?,?,+,-,//,//,+,+,?)).
:- meta_predicate('m*n_parse'(?,?,+,-,3,//,?,+,+,?)).
:- meta_predicate('m*n_parse'(?,?,+,-,4,//,?,?,+,+,?)).
:- meta_predicate('m*n_parse'(?,?,+,-,5,//,?,?,?,+,+,?)).
:- meta_predicate('m*n_parse'(?,?,+,-,6,//,?,?,?,?,+,+,?)).
:- meta_predicate('m*n_parse'(?,?,+,-,7,//,?,?,?,?,?,+,+,?)).

:- predicate_options('#'//3, 3, [pass_to('m*n'//4, 4)]).
:- predicate_options('#'//4, 4, [pass_to('m*n'//5, 5)]).
:- predicate_options('#'//5, 5, [pass_to('m*n'//6, 6)]).
:- predicate_options('#'//6, 6, [pass_to('m*n'//7, 7)]).
:- predicate_options('#'//7, 7, [pass_to('m*n'//8, 8)]).
:- predicate_options('#'//8, 8, [pass_to('m*n'//9, 9)]).

:- predicate_options('*'//2, 2, [pass_to('m*n'//4, 4)]).
:- predicate_options('*'//3, 3, [pass_to('m*n'//5, 5)]).
:- predicate_options('*'//4, 4, [pass_to('m*n'//6, 6)]).
:- predicate_options('*'//5, 5, [pass_to('m*n'//7, 7)]).
:- predicate_options('*'//6, 6, [pass_to('m*n'//8, 8)]).
:- predicate_options('*'//7, 7, [pass_to('m*n'//9, 9)]).

:- predicate_options('*n'//3, 3, [pass_to('m*n'//4, 4)]).
:- predicate_options('*n'//4, 4, [pass_to('m*n'//5, 5)]).
:- predicate_options('*n'//5, 5, [pass_to('m*n'//6, 6)]).
:- predicate_options('*n'//6, 6, [pass_to('m*n'//7, 7)]).
:- predicate_options('*n'//7, 7, [pass_to('m*n'//8, 8)]).
:- predicate_options('*n'//8, 8, [pass_to('m*n'//9, 9)]).

:- predicate_options('+'//2, 2, [pass_to('m*n'//4, 4)]).
:- predicate_options('+'//3, 3, [pass_to('m*n'//5, 5)]).
:- predicate_options('+'//4, 4, [pass_to('m*n'//6, 6)]).
:- predicate_options('+'//5, 5, [pass_to('m*n'//7, 7)]).
:- predicate_options('+'//6, 6, [pass_to('m*n'//8, 8)]).
:- predicate_options('+'//7, 7, [pass_to('m*n'//9, 9)]).

:- predicate_options('+n'//3, 3, [pass_to('m*n'//4, 4)]).
:- predicate_options('+n'//4, 4, [pass_to('m*n'//5, 5)]).
:- predicate_options('+n'//5, 5, [pass_to('m*n'//6, 6)]).
:- predicate_options('+n'//6, 6, [pass_to('m*n'//7, 7)]).
:- predicate_options('+n'//7, 7, [pass_to('m*n'//8, 8)]).
:- predicate_options('+n'//8, 8, [pass_to('m*n'//9, 9)]).

:- predicate_options('m*'//3, 3, [pass_to('m*n'//4, 4)]).
:- predicate_options('m*'//4, 4, [pass_to('m*n'//5, 5)]).
:- predicate_options('m*'//5, 5, [pass_to('m*n'//6, 6)]).
:- predicate_options('m*'//6, 6, [pass_to('m*n'//7, 7)]).
:- predicate_options('m*'//7, 7, [pass_to('m*n'//8, 8)]).
:- predicate_options('m*'//8, 8, [pass_to('m*n'//9, 9)]).

:- predicate_options('m*n'//4, 4, [
     copy_term(+boolean),
     count(-nonneg),
     separator(+callable)
   ]).
:- predicate_options('m*n'//5, 5, [
     copy_term(+boolean),
     count(-nonneg),
     separator(+callable),
     pass_to(convert/3, 1)
   ]).
:- predicate_options('m*n'//6, 6, [
     copy_term(+boolean),
     count(-nonneg),
     separator(+callable),
     pass_to(convert/3, 1)
   ]).
:- predicate_options('m*n'//7, 7, [
     copy_term(+boolean),
     count(-nonneg),
     separator(+callable),
     pass_to(convert/3, 1)
   ]).
:- predicate_options('m*n'//8, 8, [
     copy_term(+boolean),
     count(-nonneg),
     separator(+callable),
     pass_to(convert/3, 1)
   ]).
:- predicate_options('m*n'//9, 9, [
     copy_term(+boolean),
     count(-nonneg),
     separator(+callable),
     pass_to(convert/3, 1)
   ]).

:- predicate_options(convert/3, 1, [
     convert(+pair(between(1,5),oneof([atom,codes,string])))
   ]).

is_meta(separator).





%! #(?N:nonneg, :Dcg_0, :Options:list(compound))// .
%! #(?N:nonneg, :Dcg_1, ?Args1:list, :Options:list(compound))// .
%! #(?N:nonneg, :Dcg_2, ?Args1:list, ?Args2:list, :Options:list(compound))// .
%! #(?N:nonneg, :Dcg_3, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(compound))// .
%! #(?N:nonneg, :Dcg_4, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(compound))// .
%! #(?N:nonneg, :Dcg_5, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(compound))// .
%
% ### Module prefix
%
% Normally meta_options/3 must appear before other option predicates
%  (here: merge_options/3).
% In this module meta_options/3 occurs in 'm*n'//[4-9],
%  and must occur there because these are public predicates as well.
% Inserting meta_options/3 for each of the predicates #//[3-8] would
%  make the code slightly longer than needed and would perform the same
%  operation twice.
% This is why the module prefix `Mod` is explicitly carried over here.
%
% @see Wrappers around 'm*n'//[3-8] using `M =:= N`.

#(N, Dcg_0, Mod:Opts0) -->
  {merge_options([count(N)], Opts0, Opts)},
  'm*n'(N, N, Dcg_0, Mod:Opts).

#(N, Dcg_1, L1, Mod:Opts0) -->
  {merge_options([count(N)], Opts0, Opts)},
  'm*n'(N, N, Dcg_1, L1, Mod:Opts).

#(N, Dcg_2, L1, L2, Mod:Opts0) -->
  {merge_options([count(N)], Opts0, Opts)},
  'm*n'(N, N, Dcg_2, L1, L2, Mod:Opts).

#(N, Dcg_3, L1, L2, L3, Mod:Opts0) -->
  {merge_options([count(N)], Opts0, Opts)},
  'm*n'(N, N, Dcg_3, L1, L2, L3, Mod:Opts).

#(N, Dcg_4, L1, L2, L3, L4, Mod:Opts0) -->
  {merge_options([count(N)], Opts0, Opts)},
  'm*n'(N, N, Dcg_4, L1, L2, L3, L4, Mod:Opts).

#(N, Dcg_5, L1, L2, L3, L4, L5, Mod:Opts0) -->
  {merge_options([count(N)], Opts0, Opts)},
  'm*n'(N, N, Dcg_5, L1, L2, L3, L4, L5, Mod:Opts).



%! *(:Dcg_0, :Options:list(compound))// .
%! *(:Dcg_1, ?Args1:list, :Options:list(compound))// .
%! *(:Dcg_2, ?Args1:list, ?Args3:list, :Options:list(compound))// .
%! *(:Dcg_3, ?Args1:list, ?Args3:list, ?Args3:list, :Options:list(compound))// .
%! *(:Dcg_4, ?Args1:list, ?Args3:list, ?Args3:list, ?Args4:list, :Options:list(compound))// .
%! *(:Dcg_5, ?Args1:list, ?Args3:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(compound))// .
% Implements the Regular Expression operator `*` in a nondeterministic way.
%
% @see Wrapper around 'm*n'//[3-8] with `M = 0` and `N` uninstantiated.

*(Dcg_0, Opts) -->
  'm*n'(_, _, Dcg_0, Opts).

*(Dcg_1, L1, Opts) -->
  'm*n'(_, _, Dcg_1, L1, Opts).

*(Dcg_2, L1, L2, Opts) -->
  'm*n'(_, _, Dcg_2, L1, L2, Opts).

*(Dcg_3, L1, L2, L3, Opts) -->
  'm*n'(_, _, Dcg_3, L1, L2, L3, Opts).

*(Dcg_4, L1, L2, L3, L4, Opts) -->
  'm*n'(_, _, Dcg_4, L1, L2, L3, L4, Opts).

*(Dcg_5, L1, L2, L3, L4, L5, Opts) -->
  'm*n'(_, _, Dcg_5, L1, L2, L3, L4, L5, Opts).



%! '*n'(?N:nonneg, :Dcg_0, :Options:list(compound))// .
%! '*n'(?N:nonneg, :Dcg_1, ?Args1:list, :Options:list(compound))// .
%! '*n'(?N:nonneg, :Dcg_2, ?Args1:list, ?Args2:list, :Options:list(compound))// .
%! '*n'(?N:nonneg, :Dcg_3, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(compound))// .
%! '*n'(?N:nonneg, :Dcg_4, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(compound))// .
%! '*n'(?N:nonneg, :Dcg_5, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(compound))// .
% @see Wrappers around 'm*n'//[3-8] with `M = 0` and given `N`.

'*n'(N, Dcg_0, Opts) -->
  'm*n'(_, N, Dcg_0, Opts).

'*n'(N, Dcg_1, L1, Opts) -->
  'm*n'(_, N, Dcg_1, L1, Opts).

'*n'(N, Dcg_2, L1, L2, Opts) -->
  'm*n'(_, N, Dcg_2, L1, L2, Opts).

'*n'(N, Dcg_3, L1, L2, L3, Opts) -->
  'm*n'(_, N, Dcg_3, L1, L2, L3, Opts).

'*n'(N, Dcg_4, L1, L2, L3, L4, Opts) -->
  'm*n'(_, N, Dcg_4, L1, L2, L3, L4, Opts).

'*n'(N, Dcg_5, L1, L2, L3, L4, L5, Opts) -->
  'm*n'(_, N, Dcg_5, L1, L2, L3, L4, L5, Opts).



%! +(:Dcg_0, +Options:list(compound))// .
%! +(:Dcg_1, ?Args1:list, :Options:list(compound))// .
%! +(:Dcg_2, ?Args1:list, ?Args2:list, :Options:list(compound))// .
%! +(:Dcg_3, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(compound))// .
%! +(:Dcg_4, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(compound))// .
%! +(:Dcg_5, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(compound))// .
% @see Wrappers around 'm*n'//[3-8] with `M = 1` and unbound `N`.

+(Dcg_0, Opts) -->
  'm*n'(1, _, Dcg_0, Opts).

+(Dcg_1, L1, Opts) -->
  'm*n'(1, _, Dcg_1, L1, Opts).

+(Dcg_2, L1, L2, Opts) -->
  'm*n'(1, _, Dcg_2, L1, L2, Opts).

+(Dcg_3, L1, L2, L3, Opts) -->
  'm*n'(1, _, Dcg_3, L1, L2, L3, Opts).

+(Dcg_4, L1, L2, L3, L4, Opts) -->
  'm*n'(1, _, Dcg_4, L1, L2, L3, L4, Opts).

+(Dcg_5, L1, L2, L3, L4, L5, Opts) -->
  'm*n'(1, _, Dcg_5, L1, L2, L3, L4, L5, Opts).



%! '+n'(?N:nonneg, :Dcg_0, +Options:list(compound))// .
%! '+n'(?N:nonneg, :Dcg_1, ?Args1:list, :Options:list(compound))// .
%! '+n'(?N:nonneg, :Dcg_2, ?Args1:list, ?Args2:list, :Options:list(compound))// .
%! '+n'(?N:nonneg, :Dcg_3, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(compound))// .
%! '+n'(?N:nonneg, :Dcg_4, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(compound))// .
%! '+n'(?N:nonneg, :Dcg_5, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(compound))// .
% @see Wrappers around 'm*n'//[3-8] with `M = 1` and given `N`.

'+n'(N, Dcg_0, Opts) -->
  'm*n'(1, N, Dcg_0, Opts).

'+n'(N, Dcg_1, L1, Opts) -->
  'm*n'(1, N, Dcg_1, L1, Opts).

'+n'(N, Dcg_2, L1, L2, Opts) -->
  'm*n'(1, N, Dcg_2, L1, L2, Opts).

'+n'(N, Dcg_3, L1, L2, L3, Opts) -->
  'm*n'(1, N, Dcg_3, L1, L2, L3, Opts).

'+n'(N, Dcg_4, L1, L2, L3, L4, Opts) -->
  'm*n'(1, N, Dcg_4, L1, L2, L3, L4, Opts).

'+n'(N, Dcg_5, L1, L2, L3, L4, L5, Opts) -->
  'm*n'(1, N, Dcg_5, L1, L2, L3, L4, L5, Opts).



%! ?(:Dcg_0, +Options:list(compound))// .
%! ?(:Dcg_1, ?Args1:list, +Options:list(compound))// .
%! ?(:Dcg_2, ?Args1:list, ?Args2:list, +Options:list(compound))// .
%! ?(:Dcg_3, ?Args1:list, ?Args2:list, ?Args3:list, +Options:list(compound))// .
%! ?(:Dcg_4, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, +Options:list(compound))// .
%! ?(:Dcg_5, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, +Options:list(compound))// .
% Implements the Regular Expression operator `?`,
% generating *both* the case of 0 occurrences *and* the case of 1 occurrence.
%
% Takes the following additional options:
%    - empty1(+term)
%    - empty2(+term)
%    - empty3(+term)
%    - empty4(+term)
%    - empty5(+term)
%
% @see Wrapper around 'm*n'//[3-8] with `M = 0` and `N = 1`.

?(Dcg_0, Opts) -->
  'm*n'(0, 1, Dcg_0, Opts).

?(Dcg_1, L1, Opts) -->
  'm*n'(0, 1, Dcg_1, L1, Opts).

?(Dcg_2, L1, L2, _) -->
  call(Dcg_2, L1, L2), !.
?(_, E1, E2, Opts) -->
  {
    option(empty1(E1), Opts, _Var1),
    option(empty2(E2), Opts, _Var2)
  }.

?(Dcg_3, L1, L2, L3, _) -->
  call(Dcg_3, L1, L2, L3), !.
?(_, E1, E2, E3, Opts) -->
  {
    option(empty1(E1), Opts, _Var1),
    option(empty2(E2), Opts, _Var2),
    option(empty3(E3), Opts, _Var3)
  }.

?(Dcg_4, L1, L2, L3, L4, _) -->
  call(Dcg_4, L1, L2, L3, L4), !.
?(_, E1, E2, E3, E4, Opts) -->
  {
    option(empty1(E1), Opts, _Var1),
    option(empty2(E2), Opts, _Var2),
    option(empty3(E3), Opts, _Var3),
    option(empty4(E4), Opts, _Var4)
  }.

?(Dcg_5, L1, L2, L3, L4, L5, _) -->
  call(Dcg_5, L1, L2, L3, L4, L5), !.
?(_, E1, E2, E3, E4, E5, Opts) -->
  {
    option(empty1(E1), Opts, _Var1),
    option(empty2(E2), Opts, _Var2),
    option(empty3(E3), Opts, _Var3),
    option(empty4(E4), Opts, _Var4),
    option(empty5(E5), Opts, _Var5)
  }.



%! 'm*'(?M:nonneg, :Dcg_0, :Options:list(compound))// .
%! 'm*'(?M:nonneg, :Dcg_1, ?Args1:list, :Options:list(compound))// .
%! 'm*'(?M:nonneg, :Dcg_2, ?Args1:list, ?Args2:list, :Options:list(compound))// .
%! 'm*'(?M:nonneg, :Dcg_3, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(compound))// .
%! 'm*'(?M:nonneg, :Dcg_4, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(compound))// .
%! 'm*'(?M:nonneg, :Dcg_5, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(compound))// .
% @see Wrapper around 'm*n'//[3-8] with given `M` and unbounded `N`.

'm*'(M, Dcg_0, Opts) -->
  'm*n'(M, _, Dcg_0, Opts).

'm*'(M, Dcg_1, L1, Opts) -->
  'm*n'(M, _, Dcg_1, L1, Opts).

'm*'(M, Dcg_2, L1, L2, Opts) -->
  'm*n'(M, _, Dcg_2, L1, L2, Opts).

'm*'(M, Dcg_3, L1, L2, L3, Opts) -->
  'm*n'(M, _, Dcg_3, L1, L2, L3, Opts).

'm*'(M, Dcg_4, L1, L2, L3, L4, Opts) -->
  'm*n'(M, _, Dcg_4, L1, L2, L3, L4, Opts).

'm*'(M, Dcg_5, L1, L2, L3, L4, L5, Opts) -->
  'm*n'(M, _, Dcg_5, L1, L2, L3, L4, L5, Opts).



%! 'm*n'(?M:nonneg, ?N:nonneg, :Dcg_0, :Options:list(compound))// .
%! 'm*n'(?M:nonneg, ?N:nonneg, :Dcg_1, ?Args1:list, :Options:list(compound))// .
%! 'm*n'(?M:nonneg, ?N:nonneg, :Dcg_2, ?Args1:list, ?Args2:list, :Options:list(compound))// .
%! 'm*n'(?M:nonneg, ?N:nonneg, :Dcg_3, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(compound))// .
%! 'm*n'(?M:nonneg, ?N:nonneg, :Dcg_4, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(compound))// .
%! 'm*n'(?M:nonneg, ?N:nonneg, :Dcg_5, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(compound))// .
% This predicate lies at the bases of all the other public predicates
% in this module.
%
% The uninstantiated variables in `Dcg` are *not* shared between calls
% (implemented by calling a copy of `Dcg`, using copy_term/2).
%
% Typechecking is performed on `M` and `N`,
% throwing a `type_error` if they are not integers,
% throwing a `domain_error` if they are negative integers,
% and failing silently when `N < M`.
%
% The following options are supported:
%   * convert(+compound)
%   * copy_term(+boolean)
%     Whether variables are shared between multiple calls of `Dcg`
%     (`false`, default) or not (`true`).
%   * count(-nonneg)
%     The number of times `Dcg` was called.
%   * separator(:Separator)
%     Meta-declaration `//`.
%
% @compat Semi-compatible with the specification of
%         Augmented Backus-Naur Form in RFC 2616 (HTTP 1.1).
% @throws type_error when `M` or N` is not an integer.
% @throws domain_error when `M` or `N` is a negative integer.

'm*n'(M, N, Dcg_0, Opts0) -->
  {
    'm*n_typecheck'(M, N),
    meta_options(is_meta, Opts0, Opts),
    option(copy_term(CP), Opts, false),
    option(separator(Sep_0), Opts, dcg_void)
  },
  (   parsing
  ->  'm*n_parse'(M, N, 0, C, Dcg_0, Sep_0, CP)
  ;   'm*n_generate'(M, N, 0, C, Dcg_0, Sep_0, CP)
  ),
  {(  option(count(C0), Opts)
  ->  C0 = C
  ;   true
  )}.

'm*n'(M, N, Dcg_1, L1, Opts0) -->
  {
    'm*n_typecheck'(M, N),
    meta_options(is_meta, Opts0, Opts),
    option(copy_term(CP), Opts, false),
    option(separator(Sep_0), Opts, dcg_void)
  },
  (   parsing
  ->  'm*n_parse'(M, N, 0, C, Dcg_1, Sep_0, L1_, CP),
      {convert(Opts, [L1], [L1_])}
  ;   {convert(Opts, [L1], [L1_])},
      'm*n_generate'(M, N, 0, C, Dcg_1, Sep_0, L1_, CP)
  ),
  {(  option(count(C0), Opts)
  ->  C0 = C
  ;   true
  )}.

'm*n'(M, N, Dcg_2, L1, L2, Opts0) -->
  {
    'm*n_typecheck'(M, N),
    meta_options(is_meta, Opts0, Opts),
    option(copy_term(CP), Opts, false),
    option(separator(Sep_0), Opts, dcg_void)
  },
  (   parsing
  ->  'm*n_parse'(M, N, 0, C, Dcg_2, Sep_0, L1_, L2_, CP),
      {convert(Opts, [L1,L2], [L1_,L2_])}
  ;   {convert(Opts, [L1,L2], [L1_,L2_])},
      'm*n_generate'(M, N, 0, C, Dcg_2, Sep_0, L1_, L2_, CP)
  ),
  {(  option(count(C0), Opts)
  ->  C0 = C
  ;   true
  )}.

'm*n'(M, N, Dcg_3, L1, L2, L3, Opts0) -->
  {
    'm*n_typecheck'(M, N),
    meta_options(is_meta, Opts0, Opts),
    option(copy_term(CP), Opts, false),
    option(separator(Sep_0), Opts, dcg_void)
  },
  (   parsing
  ->  'm*n_parse'(M, N, 0, C, Dcg_3, Sep_0, L1_, L2_, L3_, CP),
      {convert(Opts, [L1,L2,L3], [L1_,L2_,L3_])}
  ;   {convert(Opts, [L1,L2,L3], [L1_,L2_,L3_])},
      'm*n_generate'(M, N, 0, C, Dcg_3, Sep_0, L1_, L2_, L3_, CP)
  ),
  {(  option(count(C0), Opts)
  ->  C0 = C
  ;   true
  )}.

'm*n'(M, N, Dcg_4, L1, L2, L3, L4, Opts0) -->
  {
    'm*n_typecheck'(M, N),
    meta_options(is_meta, Opts0, Opts),
    option(copy_term(CP), Opts, false),
    option(separator(Sep_0), Opts, dcg_void)
  },
  (   parsing
  ->  'm*n_parse'(M, N, 0, C, Dcg_4, Sep_0, L1_, L2_, L3_, L4_, CP),
      {convert(Opts, [L1,L2,L3,L4], [L1_,L2_,L3_,L4_])}
  ;   {convert(Opts, [L1,L2,L3,L4], [L1_,L2_,L3_,L4_])},
      'm*n_generate'(M, N, 0, C, Dcg_4, Sep_0, L1_, L2_, L3_, L4_, CP)
  ),
  {(  option(count(C0), Opts)
  ->  C0 = C
  ;   true
  )}.

'm*n'(M, N, Dcg_5, L1, L2, L3, L4, L5, Opts0) -->
  {
    'm*n_typecheck'(M, N),
    meta_options(is_meta, Opts0, Opts),
    option(copy_term(CP), Opts, false),
    option(separator(Sep_0), Opts, dcg_void)
  },
  (   parsing
  ->  'm*n_parse'(M, N, 0, C, Dcg_5, Sep_0, L1_, L2_, L3_, L4_, L5_, CP),
      {convert(Opts, [L1,L2,L3,L4,L5], [L1_,L2_,L3_,L4_,L5_])}
  ;   {convert(Opts, [L1,L2,L3,L4,L5], [L1_,L2_,L3_,L4_,L5_])},
      'm*n_generate'(M, N, 0, C, Dcg_5, Sep_0, L1_, L2_, L3_, L4_, L5_, CP)
  ),
  {(  option(count(C0), Opts)
  ->  C0 = C
  ;   true
  )}.





% HELPERS %

%! call_dcg_sep(+Count:nonneg, :Dcg_n, :Separator_0, +Args:list, +CopyTerm:boolean)// .

call_dcg_sep(C, Dcg_n, Sep_0, Args, false) --> !,
  (   {C =:= 0}
  ->  ""
  ;   dcg_call(Sep_0)
  ),
  dcg_apply(Dcg_n, Args).
call_dcg_sep(C, Dcg_n, Sep_0, Args, true) -->
  (   {C =:= 0}
  ->  ""
  ;   dcg_call_cp(Sep_0)
  ),
  dcg_apply_cp(Dcg_n, Args).



%! convert(+Options:list(compound), +Arguments:list, -Codess:list(list(code))) is det.
%! convert(+Options:list(compound), -Arguments:list, +Codess:list(list(code))) is det.

convert(Opts, L1, L2):-
  convert(Opts, 1, L1, L2).

convert(_, _, [], []):- !.
convert(Opts, I1, [H1|T1], [H2|T2]):-
  option(convert(I1-Type), Opts), !,
  must_be(oneof([atom,codes,string]), Type),
  (   Type == atom
  ->  atom_codes(H1, H2)
  ;   Type == codes
  ->  H2 = H1
  ;   Type == string
  ->  string_codes(H1, H2)
  ),
  I2 is I1 + 1,
  convert(Opts, I2, T1, T2).
convert(Opts, I1, [H|T1], [H|T2]):-
  I2 is I1 + 1,
  convert(Opts, I2, T1, T2).



%! 'm*n_generate'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg, :Dcg_0, :Separator_0, +CopyTerm:boolean)// .
%! 'm*n_generate'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg, :Dcg_1, :Separator_0, ?Args1:list, +CopyTerm:boolean)// .
%! 'm*n_generate'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg, :Dcg_2, :Separator_0, ?Args1:list, ?Args2:list, +CopyTerm:boolean)// .
%! 'm*n_generate'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg, :Dcg_3, :Separator_0, ?Args1:list, ?Args2:list, ?Args3:list, +CopyTerm:boolean)// .
%! 'm*n_generate'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg, :Dcg_4, :Separator_0, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, +CopyTerm:boolean)// .
%! 'm*n_generate'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg, :Dcg_5, :Separator_0, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, +CopyTerm:boolean)// .
% Since generating is meager, we try to stop generating
% instances of `Dcg` as soon as possible.

'm*n_generate'(M, _, C, C, _, _, _) -->
  {'m*n_lower'(M, C)},
  "".
'm*n_generate'(M, N, C1, C, Dcg_0, Sep_0, CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg_0, Sep_0, [], CP),
  {C2 is C1 + 1},
  'm*n_generate'(M, N, C2, C, Dcg_0, Sep_0, CP).

'm*n_generate'(M, _, C, C, _, _, [], _) -->
  {'m*n_lower'(M, C)},
  "".
'm*n_generate'(M, N, C1, C, Dcg_1, Sep_0, [H1|T1], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg_1, Sep_0, [H1], CP),
  {C2 is C1 + 1},
  'm*n_generate'(M, N, C2, C, Dcg_1, Sep_0, T1, CP).

'm*n_generate'(M, _, C, C, _, _, [], [], _) -->
  {'m*n_lower'(M, C)},
  "".
'm*n_generate'(M, N, C1, C, Dcg_2, Sep_0, [H1|T1], [H2|T2], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg_2, Sep_0, [H1,H2], CP),
  {C2 is C1 + 1},
  'm*n_generate'(M, N, C2, C, Dcg_2, Sep_0, T1, T2, CP).

'm*n_generate'(M, _, C, C, _, _, [], [], [], _) -->
  {'m*n_lower'(M, C)},
  "".
'm*n_generate'(M, N, C1, C, Dcg_3, Sep_0, [H1|T1], [H2|T2], [H3|T3], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg_3, Sep_0, [H1,H2,H3], CP),
  {C2 is C1 + 1},
  'm*n_generate'(M, N, C2, C, Dcg_3, Sep_0, T1, T2, T3, CP).

'm*n_generate'(M, _, C, C, _, _, [], [], [], [], _) -->
  {'m*n_lower'(M, C)},
  "".
'm*n_generate'(M, N, C1, C, Dcg_4, Sep_0, [H1|T1], [H2|T2], [H3|T3], [H4|T4], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg_4, Sep_0, [H1,H2,H3,H4], CP),
  {C2 is C1 + 1},
  'm*n_generate'(M, N, C2, C, Dcg_4, Sep_0, T1, T2, T3, T4, CP).

'm*n_generate'(M, _, C, C, _, _, [], [], [], [], [], _) -->
  {'m*n_lower'(M, C)},
  "".
'm*n_generate'(M, N, C1, C, Dcg_5, Sep_0, [H1|T1], [H2|T2], [H3|T3], [H4|T4], [H5|T5], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg_5, Sep_0, [H1,H2,H3,H4,H5], CP),
  {C2 is C1 + 1},
  'm*n_generate'(M, N, C2, C, Dcg_5, Sep_0, T1, T2, T3, T4, T5, CP).



%! 'm*n_higher'(+N:nonneg, +Counter:nonneg) is semidet.
% Succeeds whenever the higher bound in 'm*n'//[3-8] is respected.

'm*n_higher'(N, C):-
  nonvar(N),
  N =< C, !,
  fail.
'm*n_higher'(_, _).



%! 'm*n_lower'(?M:nonneg, +Counter:nonneg) is semidet.
% Succeeds whenever the lower bound in 'm*n'//[3-8] is respected.

'm*n_lower'(M, C):-
  defval(0, M),
  M > C, !,
  fail.
'm*n_lower'(_, _).



%! 'm*n_parse'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg, :Dcg_0, :Sep_0, +CopyTerm:boolean)// .
%! 'm*n_parse'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg, :Dcg_1, :Sep_0, -Args1:list, +CopyTerm:boolean)// .
%! 'm*n_parse'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg, :Dcg_2, :Sep_0, -Args1:list, -Args2:list, +CopyTerm:boolean)// .
%! 'm*n_parse'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg, :Dcg_3, :Sep_0, -Args1:list, -Args2:list, -Args3:list, +CopyTerm:boolean)// .
%! 'm*n_parse'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg, :Dcg_4, :Sep_0, -Args1:list, -Args2:list, -Args3:list, -Args4:list, +CopyTerm:boolean)// .
%! 'm*n_parse'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg, :Dcg_5, :Sep_0, -Args1:list, -Args2:list, -Args3:list, -Args4:list, -Args5:list, +CopyTerm:boolean)// .
% Since parsing is eager, we try to process as many instances of `Dcg`
% as possible.

'm*n_parse'(M, N, C1, C, Dcg_0, Sep_0, CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg_0, Sep_0, [], CP),
  {C2 is C1 + 1},
  'm*n_parse'(M, N, C2, C, Dcg_0, Sep_0, CP).
'm*n_parse'(M, _, C, C, _, _, _) -->
  {'m*n_lower'(M, C)},
  "".

'm*n_parse'(M, N, C1, C, Dcg_1, Sep_0, [H1|T1], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg_1, Sep_0, [H1], CP),
  {C2 is C1 + 1},
  'm*n_parse'(M, N, C2, C, Dcg_1, Sep_0, T1, CP).
'm*n_parse'(M, _, C, C, _, _, [], _) -->
  {'m*n_lower'(M, C)},
  "".

'm*n_parse'(M, N, C1, C, Dcg_2, Sep_0, [H1|T1], [H2|T2], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg_2, Sep_0, [H1,H2], CP),
  {C2 is C1 + 1},
  'm*n_parse'(M, N, C2, C, Dcg_2, Sep_0, T1, T2, CP).
'm*n_parse'(M, _, C, C, _, _, [], [], _) -->
  {'m*n_lower'(M, C)},
  "".

'm*n_parse'(M, N, C1, C, Dcg_3, Sep_0, [H1|T1], [H2|T2], [H3|T3], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg_3, Sep_0, [H1,H2,H3], CP),
  {C2 is C1 + 1},
  'm*n_parse'(M, N, C2, C, Dcg_3, Sep_0, T1, T2, T3, CP).
'm*n_parse'(M, _, C, C, _, _, [], [], [], _) -->
  {'m*n_lower'(M, C)},
  "".

'm*n_parse'(M, N, C1, C, Dcg_4, Sep_0, [H1|T1], [H2|T2], [H3|T3], [H4|T4], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg_4, Sep_0, [H1,H2,H3,H4], CP),
  {C2 is C1 + 1},
  'm*n_parse'(M, N, C2, C, Dcg_4, Sep_0, T1, T2, T3, T4, CP).
'm*n_parse'(M, _, C, C, _, _, [], [], [], [], _) -->
  {'m*n_lower'(M, C)},
  "".

'm*n_parse'(M, N, C1, C, Dcg_5, Sep_0, [H1|T1], [H2|T2], [H3|T3], [H4|T4], [H5|T5], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg_5, Sep_0, [H1,H2,H3,H4,H5], CP),
  {C2 is C1 + 1},
  'm*n_parse'(M, N, C2, C, Dcg_5, Sep_0, T1, T2, T3, T4, T5, CP).
'm*n_parse'(M, _, C, C, _, _, [], [], [], [], [], _) -->
  {'m*n_lower'(M, C)},
  "".



% Type error for M.
'm*n_typecheck'(M, _):-
  nonvar(M),
  \+ integer(M), !,
  type_error(integer, M).
% Domain error for M.
'm*n_typecheck'(M, _):-
  nonvar(M),
  M < 0, !,
  domain_error(nonneg, M).
% Type error for N.
% Domain error for N.
'm*n_typecheck'(_, N):-
  nonvar(N),
  (   \+ integer(N)
  ->  type_error(integer, N)
  ;   N < 0
  ->  domain_error(nonvar, N)
  ).
% N below M: fail silently.
'm*n_typecheck'(M, N):-
  nonvar(M),
  nonvar(N),
  N < M, !,
  fail.
% Everything else succeeds.
'm*n_typecheck'(_, _).
