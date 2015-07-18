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

While DCGs are nice, the use of Backus Naur Form-notation (BNF)
sometimes results in simpler code.

For example, the following is quite common in DCGs:

```prolog
word(Word) -->
  letters(Codes),
  atom_codes(Word, Codes).

letters([H|T]) -->
  letter(H),
  letters(T).
letters([]) --> "".
```

This can be written down simpler using the Kleene star (`*`):

```prolog
word(Word) -->
  '*'(letter, Codes ,[]),
  atom_codes(Word, Codes).
```

Inspired by
[RFC 5234: Advanced Backus Naur Form (ABNF)](https://tools.ietf.org/html/rfc5234),
library `dcg_abnf` introduces the following operators:

| **Expression**            | **Meaning**                       |
| ------------------------- | --------------------------------- |
| `'#'(?N, :Dcg, [])`       | Process `Dcg` exactly `N` times.  |
| `'*'(:Dcg, [])`           | Process `Dcg` 0 or more times.    |
| `'*n'(?N, :Dcg, [])`      | Process `Dcg` at most `N` times.  |
| `'+'(:Dcg, [])`           | Process `Dcg` 1 or more times.    |
| `'?'(:Dcg, [])`           | Process `Dcg` 0 or 1 times. Alternatively: `Dcg` is optional. |
| `'m*'(?M, :Dcg, [])`      | Process `Dcg` at least `M` times. |
| `'m*n'(?M, ?N, :Dcg, [])` | Process `Dcg` at least `M` and at most `N` times.            |

In the previous table the last argument is the empty list.
This list can contain any of the following options:

| **Option**             | **Meaning** |
| ---------------------- | ----------- |
| `copy_term(+boolean)`  | Whether or not `Dcg` should first be copied before being processed. |
| `count(-nonneg)`       | The exact number of times `Dcg` is processed. For `'#'//[3-8]` this is the same as `N`. For all other ABNF operators this returns a non-trivial results that may be informative to the calling context. |
| `separator(+callable)` | If `Dcg` is processed more than once, the separator DCG rule is processed in between any two productions of `Dcg`. |


## Example with option `count` and `separator`

In the following example we state that a sentence consists of
one or more words that are separated by whitespace.
We also want to keep track of the number of words:

```prolog
sentence(N1, [H|T]) -->
  word(H),
  white,
  {succ(N1, N2)},
  words(N2, T).

words(N1, [H|T]) -->
  word(H),
  white,
  {succ(N1, N2)},
  words(N2, T).
words(0, []) --> "".
```

Using library `dcg_abnf` we can write this in a more concise way:

```prolog
sentence(N, Words) -->
  '+'(word, Words, [count(N),separator(white)]).
```


## Example solutions with option `count`

The predicates defined in this module allow the number of DCG productions
to be returned through the `count` option.

```prolog
?- phrase('*'(Count, arrow(Head, Length), [count(Count),copy_term(true)]), `<--->`).
Count = 1 ;   % `<--->`
Count = 2 ;   % `<---` and `>`
Count = 2 ;   % `<--` and `->`
Count = 2 ;   % `<-` and `-->`
Count = 2 ;   % `<` and `--->`
false.
```

### Uninstantiated variables: shared or not?

The DCG rules defined in this module allow the DCG goal
to be either copied or not using the `copy_term` option.

For `copy_term(true)` a new copy of the DCG rule is called each time.
For `copy_term(false)` the uninstantiated variables are shared
between all productions of DCG.
We illustrate this distinction with an example.

The following generates all sequences of at most 2 arrows
surrounded by triple quotes, with uninstantiated variables shared
between successive calls of `Dcg`.
The output shows that the single and double quote characters
do not occur in the same string.

```prolog
?- phrase('*n'(2, quoted(3, double_quote, arrow(right, 8)), [copy_term(false)]), Codes),
   atom_codes(Atom, Codes).
Codes = [],
Atom = '' ;
Codes = """"------->"""",
Atom = '"""------->"""' ;
Codes = """"------->""""""------->"""",
Atom = '"""------->""""""------->"""' ;
Codes = "'''------->'''",
Atom = '\'\'\'------->\'\'\'' ;
Codes = "'''------->''''''------->'''",
Atom = '\'\'\'------->\'\'\'\'\'\'------->\'\'\'' ;
false.
```

The following generates all sequences of at most 2 arrows
surrounded by triple quotes, without sharing variables
between successive calls of `Dcg`.
The output shows that the single and double quote characters
do not occur in the same string.

```prolog
?- phrase('*n'(2, quoted(3, double_quote, arrow(right, 8)), [copy_term(true)]), Codes),
   atom_codes(Atom, Codes).
Codes = [],
Atom = '' ;
Codes = """"------->"""",
Atom = '"""------->"""' ;
Codes = """"------->""""""------->"""",
Atom = '"""------->""""""------->"""' ;
Codes = """"------->"""'''------->'''",
Atom = '"""------->"""\'\'\'------->\'\'\'' ;
Codes = "'''------->'''",
Atom = '\'\'\'------->\'\'\'' ;
Codes = "'''------->'''"""------->"""",
Atom = '\'\'\'------->\'\'\'"""------->"""' ;
Codes = "'''------->''''''------->'''",
Atom = '\'\'\'------->\'\'\'\'\'\'------->\'\'\'' ;
false.
```

---

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(dcg/dcg_call)).
:- use_module(library(default)).
:- use_module(library(error)).
:- use_module(library(option)).

:- meta_predicate('#'(?,//,:,?,?)).
:- meta_predicate('#'(?,3,?,:,?,?)).
:- meta_predicate('#'(?,4,?,?,:,?,?)).
:- meta_predicate('#'(?,5,?,?,?,:,?,?)).
:- meta_predicate('#'(?,6,?,?,?,?,:,?,?)).
:- meta_predicate('#'(?,7,?,?,?,?,?,:,?,?)).
:- meta_predicate('*'(//,:,?,?)).
:- meta_predicate('*'(3,?,:,?,?)).
:- meta_predicate('*'(4,?,?,:,?,?)).
:- meta_predicate('*'(5,?,?,?,:,?,?)).
:- meta_predicate('*'(6,?,?,?,?,:,?,?)).
:- meta_predicate('*'(7,?,?,?,?,?,:,?,?)).
:- meta_predicate('*n'(?,//,:,?,?)).
:- meta_predicate('*n'(?,3,?,:,?,?)).
:- meta_predicate('*n'(?,4,?,?,:,?,?)).
:- meta_predicate('*n'(?,5,?,?,?,:,?,?)).
:- meta_predicate('*n'(?,6,?,?,?,?,:,?,?)).
:- meta_predicate('*n'(?,7,?,?,?,?,?,:,?,?)).
:- meta_predicate('+'(//,:,?,?)).
:- meta_predicate('+'(3,?,:,?,?)).
:- meta_predicate('+'(4,?,?,:,?,?)).
:- meta_predicate('+'(5,?,?,?,:,?,?)).
:- meta_predicate('+'(6,?,?,?,?,:,?,?)).
:- meta_predicate('+'(7,?,?,?,?,?,:,?,?)).
:- meta_predicate('+n'(?,//,:,?,?)).
:- meta_predicate('+n'(?,3,?,:,?,?)).
:- meta_predicate('+n'(?,4,?,?,:,?,?)).
:- meta_predicate('+n'(?,5,?,?,?,:,?,?)).
:- meta_predicate('+n'(?,6,?,?,?,?,:,?,?)).
:- meta_predicate('+n'(?,7,?,?,?,?,?,:,?,?)).
:- meta_predicate('?'(//,+,?,?)).
:- meta_predicate('?'(3,?,+,?,?)).
:- meta_predicate('?'(4,?,?,+,?,?)).
:- meta_predicate('?'(5,?,?,?,+,?,?)).
:- meta_predicate('?'(6,?,?,?,?,+,?,?)).
:- meta_predicate('?'(7,?,?,?,?,?,+,?,?)).
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
     separator(+callable)
   ]).
:- predicate_options('m*n'//6, 6, [
     copy_term(+boolean),
     count(-nonneg),
     separator(+callable)
   ]).
:- predicate_options('m*n'//7, 7, [
     copy_term(+boolean),
     count(-nonneg),
     separator(+callable)
   ]).
:- predicate_options('m*n'//8, 8, [
     copy_term(+boolean),
     count(-nonneg),
     separator(+callable)
   ]).
:- predicate_options('m*n'//9, 9, [
     copy_term(+boolean),
     count(-nonneg),
     separator(+callable)
   ]).

is_meta(separator).





%! '#'(?N:nonneg, :Dcg, :Options:list(nvpair))// .
%! '#'(?N:nonneg, :Dcg, ?Args1:list, :Options:list(nvpair))// .
%! '#'(?N:nonneg, :Dcg, ?Args1:list, ?Args2:list, :Options:list(nvpair))// .
%! '#'(?N:nonneg, :Dcg, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(nvpair))// .
%! '#'(?N:nonneg, :Dcg, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(nvpair))// .
%! '#'(?N:nonneg, :Dcg, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(nvpair))// .
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
% @see Wrappers around 'm*n'//[3-8] using `M =:= N`.

'#'(N, Dcg, Mod:Opts1) -->
  {merge_options([count(N)], Opts1, Opts2)},
  'm*n'(N, N, Dcg, Mod:Opts2).

'#'(N, Dcg, L1, Mod:Opts1) -->
  {merge_options([count(N)], Opts1, Opts2)},
  'm*n'(N, N, Dcg, L1, Mod:Opts2).

'#'(N, Dcg, L1, L2, Mod:Opts1) -->
  {merge_options([count(N)], Opts1, Opts2)},
  'm*n'(N, N, Dcg, L1, L2, Mod:Opts2).

'#'(N, Dcg, L1, L2, L3, Mod:Opts1) -->
  {merge_options([count(N)], Opts1, Opts2)},
  'm*n'(N, N, Dcg, L1, L2, L3, Mod:Opts2).

'#'(N, Dcg, L1, L2, L3, L4, Mod:Opts1) -->
  {merge_options([count(N)], Opts1, Opts2)},
  'm*n'(N, N, Dcg, L1, L2, L3, L4, Mod:Opts2).

'#'(N, Dcg, L1, L2, L3, L4, L5, Mod:Opts1) -->
  {merge_options([count(N)], Opts1, Opts2)},
  'm*n'(N, N, Dcg, L1, L2, L3, L4, L5, Mod:Opts2).



%! '*'(:Dcg, :Options:list(nvpair))// .
%! '*'(:Dcg, ?Args1:list, :Options:list(nvpair))// .
%! '*'(:Dcg, ?Args1:list, ?Args3:list, :Options:list(nvpair))// .
%! '*'(:Dcg, ?Args1:list, ?Args3:list, ?Args3:list, :Options:list(nvpair))// .
%! '*'(:Dcg, ?Args1:list, ?Args3:list, ?Args3:list, ?Args4:list, :Options:list(nvpair))// .
%! '*'(:Dcg, ?Args1:list, ?Args3:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(nvpair))// .
% Implements the Regular Expression operator `*` in a nondeterministic way.
%
% @see Wrapper around 'm*n'//[3-8] with `M = 0` and `N` uninstantiated.

'*'(Dcg, Opts) -->
  'm*n'(_, _, Dcg, Opts).

'*'(Dcg, L1, Opts) -->
  'm*n'(_, _, Dcg, L1, Opts).

'*'(Dcg, L1, L2, Opts) -->
  'm*n'(_, _, Dcg, L1, L2, Opts).

'*'(Dcg, L1, L2, L3, Opts) -->
  'm*n'(_, _, Dcg, L1, L2, L3, Opts).

'*'(Dcg, L1, L2, L3, L4, Opts) -->
  'm*n'(_, _, Dcg, L1, L2, L3, L4, Opts).

'*'(Dcg, L1, L2, L3, L4, L5, Opts) -->
  'm*n'(_, _, Dcg, L1, L2, L3, L4, L5, Opts).



%! '*n'(?N:nonneg, :Dcg, :Options:list(nvpair))// .
%! '*n'(?N:nonneg, :Dcg, ?Args1:list, :Options:list(nvpair))// .
%! '*n'(?N:nonneg, :Dcg, ?Args1:list, ?Args2:list, :Options:list(nvpair))// .
%! '*n'(?N:nonneg, :Dcg, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(nvpair))// .
%! '*n'(?N:nonneg, :Dcg, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(nvpair))// .
%! '*n'(?N:nonneg, :Dcg, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(nvpair))// .
% @see Wrappers around 'm*n'//[3-8] with `M = 0` and given `N`.

'*n'(N, Dcg, Opts) -->
  'm*n'(_, N, Dcg, Opts).

'*n'(N, Dcg, L1, Opts) -->
  'm*n'(_, N, Dcg, L1, Opts).

'*n'(N, Dcg, L1, L2, Opts) -->
  'm*n'(_, N, Dcg, L1, L2, Opts).

'*n'(N, Dcg, L1, L2, L3, Opts) -->
  'm*n'(_, N, Dcg, L1, L2, L3, Opts).

'*n'(N, Dcg, L1, L2, L3, L4, Opts) -->
  'm*n'(_, N, Dcg, L1, L2, L3, L4, Opts).

'*n'(N, Dcg, L1, L2, L3, L4, L5, Opts) -->
  'm*n'(_, N, Dcg, L1, L2, L3, L4, L5, Opts).



%! '+'(:Dcg, +Options:list(nvpair))// .
%! '+'(:Dcg, ?Args1:list, :Options:list(nvpair))// .
%! '+'(:Dcg, ?Args1:list, ?Args2:list, :Options:list(nvpair))// .
%! '+'(:Dcg, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(nvpair))// .
%! '+'(:Dcg, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(nvpair))// .
%! '+'(:Dcg, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(nvpair))// .
% @see Wrappers around 'm*n'//[3-8] with `M = 1` and unbound `N`.

'+'(Dcg, Opts) -->
  'm*n'(1, _, Dcg, Opts).

'+'(Dcg, L1, Opts) -->
  'm*n'(1, _, Dcg, L1, Opts).

'+'(Dcg, L1, L2, Opts) -->
  'm*n'(1, _, Dcg, L1, L2, Opts).

'+'(Dcg, L1, L2, L3, Opts) -->
  'm*n'(1, _, Dcg, L1, L2, L3, Opts).

'+'(Dcg, L1, L2, L3, L4, Opts) -->
  'm*n'(1, _, Dcg, L1, L2, L3, L4, Opts).

'+'(Dcg, L1, L2, L3, L4, L5, Opts) -->
  'm*n'(1, _, Dcg, L1, L2, L3, L4, L5, Opts).



%! '+n'(?N:nonneg, :Dcg, +Options:list(nvpair))// .
%! '+n'(?N:nonneg, :Dcg, ?Args1:list, :Options:list(nvpair))// .
%! '+n'(?N:nonneg, :Dcg, ?Args1:list, ?Args2:list, :Options:list(nvpair))// .
%! '+n'(?N:nonneg, :Dcg, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(nvpair))// .
%! '+n'(?N:nonneg, :Dcg, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(nvpair))// .
%! '+n'(?N:nonneg, :Dcg, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(nvpair))// .
% @see Wrappers around 'm*n'//[3-8] with `M = 1` and given `N`.

'+n'(N, Dcg, Opts) -->
  'm*n'(1, N, Dcg, Opts).

'+n'(N, Dcg, L1, Opts) -->
  'm*n'(1, N, Dcg, L1, Opts).

'+n'(N, Dcg, L1, L2, Opts) -->
  'm*n'(1, N, Dcg, L1, L2, Opts).

'+n'(N, Dcg, L1, L2, L3, Opts) -->
  'm*n'(1, N, Dcg, L1, L2, L3, Opts).

'+n'(N, Dcg, L1, L2, L3, L4, Opts) -->
  'm*n'(1, N, Dcg, L1, L2, L3, L4, Opts).

'+n'(N, Dcg, L1, L2, L3, L4, L5, Opts) -->
  'm*n'(1, N, Dcg, L1, L2, L3, L4, L5, Opts).



%! '?'(:Dcg, +Options:list(nvpair))// .
%! '?'(:Dcg, ?Args1:list, +Options:list(nvpair))// .
%! '?'(:Dcg, ?Args1:list, ?Args2:list, +Options:list(nvpair))// .
%! '?'(:Dcg, ?Args1:list, ?Args2:list, ?Args3:list, +Options:list(nvpair))// .
%! '?'(:Dcg, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, +Options:list(nvpair))// .
%! '?'(:Dcg, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, +Options:list(nvpair))// .
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

'?'(Dcg, Opts) -->
  'm*n'(0, 1, Dcg, Opts).

'?'(Dcg, L1, Opts) -->
  'm*n'(0, 1, Dcg, L1, Opts).

'?'(Dcg, L1, L2, _) -->
  call(Dcg, L1, L2), !.
'?'(_, E1, E2, Opts) -->
  {
    option(empty1(E1), Opts, _Var1),
    option(empty2(E2), Opts, _Var2)
  }.

'?'(Dcg, L1, L2, L3, _) -->
  call(Dcg, L1, L2, L3), !.
'?'(_, E1, E2, E3, Opts) -->
  {
    option(empty1(E1), Opts, _Var1),
    option(empty2(E2), Opts, _Var2),
    option(empty3(E3), Opts, _Var3)
  }.

'?'(Dcg, L1, L2, L3, L4, _) -->
  call(Dcg, L1, L2, L3, L4), !.
'?'(_, E1, E2, E3, E4, Opts) -->
  {
    option(empty1(E1), Opts, _Var1),
    option(empty2(E2), Opts, _Var2),
    option(empty3(E3), Opts, _Var3),
    option(empty4(E4), Opts, _Var4)
  }.

'?'(Dcg, L1, L2, L3, L4, L5, _) -->
  call(Dcg, L1, L2, L3, L4, L5), !.
'?'(_, E1, E2, E3, E4, E5, Opts) -->
  {
    option(empty1(E1), Opts, _Var1),
    option(empty2(E2), Opts, _Var2),
    option(empty3(E3), Opts, _Var3),
    option(empty4(E4), Opts, _Var4),
    option(empty5(E5), Opts, _Var5)
  }.



%! 'm*'(?M:nonneg, :Dcg, :Options:list(nvpair))// .
%! 'm*'(?M:nonneg, :Dcg, ?Args1:list, :Options:list(nvpair))// .
%! 'm*'(?M:nonneg, :Dcg, ?Args1:list, ?Args2:list, :Options:list(nvpair))// .
%! 'm*'(?M:nonneg, :Dcg, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(nvpair))// .
%! 'm*'(?M:nonneg, :Dcg, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(nvpair))// .
%! 'm*'(?M:nonneg, :Dcg, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(nvpair))// .
% @see Wrapper around 'm*n'//[3-8] with given `M` and unbounded `N`.

'm*'(M, Dcg, Opts) -->
  'm*n'(M, _, Dcg, Opts).

'm*'(M, Dcg, L1, Opts) -->
  'm*n'(M, _, Dcg, L1, Opts).

'm*'(M, Dcg, L1, L2, Opts) -->
  'm*n'(M, _, Dcg, L1, L2, Opts).

'm*'(M, Dcg, L1, L2, L3, Opts) -->
  'm*n'(M, _, Dcg, L1, L2, L3, Opts).

'm*'(M, Dcg, L1, L2, L3, L4, Opts) -->
  'm*n'(M, _, Dcg, L1, L2, L3, L4, Opts).

'm*'(M, Dcg, L1, L2, L3, L4, L5, Opts) -->
  'm*n'(M, _, Dcg, L1, L2, L3, L4, L5, Opts).



%! 'm*n'(?M:nonneg, ?N:nonneg, :Dcg, :Options:list(nvpair))// .
%! 'm*n'(?M:nonneg, ?N:nonneg, :Dcg, ?Args1:list, :Options:list(nvpair))// .
%! 'm*n'(?M:nonneg, ?N:nonneg, :Dcg, ?Args1:list, ?Args2:list, :Options:list(nvpair))// .
%! 'm*n'(?M:nonneg, ?N:nonneg, :Dcg, ?Args1:list, ?Args2:list, ?Args3:list, :Options:list(nvpair))// .
%! 'm*n'(?M:nonneg, ?N:nonneg, :Dcg, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, :Options:list(nvpair))// .
%! 'm*n'(?M:nonneg, ?N:nonneg, :Dcg, ?Args1:list, ?Args2:list, ?Args3:list, ?Args4:list, ?Args5:list, :Options:list(nvpair))// .
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
%   - copy_term(+boolean)
%     Whether variables are shared between multiple calls of `Dcg`
%     (`false`, default) or not (`true`).
%   - count(-nonneg)
%     The number of times `Dcg` was called.
%   - separator(:Separator)
%     Meta-declaration `//`.
%
% @compat Semi-compatible with the specification of
%         Augmented Backus-Naur Form in RFC 2616 (HTTP 1.1).
% @throws type_error when `M` or N` is not an integer.
% @throws domain_error when `M` or `N` is a negative integer.

'm*n'(M, N, Dcg, Opts1) -->
  {
    'm*n_typecheck'(M, N),
    meta_options(is_meta, Opts1, Opts2),
    option(copy_term(CP), Opts2, false),
    option(separator(Sep), Opts2, dcg_void)
  },
  (   parsing
  ->  'm*n_parse'(M, N, 0, C, Dcg, Sep, CP)
  ;   'm*n_generate'(M, N, 0, C, Dcg, Sep, CP)
  ),
  {(  option(count(C0), Opts2)
  ->  C0 = C
  ;   true
  )}.

'm*n'(M, N, Dcg, L1, Opts1) -->
  {
    'm*n_typecheck'(M, N),
    meta_options(is_meta, Opts1, Opts2),
    option(copy_term(CP), Opts2, false),
    option(separator(Sep), Opts2, dcg_void)
  },
  (   parsing
  ->  'm*n_parse'(M, N, 0, C, Dcg, Sep, L1, CP)
  ;   'm*n_generate'(M, N, 0, C, Dcg, Sep, L1, CP)
  ),
  {(  option(count(C0), Opts2)
  ->  C0 = C
  ;   true
  )}.

'm*n'(M, N, Dcg, L1, L2, Opts1) -->
  {
    'm*n_typecheck'(M, N),
    meta_options(is_meta, Opts1, Opts2),
    option(copy_term(CP), Opts2, false),
    option(separator(Sep), Opts2, dcg_void)
  },
  (   parsing
  ->  'm*n_parse'(M, N, 0, C, Dcg, Sep, L1, L2, CP)
  ;   'm*n_generate'(M, N, 0, C, Dcg, Sep, L1, L2, CP)
  ),
  {(  option(count(C0), Opts2)
  ->  C0 = C
  ;   true
  )}.

'm*n'(M, N, Dcg, L1, L2, L3, Opts1) -->
  {
    'm*n_typecheck'(M, N),
    meta_options(is_meta, Opts1, Opts2),
    option(copy_term(CP), Opts2, false),
    option(separator(Sep), Opts2, dcg_void)
  },
  (   parsing
  ->  'm*n_parse'(M, N, 0, C, Dcg, Sep, L1, L2, L3, CP)
  ;   'm*n_generate'(M, N, 0, C, Dcg, Sep, L1, L2, L3, CP)
  ),
  {(  option(count(C0), Opts2)
  ->  C0 = C
  ;   true
  )}.

'm*n'(M, N, Dcg, L1, L2, L3, L4, Opts1) -->
  {
    'm*n_typecheck'(M, N),
    meta_options(is_meta, Opts1, Opts2),
    option(copy_term(CP), Opts2, false),
    option(separator(Sep), Opts2, dcg_void)
  },
  (   parsing
  ->  'm*n_parse'(M, N, 0, C, Dcg, Sep, L1, L2, L3, L4, CP)
  ;   'm*n_generate'(M, N, 0, C, Dcg, Sep, L1, L2, L3, L4, CP)
  ),
  {(  option(count(C0), Opts2)
  ->  C0 = C
  ;   true
  )}.

'm*n'(M, N, Dcg, L1, L2, L3, L4, L5, Opts1) -->
  {
    'm*n_typecheck'(M, N),
    meta_options(is_meta, Opts1, Opts2),
    option(copy_term(CP), Opts2, false),
    option(separator(Sep), Opts2, dcg_void)
  },
  (   parsing
  ->  'm*n_parse'(M, N, 0, C, Dcg, Sep, L1, L2, L3, L4, L5, CP)
  ;   'm*n_generate'(M, N, 0, C, Dcg, Sep, L1, L2, L3, L4, L5, CP)
  ),
  {(  option(count(C0), Opts2)
  ->  C0 = C
  ;   true
  )}.



% HELPERS %

%! call_dcg_sep(
%!   +Count:nonneg,
%!   :Dcg,
%!   :Separator,
%!   +Arguments:list,
%!   +CopyTerm:boolean
%! )// .

call_dcg_sep(C, Dcg, Sep, Args, false) --> !,
  (   {C =:= 0}
  ->  ""
  ;   dcg_call(Sep)
  ),
  dcg_apply(Dcg, Args).
call_dcg_sep(C, Dcg, Sep, Args, true) -->
  (   {C =:= 0}
  ->  ""
  ;   dcg_call_cp(Sep)
  ),
  dcg_apply_cp(Dcg, Args).



%! 'm*n_generate'(
%!   ?M:nonneg,
%!   ?N:nonneg,
%!   +Counter:nonneg,
%!   -Count:nonneg,
%!   :Dcg,
%!   :Separator,
%!   +CopyTerm:boolean
%! )// .
%! 'm*n_generate'(
%!   ?M:nonneg,
%!   ?N:nonneg,
%!   +Counter:nonneg,
%!   -Count:nonneg,
%!   :Dcg,
%!   :Separator,
%!   ?Args1:list,
%!   :Convert1,
%!   +CopyTerm:boolean
%! )// .
% Since generating is meager, we try to stop generating
% instances of `Dcg` as soon as possible.

'm*n_generate'(M, _, C, C, _, _, _) -->
  {'m*n_lower'(M, C)},
  "".
'm*n_generate'(M, N, C1, C, Dcg, Sep, CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg, Sep, [], CP),
  {succ(C1, C2)},
  'm*n_generate'(M, N, C2, C, Dcg, Sep, CP).

'm*n_generate'(M, _, C, C, _, _, [], _) -->
  {'m*n_lower'(M, C)},
  "".
'm*n_generate'(M, N, C1, C, Dcg, Sep, [H1|T1], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg, Sep, [H1], CP),
  {succ(C1, C2)},
  'm*n_generate'(M, N, C2, C, Dcg, Sep, T1, CP).

'm*n_generate'(M, _, C, C, _, _, [], [], _) -->
  {'m*n_lower'(M, C)},
  "".
'm*n_generate'(M, N, C1, C, Dcg, Sep, [H1|T1], [H2|T2], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg, Sep, [H1,H2], CP),
  {succ(C1, C2)},
  'm*n_generate'(M, N, C2, C, Dcg, Sep, T1, T2, CP).

'm*n_generate'(M, _, C, C, _, _, [], [], [], _) -->
  {'m*n_lower'(M, C)},
  "".
'm*n_generate'(M, N, C1, C, Dcg, Sep, [H1|T1], [H2|T2], [H3|T3], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg, Sep, [H1,H2,H3], CP),
  {succ(C1, C2)},
  'm*n_generate'(M, N, C2, C, Dcg, Sep, T1, T2, T3, CP).

'm*n_generate'(M, _, C, C, _, _, [], [], [], [], _) -->
  {'m*n_lower'(M, C)},
  "".
'm*n_generate'(
  M, N, C1, C, Dcg, Sep, [H1|T1], [H2|T2], [H3|T3], [H4|T4], CP
) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg, Sep, [H1,H2,H3,H4], CP),
  {succ(C1, C2)},
  'm*n_generate'(M, N, C2, C, Dcg, Sep, T1, T2, T3, T4, CP).

'm*n_generate'(M, _, C, C, _, _, [], [], [], [], [], _) -->
  {'m*n_lower'(M, C)},
  "".
'm*n_generate'(
  M, N, C1, C, Dcg, Sep, [H1|T1], [H2|T2], [H3|T3], [H4|T4], [H5|T5], CP
) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg, Sep, [H1,H2,H3,H4,H5], CP),
  {succ(C1, C2)},
  'm*n_generate'(M, N, C2, C, Dcg, Sep, T1, T2, T3, T4, T5, CP).



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



%! 'm*n_parse'(
%!   ?M:nonneg,
%!   ?N:nonneg,
%!   +Counter:nonneg,
%!   -Count:nonneg,
%!   :Dcg,
%!   :Separator,
%!   +CopyTerm:boolean
%! )// .
%! 'm*n_parse'(
%!   ?M:nonneg,
%!   ?N:nonneg,
%!   +Counter:nonneg,
%!   -Count:nonneg,
%!   :Dcg,
%!   :Separator,
%!   -Args1:list,
%!   +CopyTerm:boolean
%! )// .
% Since parsing is eager, we try to process as many instances of `Dcg`
% as possible.

'm*n_parse'(M, N, C1, C, Dcg, Sep, CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg, Sep, [], CP),
  {succ(C1, C2)},
  'm*n_parse'(M, N, C2, C, Dcg, Sep, CP).
'm*n_parse'(M, _, C, C, _, _, _) -->
  {'m*n_lower'(M, C)},
  "".

'm*n_parse'(M, N, C1, C, Dcg, Sep, [H1|T1], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg, Sep, [H1], CP),
  {succ(C1, C2)},
  'm*n_parse'(M, N, C2, C, Dcg, Sep, T1, CP).
'm*n_parse'(M, _, C, C, _, _, [], _) -->
  {'m*n_lower'(M, C)},
  "".

'm*n_parse'(M, N, C1, C, Dcg, Sep, [H1|T1], [H2|T2], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg, Sep, [H1,H2], CP),
  {succ(C1, C2)},
  'm*n_parse'(M, N, C2, C, Dcg, Sep, T1, T2, CP).
'm*n_parse'(M, _, C, C, _, _, [], [], _) -->
  {'m*n_lower'(M, C)},
  "".

'm*n_parse'(M, N, C1, C, Dcg, Sep, [H1|T1], [H2|T2], [H3|T3], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg, Sep, [H1,H2,H3], CP),
  {succ(C1, C2)},
  'm*n_parse'(M, N, C2, C, Dcg, Sep, T1, T2, T3, CP).
'm*n_parse'(M, _, C, C, _, _, [], [], [], _) -->
  {'m*n_lower'(M, C)},
  "".

'm*n_parse'(M, N, C1, C, Dcg, Sep, [H1|T1], [H2|T2], [H3|T3], [H4|T4], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg, Sep, [H1,H2,H3,H4], CP),
  {succ(C1, C2)},
  'm*n_parse'(M, N, C2, C, Dcg, Sep, T1, T2, T3, T4, CP).
'm*n_parse'(M, _, C, C, _, _, [], [], [], [], _) -->
  {'m*n_lower'(M, C)},
  "".

'm*n_parse'(M, N, C1, C, Dcg, Sep, [H1|T1], [H2|T2], [H3|T3], [H4|T4], [H5|T5], CP) -->
  {'m*n_higher'(N, C1)},
  call_dcg_sep(C1, Dcg, Sep, [H1,H2,H3,H4,H5], CP),
  {succ(C1, C2)},
  'm*n_parse'(M, N, C2, C, Dcg, Sep, T1, T2, T3, T4, T5, CP).
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



%! parsing// is semidet.

parsing(H, H):-
   nonvar(H).
