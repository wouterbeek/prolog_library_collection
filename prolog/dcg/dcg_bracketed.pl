:- module(
  dcg_bracketed,
  [
    bracketed//1, % :Dcg_0
    bracketed//2 % +Type:oneof([angular,curly,langular,round,square])
                 % :Dcg_0
  ]
).

/** <module> DCG bracketed

Support for bracketed expressions in DCG.

```prolog
?- use_module(library(dcg/dcg_bracketed)).
?- phrase(bracketed(Type, `monkey`), Cs).
Type = angular,
Cs = "<monkey>" ;
Type = curly,
Cs = "{monkey}" ;
Type = round,
Cs = "(monkey)" ;
Type = square,
Cs = "[monkey]" ;
Type = langular,
Cs = [12296, 109, 111, 110, 107, 101, 121, 12297].
---

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_unicode)).

:- meta_predicate(bracketed(//,?,?)).
:- meta_predicate(bracketed(+,//,?,?)).





%! bracketed(:Dcg_0)// .
% Wrapper around bracketed//2 using round brackets.

bracketed(Dcg_0) -->
  bracketed(round, Dcg_0).


%! bracketed(+Type:oneof([angular,curly,langular,round,square]), :Dcg_0)// .

bracketed(Type, Dcg_0) -->
  dcg_between(
    opening_bracket(Type, _),
    Dcg_0,
    closing_bracket(Type, _)
  ).
