:- module(
  dcg_quote,
  [
    quoted//1, % :Dcg_0
    quoted//2, % :Quote_0, :Dcg_0
    quoted//3  % ?Length:positive_integer, :Quote_0, :Dcg_0
  ]
).

/** <module> DCG Quoted

Support for quoting in DCGs.

@author Wouter Beek
@version 2015/07-2015/08, 2016/01
*/

:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_ext)).

:- meta_predicate
	quoted(//, ?, ?),
	quoted(//, //, ?, ?),
	quoted(?, //, //, ?, ?).





%! quoted(:Dcg_0)// .
%! quoted(:Quote_0, :Dcg_0)// .
%! quoted(?Length:positive_integer, :Quote_0, :Dcg_0)// .
% Typical values for Quote:
%   - `double_quote//0`
%   - `single_quote//0`

quoted(Dcg_0) -->
  quoted(double_quote, Dcg_0).
quoted(Quote_0, Dcg_0) -->
  quoted(1, Quote_0, Dcg_0).
quoted(N, Quote_0, Dcg_0) -->
  {quote(Quote_0)},
  dcg_between('#'(N, Quote_0), Dcg_0).

quote(_:Dcg_0):- (var(Dcg_0) -> quote_goal(Dcg_0) ; true).
quote_goal(double_quote).
quote_goal(single_quote).
