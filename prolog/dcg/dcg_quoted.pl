:- module(
  dcg_quoted,
  [
    quoted//1, % :Dcg_0
    quoted//2, % :Quote_0
               % :Dcg_0
    quoted//3 % ?Length:positive_number
              % :Quote_0
              % :Dcg_0
  ]
).

/** <module> DCG quoted

Support for quoting in DCGs.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_unicode)).

:- meta_predicate(quoted(//,?,?)).
:- meta_predicate(quoted(//,//,?,?)).
:- meta_predicate(quoted(?,//,//,?,?)).





%! quoted(:Dcg_0)// .
% Wrapper around quoted//2 using double quotes.

quoted(Dcg) -->
  quoted(double_quote, Dcg).


%! quoted(:Quote_0, :Dcg_0)// .
% Wrapper around quoted//3 using singular occurrences of Quote_0.

quoted(Quote_0, Dcg_0) -->
  quoted(1, Quote_0, Dcg_0).


%! quoted(?Length:positive_integer, :Quote_0, :Dcg_0)// .
% Typical values for Quote:
%   - `double_quote//0`
%   - `single_quote//0`

quoted(Length, Quote_0, Dcg_0) -->
  dcg_between('#'(Length, Quote_0, []), Dcg_0).
