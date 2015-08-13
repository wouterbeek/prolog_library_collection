:- module(
  dcg_option,
  [
    if_option//3, % +Option:compound
                  % +Options:list(compound)
                  % :Dcg
    if_select_option//4 % +Option:compound
                        % +Options:list(compound)
                        % -RestOptions:list(compound)
                        % :Dcg
  ]
).

/** <module> DCG option

Handling of options for DCG rules.

This module is intended to 

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(option)).

:- meta_predicate(if_option(+,+,//,?,?)).
:- meta_predicate(if_select_option(+,+,-,//,?,?)).





%! if_option(+Option:compound, +Options:list(compound), :Dcg_0)// is det.

if_option(Opt, Opts, Dcg_0) -->
  {option(Opt, Opts)}, !,
  Dcg_0.
if_option(_, _, _) --> [].



%! if_select_option(
%!   +Option:compound,
%!   +Options:list(compound),
%!   -RestOptions:list(compound),
%!   :Dcg_0
%! )// is det.

if_select_option(Opt, Opts1, Opts2, Dcg_0) -->
  {select_option(Opt, Opts1, Opts2)}, !,
  Dcg_0.
if_select_option(_, Opts, Opts, _) --> [].
