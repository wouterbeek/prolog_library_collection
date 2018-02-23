:- module(
  dcg_option,
  [
    dcg_if_option//3, % +Option:compound
                      % +Options:list(compound)
                      % :Dcg_2
    dcg_if_select_option//4 % +Option:compound
                            % +Options:list(compound)
                            % -RestOptions:list(compound)
                            % :Dcg_2
  ]
).

/** <module> DCG option

Handling of options for DCG rules.

This module is intended to 

@author Wouter Beek
@version 2015/08, 2015/10
*/

:- use_module(library(option)).

:- meta_predicate(dcg_if_option(+,+,//,?,?)).
:- meta_predicate(dcg_if_select_option(+,+,-,//,?,?)).





%! dcg_if_option(+Option:compound, +Options:list(compound), :Dcg_2)// is det.

dcg_if_option(Option, Options, Dcg_2) -->
  {option(Option, Options)}, !,
  Dcg_2.
dcg_if_option(_, _, _) --> [].



%! dcg_if_select_option(
%!   +Option:compound,
%!   +Options:list(compound),
%!   -RestOptions:list(compound),
%!   :Dcg_2
%! )// is det.

dcg_if_select_option(Opt, Opts1, Opts2, Dcg_2) -->
  {select_option(Opt, Opts1, Opts2)}, !,
  Dcg_2.
dcg_if_select_option(_, Opts, Opts, _) --> [].
