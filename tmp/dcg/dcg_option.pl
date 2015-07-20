:- module(
  dcg_option,
  [
    if_option//3, % +Option:nvpair
                  % +Options:list(nvpair)
                  % :Dcg
    if_select_option//4 % +Option:nvpair
                        % +Options:list(nvpair)
                        % -RestOptions:list(nvpair)
                        % :Dcg
  ]
).

/** <module> DCG option

Handling of options for DCG rules.

This module is intended to 

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(option)).

:- meta_predicate(if_option(+,+,//,?,?)).
:- meta_predicate(if_select_option(+,+,-,//,?,?)).



%! if_option(+Option:nvpair, +Options:list(nvpair), :Dcg)// is det.

if_option(Option, Options, Dcg) -->
  {option(Option, Options)}, !,
  Dcg.
if_option(_, _, _) --> [].


%! if_select_option(
%!   +Option:nvpair,
%!   +Options:list(nvpair),
%!   -RestOptions:list(nvpair),
%!   :Dcg
%! )// is det.

if_select_option(Option, Options1, Options2, Dcg) -->
  {select_option(Option, Options1, Options2)}, !,
  Dcg.
if_select_option(_, Options, Options, _) --> [].
