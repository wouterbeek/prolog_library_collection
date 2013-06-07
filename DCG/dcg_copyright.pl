:- module(
  dcg_copyright,
  [
    copyright//2 % -Holders:list(atom)
                 % -Year:oneof([integer,pair(integer)])
  ]
).

/** <module> DCG_COPYRIGHT

DCGs for parsing copyright information.

@author Wouter Beek
@version 2013/06
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_year)).
:- use_module(library(dcg/basics)).



copyright(Holders, Year) -->
  (copyright, blank ; ""),
  year(_Lang, Year), blank,
  holders(Holders).

holders([H|T]) --> middle_holder(H), holders(T).
% Note that the last holders must occur after the
% the middle holders!
holders([H]) --> last_holder(H).

middle_holder(H) -->
  dcg_until_atom((blank, separator), H),
  blank, separator, blank.
%middle_holder(H) --> dcg_until_atom(, H), blank, ampersand, blank.

last_holder(H) --> dcg_all_atom(H).

% Separators:
%   * " /"
%   * " &"
separator -->
  forward_slash.
separator -->
  ampersand.

