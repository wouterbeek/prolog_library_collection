:- module(
  dcg_century,
  [
    century//2, % ?Lang:atom
                % ?Century:integer
    century_interval//2 % ?Lang:atom
                        % ?Interval:pair(integer)
  ]
).

/** <module> DCG_YEAR

DCGs for parsing/generating century information.

@author Wouter Beek
@version 2013/05
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_ordinal)).
:- use_module(library(dcg/basics)).



%! century_interval(?Lang:atom, ?Century:integer)//
% Parses a century as an integer.

century(Lang, Century) -->
  (uncertainty(Lang), blank ; ""),
  (century_adjective(Lang), blank ; ""),
  ordinal(Lang, Century),
  blank,
  century_noun(Lang).

century_adjective(nl) --> "begin".
century_adjective(nl) --> "eind".

%! century_interval(?Lang:atom, ?Interval:pair(integer))//
% Parses a century as an interval delimited by years.

% A single century, pick the first and last year to delimit the interval.
century_interval(Lang, Interval) -->
  century_interval0(Lang, Interval).
% A pair of centuries, pick the first year of the former century
% and last year of the latter century to delimit the interval.
century_interval(Lang, Year1-Year2) -->
  ordinal(Lang, Century1),
  century_separator,
  century_interval0(Lang, Century2),
  {Year1 is Century1 * 100},
  {Year2 is Century2 * 100 + 99}.

century_interval0(Lang, Year1-Year2) -->
  century(Lang, Century),
  {Year1 is Century * 100},
  {Year2 is Year1 + 99}.

century_noun(nl) --> "eeuw".

century_separator --> forward_slash.
century_separator --> hyphen_minus.

