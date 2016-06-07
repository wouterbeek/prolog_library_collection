:- module(
  dcg_code,
  [
    between_code//2,     % +Low, +High
    between_code//3,     % +Low, +High, ?C
    between_code_rad//2, % +RadLow, +RadHigh
    between_code_rad//3, % +RadLow, +RadHigh, -C
    code//1,             % ?C
    code_ci//1,          % ?C
    code_lower//1,       % ?C
    code_rad//1,         % ?RadC
    code_rad//2,         % ?RadC, -C
    code_upper//1        % ?C
  ]
).

/** <module> DCG code

DCG support for entering character codes.

@author Wouter Beek
@version 2015/07-2015/08, 2016/06
*/

:- use_module(library(code_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(math/radconv)).





%! between_code(+Low, +High)// .
%! between_code(+Low, +High, ?C)// .

between_code(Low, High) -->
  between_code(Low, High, _).


between_code(Low, High, C) -->
  [C],
  {between(Low, High, C)}.



%! between_code_rad(+Low, +High)// .
%! between_code_rad(+Low, +High, -C)// .
%
% Parses or generates a code between the given numbers.

between_code_rad(Low, High) -->
  between_code_rad(Low, High, _).


between_code_rad(dec(Low), dec(High), C) --> !,
  between_code(Low, High, C).
between_code_rad(Low1, High1, C) -->
  {
    radconv(Low1, dec(Low2)),
    radconv(High1, dec(High2))
  },
  between_code_rad(dec(Low2), dec(High2), C).



%! code(?C)// .
%
% Useful in meta-predicates.

code(C) -->
  [C].



%! code_ci(+C)// is multi.
%! code_ci(-C)// is nondet.
% Writes case-insensitive variants of the given code.
% Generates the upper- and lowercase variants of a given letter
% (in no particular order).
%
% ```prolog
% ?- phrase(code_ci(66), Cs).
% Cs = "b" ;
% Cs = "B".
% ?- phrase(code_ci(98), Cs).
% Cs = "B" ;
% Cs = "b".
%
% ```
%
% Parses letters returning the chracter codes of their
% lower- and upper-case variants (in no particular order).
%
% ```prolog
% ?- phrase(code_ci(X), `b`).
% X = 66 ;
% X = 98.
% ?- phrase(code_ci(X), `B`).
% X = 98 ;
% X = 66.
% ```
%
% This can be used to process all case-variants of a given string.
%
% ```prolog
% ?- phrase(*(code_ci, `http`, []), Cs).
% Cs = "HTTP" ;
% Cs = "HTTp" ;
% Cs = "HTtP" ;
% Cs = "HTtp" ;
% Cs = "HtTP" ;
% Cs = "HtTp" ;
% Cs = "HttP" ;
% Cs = "Http" ;
% Cs = "hTTP" ;
% Cs = "hTTp" ;
% Cs = "hTtP" ;
% Cs = "hTtp" ;
% Cs = "htTP" ;
% Cs = "htTp" ;
% Cs = "httP" ;
% Cs = "http" ;
% false.
% ```
%
% The latter comes close to using atom_ci//1.

code_ci(C) -->
  {var(C)}, !,
  [C0],
  {code_ci(C0, C)}.
code_ci(C) -->
  {code_ci(C, C0)},
  [C0].



%! code_lower(+C)// is det.
%! code_lower(-C)// is nondet.
%
% Parses letters and returns their lower-case character code.
%
% ```prolog
% ?- phrase(code_lower(X), `A`).
% X = 97.
% ?- phrase(code_lower(X), `a`).
% X = 97.
% ```
%
% Generates the lower-case letter that is identical to the given
% lower-case letter or that is the lower-case variant of the given
% upper-case letter.
%
% ```prolog
% ?- phrase(code_lower(65), Cs).
% Cs = "a".
% ?- phrase(code_lower(97), Cs).
% Cs = "a".
% ```

code_lower(Lower) -->
  parsing, !,
  [C],
  {code_type(C, upper(Lower))}.
code_lower(C) -->
  {to_lower(C, Lower)},
  [Lower].



%! code_rad(+RadC)// .
%! code_rad(+RadC, -C)// .
%
% Emits a single code and allows the code to be represented in one of
% the following bases:
%
%   * bin(+nonneg)
%
%   * dec(+nonneg)
%
%   * hex(+atom)
%
%   * oct(+nonneg)

code_rad(RadC) -->
  code_rad(RadC, _).


code_rad(RadC, C) -->
  {var(RadC)}, !,
  [C],
  {radconv(RadC, dec(C))}.
code_rad(RadC, C) -->
  {radconv(RadC, dec(C))},
  [C].



%! code_upper(+C)// is det.
%! code_upper(-C)// is nondet.
% Parses upper-case letters and returns their lower- and upper-case
% character code (in that order).
%
% ```prolog
% ?- phrase(code_upper(X), `A`).
% X = 65 ;
% ?- phrase(code_upper(X), `a`).
% X = 65.
% ```
%
% Generates the upper-case letter that is identical to the given
% upper-case letter or that is the upper-case version of the given
% lower-case letter.
%
% ```prolog
% ?- phrase(code_upper(65), Cs).
% Cs = "A".
% ?- phrase(code_upper(97), Cs).
% Cs = "A".
% ```

code_upper(Upper) -->
  parsing, !,
  [C],
  {code_type(C, upper(Upper))}.
code_upper(C) -->
  {to_upper(C, Upper)},
  [Upper].
