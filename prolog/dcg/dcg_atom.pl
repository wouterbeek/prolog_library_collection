:- module(
  dcg_atom,
  [
    atom_ci//1,        % ?A
    atom_ellipsis//2,  % +A, +Ellipsis:positive_integer
    atom_lower//1,     % ?A
    atom_lowercase//0,
    atom_title//1,     % ?A
    atom_upper//1,     % ?A
    atom_uppercase//0
  ]
).

/** <module> DCG atom

Grammar rules for processing atoms.

@author Wouter Beek
@version 2015/08, 2015/10, 2015/12, 2016/05-2016/06
*/

:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(url/rfc1738), [
     hialpha//1, % ?C
     lowalpha//1 % ?C
   ]).





%! atom_ci(?A)// .
%
% ```prolog
% ?- phrase(atom_ci(http), Cs).
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

atom_ci(A) -->
  parsing, !,
  *(code_ci, Cs),
  {atom_codes(A, Cs)}.
atom_ci(A) -->
  {atom_codes(A, Cs)},
  *(code_ci, Cs).



%! atom_ellipsis(+A, +Ellipsis:positive_integer)// .

atom_ellipsis(A, Ellipsis) -->
  {atom_truncate(A, Ellipsis, A0)},
  atom(A0).



%! atom_lower(?A)// .
%
% Generate/parse a lower-case atom, i.e. one consisting of all
% characters except for uppercase letters.

atom_lower(A) -->
  parsing, !,
  *(code_lower, Cs),
  {atom_codes(A, Cs)}.
atom_lower(A) -->
  {atom_codes(A, Cs)},
  *(code_lower, Cs).



%! atom_lowercase// .

atom_lowercase, [Low] -->
  [Up],
  {code_type(Low, to_lower(Up))}, !,
  rest.
atom_lowercase --> "".



%! atom_title(?A) // .

atom_title(A) -->
  generating, !,
  {atom_codes(A, [C|Cs])},
  hialpha(C),
  *(lowalpha, Cs).
atom_title(A) -->
  hialpha(C),
  *(lowalpha, Cs),
  {atom_codes(A, [C|Cs])}.
atom_title('') --> !, "".



%! atom_upper(?A)// .

atom_upper(A) -->
  parsing, !,
  *(code_upper, Cs),
  {atom_codes(A, Cs)}.
atom_upper(A) -->
  {atom_codes(A, Cs)},
  *(code_upper, Cs).



%! atom_uppercase// is det.

atom_uppercase, [Up] -->
  [Low],
  {code_type(Up, to_upper(Low))}, !,
  rest.
atom_uppercase --> "".
