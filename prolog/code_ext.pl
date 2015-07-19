:- module(
  code_ext,
  [
    code_ci/2, % +Code:nonneg
               % -CiCode:nonneg
    codes_replace/3, % +FromCodes:list(code)
                     % +Pairs:list(pair(code))
                     % +ToCodes:list(code)
    codes_replace/4, % +FromCodes:list(code)
                     % +From:code
                     % +To:code
                     % +ToCodes:list(code)
    codes_rm/3, % +FromCodes:list(code)
                % +Remove:code
                % -ToCodes:list(code)
    put_codes/1 % +Codes:list(code)
  ]
).

/** <module> Code extensions

Predicates for handling codes.

# Replace

Replacements in list of codes can be made using:
```prolog
phrase('*'(dcg_replace, [FromDCG|FromDCGs], [ToDCG|ToDCGs], []), In, Out)
```

# Split

Lists of codes can be splitted using:
```prolog
phrase(dcg_sep_list(:SeparatorDCG,-Sublists:list(list(code))), Codes)
```

---

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(apply)).
:- use_module(library(lists)).





%! code_ci(+Code:nonneg, -CiCode:nonneg) is nondet.
% Returns case-insensitive variants of the given code,
%  including the code itself.

% Lowercase is a case-insensitive variant of uppercase.
code_ci(Upper, Lower):-
  code_type(Upper, upper(Lower)).
% Uppercase is a case-insensitive variant of lowercase.
code_ci(Lower, Upper):-
  code_type(Lower, lower(Upper)).
% Every code is a case-insensitive variant of itself.
code_ci(Code, Code).



%! codes_replace(
%!   +FromCodes:list(code),
%!   +Pairs:list(pair(code)),
%!   +ToCodes:list(code)
%! ) is det.

codes_replace([], _, []):- !.
codes_replace([X|T1], Pairs, [Y|T2]):-
  member(X-Y, Pairs), !,
  codes_replace(T1, Pairs, T2).
codes_replace([H|T1], Pairs, [H|T2]):-
  codes_replace(T1, Pairs, T2).



%! codes_replace(
%!   +FromCodes:list(code),
%!   +From:code,
%!   +To:code,
%!   +ToCodes:list(code)
%! ) is det.
% @see Wrapper around codes_replace/3.

codes_replace(FromCodes, From, To, ToCodes):-
  codes_replace(FromCodes, [From-To], ToCodes).



%! codes_rm(
%!   +FromCodes:list(code),
%!   +Remove:list(code),
%!   -ToCodes:list(code)
%! ) is det.

codes_rm([], _, []):- !.
codes_rm([H|T1], Xs, T2):-
  member(H, Xs), !,
  codes_rm(T1, Xs, T2).
codes_rm([H|T1], Xs, [H|T2]):- !,
  codes_rm(T1, Xs, T2).



%! put_codes(+Codes:list(code)) is det.
%! put_codes(+Out:stream, +Codes:list(code)) is det.
% @see Wrapper around put_code/1 that works on lists of codes
%      and that can write to an arbitrary stream.

put_codes(Codes):-
  maplist(put_code, Codes).
