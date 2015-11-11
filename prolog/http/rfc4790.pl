:- module(
  rfc4790,
  [
    ascii_casemap/2, % +X:list(code)
                     % -Y:list(code)
    octet_match/2, % +X:list(between(0,255))
                   % +Y:list(between(0,255))
    octet_order/3, % +X:list(between(0,255))
                   % +Y:list(between(0,255))
                   % +Order:oneof([<,=,>])
    octet_substring/2 % ?X:list(between(0,255))
                      % +Y:list(between(0,255))
  ]
).

/** <module> RFC 4790: Internet Application Protocol Collation Registry

@author Wouter Beek
@compat RFC 4790
@see http://tools.ietf.org/html/rfc4790
@version 2015/11
*/




%! ascii_casemap(+X:list(code), +Y:list(code)) is semidet.
%! ascii_casemap(+X:list(code), -Y:list(code)) is det.
% Its equality, ordering, and substring operations are as for i;octet,
% except that at first, the lower-case letters (octet values 97-122) in
% each input string are changed to upper case (octet values 65-90).

ascii_casemap(L1, L2):-
  maplist(ascii_casemap_code, L1, L2).
ascii_casemap_code(X, Y):-
  between(97, 122, X), !,
  Y is X - 32.
ascii_casemap_code(X, X).



%! octet_match(+X:list(between(0,255)), +Y:list(between(0,255))) is semidet.

octet_match(X, Y):-
  octet_order(X, Y, =).



%! octet_order(+X:list(between(0,255)), +Y:list(between(0,255)), +Order:oneof([<,=,>])) is semidet.
%! octet_order(+X:list(between(0,255)), +Y:list(between(0,255)), -Order:oneof([<,=,>])) is det.
% The "i;octet" collation is a simple and fast collation intended for
% use on binary octet strings rather than on character data.
%
% The ordering algorithm is as follows:
%   1.  If both strings are the empty string, return the result "equal".
%   2.  If the first string is empty and the second is not, return the
%       result "less".
%   3.  If the second string is empty and the first is not, return the
%       result "greater".
%   4.  If both strings begin with the same octet value, remove the first
%       octet from both strings and repeat this algorithm from step 1.
%   5.  If the unsigned value (0 to 255) of the first octet of the first
%       string is less than the unsigned value of the first octet of the
%       second string, then return "less".
%   6.  If this step is reached, return "greater".

octet_order([], [], =):- !.
octet_order([], _, <):- !.
octet_order(_, [], >):- !.
octet_order([H|T1], [H|T2], Comp):- !,
  octet_order(T1, T2, Comp).
octet_order([H1|_], [H2|_], <):-
  H1 < H2, !.
octet_order(_, _, >).



%! octet_substring(+X:list(between(0,255)), +Y:list(between(0,255))) is semidet.
%! octet_substring(-X:list(between(0,255)), +Y:list(between(0,255))) is multi.
% The substring operation returns "match" if the first string is the
% empty string, or if there exists a substring of the second string of
% length equal to the length of the first string, which would result in
% a "match" result from the equality function.  Otherwise, the
% substring operation returns "no-match".

octet_substring([H|T1], [H|T2]):-
  octet_substring_found(T1, T2), !.
octet_substring([], _):- !.
octet_substring(L, [_|T]):-
  octet_substring(L, T).

octet_substring_found([H|T1], [H|T2]):- !,
  octet_substring_found(T1, T2).
octet_substring_found([], _).
