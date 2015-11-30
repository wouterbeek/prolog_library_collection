:- module(
  rfc2396_token,
  [
    domainlabel//1, % ?DomainLabel:string
    fragment//1, % ?Fragment:string
    'IPv4address'//1, % ?Address:list(nonneg)
    opaque_part//1, % ?OpaquePart:string
    param//1, % ?Parameter:string
    port//1, % ?Port:nonneg
    query//1, % ?Query:string
    reg_name//1, % ?RegisteredName:string
    rel_segment//1, % ?Segment:string
    scheme//1, % ?Scheme:string
    toplabel//1, % ?TopLabel:string
    userinfo//1 % ?UserInfo:string
  ]
).

/** <module> RFC 2396: Tokens

@author Wouter Beek
@compat RFC 2396
@deprecated
@version 2015/11
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_rfc)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(uri/rfc2396_code)).





%! domainlabel(DomainLabel:string)// .
% ```abnf
% domainlabel = alphanum | alphanum *( alphanum | "-" ) alphanum
% ```

domainlabel(S) --> dcg_string(domainlabel_codes, S).
domainlabel_codes(L) -->
  alphadigit(H),
  (   *(alphadigit_hyphen, T),
      alphadigit(X)
  ->  {append([H|T], [X], L)}
  ;   {L = [H]}
  ).



%! fragment(?Fragment:string)// .
% ```abnf
% fragment = *uric
% ```

fragment(S) --> *(uric, Cs), {string_codes(S, Cs)}.



%! 'IPv4address'(?Address:list(nonneg))// .
% ```abnf
% IPv4address = 1*digit "." 1*digit "." 1*digit "." 1*digit
% ```

'IPv4address'([N1,N2,N3,N4]) -->
  '+digit'(N1), ".", '+digit'(N2), ".", '+digit'(N3), ".", '+digit'(N4), ".".



%! opaque_part(OpaquePart:string)// .
% ```abnf
% opaque_part = uric_no_slash *uric
% ```

opaque_part(S) --> uric_no_slash(H), *(uric, T), {string_codes(S, [H|T])}.



%! param(?Parameter:string)// .
% ```abnf
% param = *pchar
% ```

param(S) --> *(pchar, Cs), {string_codes(S, Cs)}.



%! port(?Port:nonneg)// .
% ````abnf
% port = *digit
% ```

port(I) --> '*digit'(I).



%! query(?Query:string)// .
% ```abnf
% query = *uric
% ```

query(S) --> *(uric, Cs), {string_codes(S, Cs)}.



%! reg_name(S:string)// .
% ```abnf
% reg_name = 1*( unreserved | escaped | "$" | ","
%          | ";" | ":" | "@" | "&" | "=" | "+" )
% ```

reg_name(S) --> +(reg_name_code, Cs), {string_codes(S, Cs)}.
reg_name_code(C)   --> unreserved(C).
reg_name_code(C)   --> escaped(C).
reg_name_code(0'$) --> "$".
reg_name_code(0',) --> ",".
reg_name_code(0';) --> ";".
reg_name_code(0':) --> ":".
reg_name_code(0'@) --> "@".
reg_name_code(0'&) --> "&".
reg_name_code(0'=) --> "=".
reg_name_code(0'+) --> "+".



%! rel_segment(?Segment:string)// .
% ```abnf
% rel_segment = 1*( unreserved | escaped
%             | ";" | "@" | "&" | "=" | "+" | "$" | "," )
% ```

rel_segment(S) --> +(rel_segment_code, Cs), {string_codes(S, Cs)}.
rel_segment_code(C)   --> unreserved(C).
rel_segment_code(C)   --> escaped(C).
rel_segment_code(0';) --> ";".
rel_segment_code(0'@) --> "@".
rel_segment_code(0'&) --> "&".
rel_segment_code(0'=) --> "=".
rel_segment_code(0'+) --> "+".
rel_segment_code(0'$) --> "$".
rel_segment_code(0',) --> ",".



%! scheme(?Scheme:string)// .
% ```abnf
% scheme = alpha *( alpha | digit | "+" | "-" | "." )
% ```

scheme(S) --> *(scheme_code, Cs), {string_codes(S, Cs)}.
scheme_code(C)   --> alphadigit(C).
scheme_code(0'+) --> "+".
scheme_code(0'-) --> "-".
scheme_code(0'.) --> ".".



%! toplabel(?TopLabel:string)// .
% ```abnf
% toplabel = alpha | alpha *( alphanum | "-" ) alphanum
% ```

toplabel(S) -->
  alpha(H),
  (   *(alphadigit_hyphen, T), alphadigit(X)
  ->  {append([H|T], [X], Cs)}
  ;   {Cs = [H]}
  ),
  {string_codes(S, Cs)}.



%! userinfo(S:string)// .
% ```abnf
% userinfo = *( unreserved | escaped
%          | ";" | ":" | "&" | "=" | "+" | "$" | "," )
% ```

userinfo(S) --> *(userinfo_code, Cs), {string_codes(S, Cs)}.
userinfo_code(C)   --> unreserved(C).
userinfo_code(C)   --> escaped(C).
userinfo_code(0';) --> ";".
userinfo_code(0':) --> ":".
userinfo_code(0'&) --> "&".
userinfo_code(0'=) --> "=".
userinfo_code(0'+) --> "+".
userinfo_code(0'$) --> "$".
userinfo_code(0',) --> ",".
