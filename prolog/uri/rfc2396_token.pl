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

:- use_module(library(dcg/dcg_re)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(uri/rfc2396_code)).





%! domainlabel(DomainLabel:string)// .
% ```abnf
% domainlabel = alphanum | alphanum *( alphanum | "-" ) alphanum
% ```

domainlabel(S) --> dcg_string(domainlabel_codes, S).
domainlabel_codes([C]) --> alphanum(C).
domainlabel_codes(L) -->
  alphanum(H),
  alphanums(T),
  alphanum(X),
  {append([H|T], [X], L)}.



%! fragment(?Fragment:string)// .
% ```abnf
% fragment = *uric
% ```

fragment(S) --> dcg_string(fragment_codes, S).
fragment_codes([H|T]) --> uric(H), !, fragment_codes(T).
fragment_codes([])    --> "".



%! 'IPv4address'(?Address:list(nonneg))// .
% ```abnf
% IPv4address = 1*digit "." 1*digit "." 1*digit "." 1*digit
% ```

'IPv4address'([N1,N2,N3,N4]) -->
  '+DIGIT'(N1), ".",
  '+DIGIT'(N2), ".",
  '+DIGIT'(N3), ".",
  '+DIGIT'(N4), ".".



%! opaque_part(OpaquePart:string)// .
% ```abnf
% opaque_part = uric_no_slash *uric
% ```

opaque_part(S) --> dcg_string(opaque_part_codes1, S).
opaque_part_codes1([H|T]) --> uric_no_slash(H), !, opaque_part_codes2(T).
opaque_part_codes2([H|T]) --> uric(H),          !, opaque_part_codes2(T).
opaque_part_codes2([])    --> "".



%! param(?Parameter:string)// .
% ```abnf
% param = *pchar
% ```

param(S) --> dcg_string(param_codes, S).
param_codes([H|T]) --> pchar(H), !, param_codes(T).
param_codes([])    --> "".



%! port(?Port:nonneg)// .
% ````abnf
% port = *digit
% ```

port(I) --> '*DIGIT'(I).



%! query(?Query:string)// .
% ```abnf
% query = *uric
% ```

query(S) --> dcg_string(query_codes, S).
query_codes([H|T]) --> uric(H), !, query_codes(T).
query_codes([])    --> "".



%! reg_name(S:string)// .
% ```abnf
% reg_name = 1*( unreserved | escaped | "$" | ","
%          | ";" | ":" | "@" | "&" | "=" | "+" )
% ```

reg_name(S) --> dcg_string(reg_name_codes1, S).
reg_name_codes1([H|T]) --> reg_name_code(H),    reg_name_codes2(T).
reg_name_codes2([H|T]) --> reg_name_code(H), !, reg_name_codes2(T).
reg_name_codes2([])    --> "".
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

rel_segment(S) --> dcg_string(rel_segment_codes, S).
rel_segment_codes([H|T]) --> rel_segment_code(H), rel_segment_codes(T).
rel_segment_codes([H]) --> rel_segment_code(H).
rel_segment_code(C) --> unreserved(C).
rel_segment_code(C) --> escaped(C).
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

scheme(S) --> dcg_string(scheme_codes1, S).
scheme_codes1([H|T])   --> alpha(H),    scheme_codes2(T).
scheme_codes2([H|T])   --> digit(H), !, scheme_codes2(T).
scheme_codes2([0'+|T]) --> "+",      !, scheme_codes2(T).
scheme_codes2([0'-|T]) --> "-",      !, scheme_codes2(T).
scheme_codes2([0'.|T]) --> ".",      !, scheme_codes2(T).
scheme_codes2([])      --> "".



%! toplabel(?TopLabel:string)// .
% ```abnf
% toplabel = alpha | alpha *( alphanum | "-" ) alphanum
% ```

toplabel(S) --> dcg_string(toplabel_codes, S).
toplabel_codes([H]) --> alpha(H).
toplabel_codes(L)   -->
  alpha(H),
  alphanums(T),
  alphanum(X),
  {append([H|T], [X], L)}.



%! userinfo(S:string)// .
% ```abnf
% userinfo = *( unreserved | escaped
%          | ";" | ":" | "&" | "=" | "+" | "$" | "," )
% ```

userinfo(S) --> dcg_string(userinfo_codes, S).
userinfo_codes([H|T])   --> unreserved(H), !, userinfo_codes(T).
userinfo_codes([H|T])   --> escaped(H),    !, userinfo_codes(T).
userinfo_codes([0';|T]) --> ";",           !, userinfo_codes(T).
userinfo_codes([0':|T]) --> ":",           !, userinfo_codes(T).
userinfo_codes([0'&|T]) --> "&",           !, userinfo_codes(T).
userinfo_codes([0'=|T]) --> "=",           !, userinfo_codes(T).
userinfo_codes([0'+|T]) --> "+",           !, userinfo_codes(T).
userinfo_codes([0'$|T]) --> "$",           !, userinfo_codes(T).
userinfo_codes([0',|T]) --> ",",           !, userinfo_codes(T).
userinfo_codes([])      --> "".





% HELPERS %

alphanums([H|T])   --> alphanum(H), !, alphanums(T).
alphanums([0'-|T]) --> "-",         !, alphanums(T).
alphanums([])      --> "".
