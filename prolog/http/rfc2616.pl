:- module(
  rfc2616,
  [
    http_URL//1,        % -Uri
    'LOALPHA'//1,       % ?Code
    'quoted-string'//1, % -String
    'rfc1123-date'//1,  % -Lex
    token//1,           % -Token
    'UPALPHA'//1        % ?Code
  ]
).
:- reexport(library(dcg/rfc2234), [
     'ALPHA'//1,           % ?Code
     'CHAR'//1,            % ?Code
     'CRLF'//0,
     'CTL'//1,             % ?Code
     'DIGIT'//1,           % ?Weight
     'DIGIT'//2,           % ?Weight, ?Code
     'HEXDIG'//1 as 'HEX', % ?Weight
     'HEXDIG'//2 as 'HEX', % ?Weight, ?Code
     'HTAB'//0 as 'HT',
     'HTAB'//1 as 'HT',    % ?Code
     'OCTET'//1,           % ?Code
     'SP'//0,
     'SP'//1               % ?Code
   ]).

/** <module> RFC 2616: HTTP 1.1

@author Wouter Beek
@compat RFC 2616
@see https://tools.ietf.org/html/rfc2616
@version 2017/05-2017/08
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(math_ext)).
:- use_module(library(uri/rfc2396), [
     abs_path//1, % -Path
     host//1,     % -Host
     port//1,     % -Port
     query//1     % -Query
   ]).





%! comment(-String)// .
%
% ```abnf
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ```

comment(String) -->
  comment_codes(Cs),
  {string_codes(String, Cs)}.

comment_codes(Cs) -->
  "(",
  comment_codes_inner(Cs),
  ")".

comment_codes_inner([H|T]) -->
  ctext(H), !,
  comment_codes_inner(T).
comment_codes_inner([H|T]) -->
  'quoted-pair'(H), !,
  comment_codes_inner(T).
comment_codes_inner(L) -->
  comment_codes(L), !.
comment_codes_inner([]) --> "".



%! ctext(-Atom)// .
%
% ```abnf
% ctext = <any TEXT excluding "(" and ")">
% ```

ctext(A) -->
  ctext_codes(Cs),
  {atom_codes(A, Cs)}.

ctext_codes([H|T]) -->
  'TEXT'(H),
  {\+ memberchk(H, [40,41])}, !,
  ctext_codes(T).
ctext_codes([]) --> "".



%! date1(-Year:between(0,99), -Month:between(1,12), -Day:between(0,9999))// .
%
% ```abnf
% date1 = 2DIGIT SP month SP 4DIGIT
% ```

date1(Y, Mo, D) -->
  #(2, 'DIGIT', Weights1),
  {integer_weights(D, Weights1)},
  'SP',
  month(Mo),
  'SP',
  #(4, 'DIGIT', Weights2),
  {integer_weights(Y, Weights2)}.



%! date2(-Year:between(0,99), -Month:between(1,12), -Day:between(0,9999))// .
%
% ```abnf
% date2 = 2DIGIT "-" month "-" 2DIGIT
% ```

date2(Y, Mo, D) -->
  #(2, 'DIGIT', Weights1),
  {integer_weights(D, Weights1)},
  "-",
  month(Mo),
  "-",
  #(2, 'DIGIT', Weights2),
  {integer_weights(Y, Weights2)}.



%! http_URL(-Uri:compound)// .
%
% ```abnf
% http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
% ```

http_URL(uri(http,auth(_,Host,Port),Segments,QueryComps)) -->
  atom_ci('http://'),
  host(Host),
  (":" -> port(Port) ; {uri:default_port(http, Port)}),
  (abs_path(Segments) -> ("?" -> query(QueryComps) ; "") ; "").



%! 'HTTP-Version'(-Version:pair(nonneg))// .
%
% ```abnf
% HTTP-Version   = "HTTP" "/" 1*DIGIT "." 1*DIGIT
% ```

'HTTP-Version'(Major-Minor) -->
  "HTTP/",
  +('DIGIT', Weights1),
  {integer_weights(Major, Weights1)},
  ".",
  +('DIGIT', Weights2),
  {integer_weights(Minor, Weights2)}.



%! 'LOALPHA'(?Code:code)// .

'LOALPHA'(C) -->
  [C],
  {between(0x61, 0x7A, C)}.



%! 'LWS'// .
%
% ```abnf
% LWS = [CRLF] 1*( SP | HT )
% ```

'LWS' -->
  ?('CRLF'),
  +(sp_or_ht0), !.

sp_or_ht0 --> 'SP'.
sp_or_ht0 --> 'HT'.



%! month(-Month:between(1,12))// .
%
% ```abnf
% month = "Jan" | "Feb" | "Mar" | "Apr"
%       | "May" | "Jun" | "Jul" | "Aug"
%       | "Sep" | "Oct" | "Nov" | "Dec"
% ```

month(1) --> atom_ci('Jan'), !.
month(2) --> atom_ci('Feb'), !.
month(3) --> atom_ci('Mar'), !.
month(4) --> atom_ci('Apr'), !.
month(5) --> atom_ci('May'), !.
month(6) --> atom_ci('Jun'), !.
month(7) --> atom_ci('Jul'), !.
month(8) --> atom_ci('Aug'), !.
month(9) --> atom_ci('Sep'), !.
month(10) --> atom_ci('Oct'), !.
month(11) --> atom_ci('Nov'), !.
month(12) --> atom_ci('Dec').



%! qdtext(?Code:code)// .
%
% ```abnf
% qdtext = <any TEXT except <">>
% ```

qdtext(C) -->
  'TEXT'(C),
  {C \== 0'"}.



%! 'quoted-pair'(?Code:code)// .
%
% ```abnf
% quoted-pair = "\" CHAR
% ```

'quoted-pair'(C) -->
  "\\",
  'CHAR'(C).



%! 'quoted-string'(-String:atom)// .
%
% ```abnf
% quoted-string = ( <"> *(qdtext | quoted-pair ) <"> )
% ```

'quoted-string'(Str) -->
  "\"",
  *(quoted_string_code, Cs), !,
  {atom_codes(Str, Cs)},
  "\"".

quoted_string_code(C) --> qdtext(C).
quoted_string_code(C) --> 'quoted-pair'(C).



%! 'rfc850-date'(-Datetime:compound)// .
%
% ```abnf
% rfc850-date = weekday "," SP date2 SP time SP "GMT"
% ```

'rfc850-date'(date_time(Y,Mo,D,H,Mi,S)) -->
  weekday(D),
  ",",
  'SP',
  date2(Y, Mo, D),
  'SP',
  time(H, Mi, S),
  'SP',
  atom_ci('GMT').



%! 'rfc1123-date'(-Datetime:compound)// .
%
% ```abnf
% rfc1123-date = wkday "," SP date1 SP time SP "GMT"
% ```

'rfc1123-date'(date_time(Y,Mo,D,H,Mi,S)) -->
  wkday(_),
  ",",
  'SP',
  date1(Y, Mo, D),
  'SP',
  time(H, Mi, S),
  'SP',
  atom_ci('GMT').



%! separators(?Code)// .
%
% ```abnf
% separators = "(" | ")" | "<" | ">" | "@" | "," | ";" | ":" | "\" | <">
%            | "/" | "[" | "]" | "?" | "=" | "{" | "}" | SP | HT
% ```

separators(0'() --> "(".
separators(0')) --> ")".
separators(0'<) --> "<".
separators(0'>) --> ">".
separators(0'@) --> "@".
separators(0',) --> ",".
separators(0';) --> ";".
separators(0':) --> ":".
separators(0'\\) --> "\\".
separators(0'") --> "\"". %"
separators(0'/) --> "/".
separators(0'[) --> "[".
separators(0']) --> "]".
separators(0'?) --> "?".
separators(0'=) --> "=".
separators(0'{) --> "{".
separators(0'}) --> "}".
separators(C) --> 'SP'(C).
separators(C) --> 'HT'(C).



%! 'TEXT'(?Code:code)// .
%
% ```abnf
% TEXT = <any OCTET except CTLs, but including LWS>
% ```

'TEXT'(C) -->
  'OCTET'(C),
  {\+ 'CTL'(C, _, _)}.



%! time(-H, -Mi, -S)// .
%
% ```abnf
% time = 2DIGIT ":" 2DIGIT ":" 2DIGIT
% ```

time(H, Mi, S) -->
  #(2, 'DIGIT', Weights1),
  {integer_weights(H, Weights1)},
  ":",
  #(2, 'DIGIT', Weights2),
  {integer_weights(Mi, Weights2)},
  ":",
  #(2, 'DIGIT', Weights3),
  {integer_weights(S, Weights3)}.



%! token(-Token:atom)// .
%
% ```abnf
% token = 1*<any CHAR except CTLs or separators>
% ```

token(Token) -->
  +(token_code, Cs), !,
  {atom_codes(Token, Cs)}.

token_code(C) -->
  'CHAR'(C),
  {
    \+ 'CTL'(C, _, _),
    \+ separators(C, _, _)
  }.



%! 'UPALPHA'(?Code:code)// .

'UPALPHA'(C) -->
  [C],
  {between(0x41, 0x5A, C)}.



%! value(-Value:atom)// .
%
% ```abnf
% value = token | quoted-string
% ```

value(Token) -->
  token(Token), !.
value(Str) -->
  'quoted-string'(Str).



%! weekday(-Day:between(1,7))// .
%
% ```abnf
% weekday = "Monday" | "Tuesday" | "Wednesday"
%         | "Thursday" | "Friday" | "Saturday" | "Sunday"
% ```

weekday(1) --> atom_ci('Monday'), !.
weekday(2) --> atom_ci('Tuesday'), !.
weekday(3) --> atom_ci('Wednesday'), !.
weekday(4) --> atom_ci('Thursday'), !.
weekday(5) --> atom_ci('Friday'), !.
weekday(6) --> atom_ci('Saturday'), !.
weekday(7) --> atom_ci('Sunday').



%! wkday(-Day:between(1,7))// .
%
% ```abnf
% wkday = "Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun"
% ```

wkday(1) --> atom_ci('Mon'), !.
wkday(2) --> atom_ci('Tue'), !.
wkday(3) --> atom_ci('Wed'), !.
wkday(4) --> atom_ci('Thu'), !.
wkday(5) --> atom_ci('Fri'), !.
wkday(6) --> atom_ci('Sat'), !.
wkday(7) --> atom_ci('Sun').
