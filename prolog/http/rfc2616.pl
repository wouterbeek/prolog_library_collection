:- module(
  rfc2616,
  [
    http_URL//1,        % -Uri:compound
    'LWS'//0,
    'quoted-string'//1, % -String:atom
    'rfc1123-date'//1,  % -Lex:atom
    token//1,           % -Token:atom
    value//1            % -Val:atom
  ]
).
:- reexport(library(dcg/rfc2234), [
     'ALPHA'//1,        % ?Code
     'CHAR'//1,         % ?Code
     'CRLF'//0,
     'CTL'//1,          % ?Code
     'DIGIT'//1,        % ?Digit:between(0,9)
     'DIGIT'//2,        % ?Digit:between(0,9)
                        % ?Code
     'HTAB'//0 as 'HT',
     'HTAB'//1 as 'HT', % ?Code
     'OCTET'//1,        % ?Code
     'SP'//0,
     'SP'//1            % ?Code
   ]).
:- reexport(library(uri/rfc1738), [
     hialpha//1 as 'HIALPHA', % ?Code
     lowalpha//1 as 'LOALPHA' % ?Code
   ]).

/** <module> RFC 2616: HTTP 1.1

@author Wouter Beek
@compat RFC 2616
@see https://tools.ietf.org/html/rfc2616
@version 2015/12, 2016/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/http_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(semweb/rdf11) ,[
     op(650, xfx, ^^)
   ]).
:- use_module(library(uri/rfc2396), [
     abs_path//1, % -Path:list(atom)
     host//1,     % -Host
     port//1,     % -Port:nonneg
     query//1     % -Query:atom
   ]).
:- use_module(library(uri/uri_ext)).





%! date1(
%!   -Year:between(0,99),
%!   -Month:between(1,12),
%!   -Day:between(0,9999)
%! )// is det.
%
% ```abnf
% date1 = 2DIGIT SP month SP 4DIGIT
% ```

date1(Y, Mo, D) -->
  #(2, 'DIGIT', Ds1),
  {pos_sum(Ds1, D)},
  'SP',
  month(Mo),
  'SP',
  #(4, 'DIGIT', Ds2),
  {pos_sum(Ds2, Y)}.



%! date2(
%!   -Year:between(0,99),
%!   -Month:between(1,12),
%!   -Day:between(0,9999)
%! )// is det.
%
% ```abnf
% date2 = 2DIGIT "-" month "-" 2DIGIT
% ```

date2(Y, Mo, D) -->
  #(2, 'DIGIT', Ds1),
  {pos_sum(Ds1, D)},
  "-",
  month(Mo),
  "-",
  #(2, 'DIGIT', Ds2),
  {pos_sum(Ds2, Y)}.



%! http_URL(-Uri:compound)// is det.
%
% ```abnf
% http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
% ```

http_URL(uri(http,auth(_,Host,Port),Segments,QueryComps)) -->
  atom_ci('http://'),
  host(Host),
  (":" -> port(Port) ; {http_default_port(http, Port)}),
  (abs_path(Segments) -> ("?" -> query(QueryComps) ; "") ; "").



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



%! month(-Month:between(1,12))// is det.
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



%! 'quoted-string'(-String:atom)// is det.
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



%! 'rfc850-date'(-Lit:rdf_literal)// is det.
%
% ```abnf
% rfc850-date = weekday "," SP date2 SP time SP "GMT"
% ```

'rfc850-date'(Lex^^DatatypeUri) -->
  weekday(D),
  ",",
  'SP',
  date2(Y, Mo, D),
  'SP',
  time(H, Mi, S),
  'SP',
  atom_ci('GMT'),
  {
    rdf_equal(xsd:dateTime, DatatypeUri),
    xsd_time_string(date_time(Y,Mo,D,H,Mi,S), DatatypeUri, Lex)
  }.



%! 'rfc1123-date'(-Lex:atom)// is det.
%
% ```abnf
% rfc1123-date = wkday "," SP date1 SP time SP "GMT"
% ```

'rfc1123-date'(Lex) -->
  wkday(_),
  ",",
  'SP',
  date1(Y, Mo, D),
  'SP',
  time(H, Mi, S),
  'SP',
  atom_ci('GMT'),
  {
    rdf_equal(xsd:dateTime, Type),
    xsd_time_string(date_time(Y,Mo,D,H,Mi,S), Type, Lex)
  }.



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

'TEXT'(C) --> 'OCTET'(C), {\+ 'CTL'(C, _, _)}.



%! time(-H, -Mi, -S)// is det.
%
% ```abnf
% time = 2DIGIT ":" 2DIGIT ":" 2DIGIT
% ```

time(H, Mi, S) -->
  #(2, 'DIGIT', Ds1),
  {pos_sum(Ds1, H)},
  ":",
  #(2, 'DIGIT', Ds2),
  {pos_sum(Ds2, Mi)},
  ":",
  #(2, 'DIGIT', Ds3),
  {pos_sum(Ds3, S)}.



%! token(-Token:atom)// is det.
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



%! value(-Val:atom)// is det.
%
% ```abnf
% value = token | quoted-string
% ```

value(Token) -->
  token(Token), !.
value(Str) -->
  'quoted-string'(Str).



%! weekday(-Day:between(1,7))// is det.
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



%! wkday(-Day:between(1,7))// is det.
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
