:- module(
  rfc2616,
  [
    http_URL//1, % -Url:dict
    'LWS'//0,
    'quoted-string'//1, % -String:string
    'rfc1123-date'//1, % -Datetime:datetime
    token//1, % -Token:string
    value//1 % -Value:string
  ]
).
:- reexport(library(dcg/rfc2234), [
     'ALPHA'//1, % ?Code:code
     'CHAR'//1, % ?Code:code
     'CRLF'//0,
     'CTL'//1, % ?Code:code
     'DIGIT'//1, % ?Digit
     'DIGIT'//2, % ?Digit:between(0,9)
                 % ?Code:code
     'HTAB'//0 as 'HT',
     'HTAB'//1 as 'HT', % ?Code:code
     'OCTET'//1, % ?Code:code
     'SP'//0,
     'SP'//1 % ?Code:code
   ]).
:- reexport(library(url/rfc1738), [
     hialpha//1 as 'HIALPHA', % ?Code:code
     lowalpha//1 as 'LOALPHA' % ?Code:code
   ]).

/** <module> RFC 2616: HTTP 1.1

@author Wouter Beek
@compat RFC 2616
@see https://tools.ietf.org/html/rfc2616
@version 2015/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_atom)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(uri/rfc2396), [
     abs_path//1, % -Path:list(string)
     host//1, % -Host
     port//1, % -Port:nonneg
     query//1 % -Query:string
   ]).





%! date1(-Date:date)// is det.
% ```abnf
% date1 = 2DIGIT SP month SP 4DIGIT
% ```

date1(date(Y,Mo,D)) -->
  #(2, 'DIGIT', Ds1),
  {pos_sum(Ds1, D)},
  'SP',
  month(Mo),
  'SP',
  #(4, 'DIGIT', Ds2),
  {pos_sum(Ds2, Y)}.



%! date2(-Date:date)// is det.
% ```abnf
% date2 = 2DIGIT "-" month "-" 2DIGIT
% ```

date2(date(Y,Mo,D)) -->
  #(2, 'DIGIT', Ds1),
  {pos_sum(Ds1, D)},
  "-",
  month(Mo),
  "-",
  #(2, 'DIGIT', Ds2),
  {pos_sum(Ds2, Y)}.



%! http_URL(-Url:dict)// is det.
% ```abnf
% http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
% ```

http_URL(D) -->
  atom_ci('http://'),
  host(Host),
  (":" -> port(Port) ; ""),
  (abs_path(Path) -> ("?" -> query(Query) ; "") ; ""),
  {
    exclude(pair_has_var_value, [host-Host,port-Port,path-Path,query-Query,scheme-http], L),
    dict_pairs(D, uri, L)
  }.



%! 'LWS'// .
% ```abnf
% LWS = [CRLF] 1*( SP | HT )
% ```

'LWS' --> ?('CRLF'), +(('SP' ; 'HT')).



%! month(-Month:between(1,12))// is det.
% ```abnf
% month = "Jan" | "Feb" | "Mar" | "Apr"
%       | "May" | "Jun" | "Jul" | "Aug"
%       | "Sep" | "Oct" | "Nov" | "Dec"
% ```

month(1)  --> atom_ci('Jan'), !.
month(2)  --> atom_ci('Feb'), !.
month(3)  --> atom_ci('Mar'), !.
month(4)  --> atom_ci('Apr'), !.
month(5)  --> atom_ci('May'), !.
month(6)  --> atom_ci('Jun'), !.
month(7)  --> atom_ci('Jul'), !.
month(8)  --> atom_ci('Aug'), !.
month(9)  --> atom_ci('Sep'), !.
month(10) --> atom_ci('Oct'), !.
month(11) --> atom_ci('Nov'), !.
month(12) --> atom_ci('Dec').



%! qdtext(?Code:code)// .
% ```abnf
% qdtext = <any TEXT except <">>
% ```

qdtext(C) --> 'TEXT'(C), {C \== 0'"}.   %"



%! 'quoted-pair'(-Code:code)// .
% ```abnf
% quoted-pair = "\" CHAR
% ```

'quoted-pair'(C) --> "\\", 'CHAR'(C).



%! 'quoted-string'(-String:string)// is det.
% ```abnf
% quoted-string = ( <"> *(qdtext | quoted-pair ) <"> )
% ```

'quoted-string'(S) -->
  "\"",
  *(quoted_string_code, Cs),
  {string_codes(S, Cs)},
  "\"".
quoted_string_code(C) --> qdtext(C).
quoted_string_code(C) --> 'quoted-pair'(C).



%! 'rfc850-date'(?Datetime:datetime)// is det.
% ```abnf
% rfc850-date = weekday "," SP date2 SP time SP "GMT"
% ```

'rfc850-date'(datetime(Y,Mo,D,H,Mi,S,_)) -->
  weekday(D),
  ",",
  'SP',
  date2(date(Y,Mo,D)),
  'SP',
  time(time(H,Mi,S)),
  'SP',
  atom_ci('GMT').



%! 'rfc1123-date'(?Datetime:datetime)// is det.
% ```abnf
% rfc1123-date = wkday "," SP date1 SP time SP "GMT"
% ```

'rfc1123-date'(datetime(Y,Mo,D,H,Mi,S,_)) -->
  wkday(_),
  ",",
  'SP',
  date1(date(Y,Mo,D)),
  'SP',
  time(time(H,Mi,S)),
  'SP',
  atom_ci('GMT').



%! separators(-Code:code)// .
% ```abnf
% separators = "(" | ")" | "<" | ">" | "@" | "," | ";" | ":" | "\" | <">
%            | "/" | "[" | "]" | "?" | "=" | "{" | "}" | SP | HT
% ```

separators(0'()  --> "(".
separators(0'))  --> ")".
separators(0'<)  --> "<".
separators(0'>)  --> ">".
separators(0'@)  --> "@".
separators(0',)  --> ",".
separators(0';)  --> ";".
separators(0':)  --> ":".
separators(0'\\) --> "\\".
separators(0'")  --> "\"". %"
separators(0'/)  --> "/".
separators(0'[)  --> "[".
separators(0'])  --> "]".
separators(0'?)  --> "?".
separators(0'=)  --> "=".
separators(0'{)  --> "{".
separators(0'})  --> "}".
separators(C)    --> 'SP'(C).
separators(C)    --> 'HT'(C).



%! 'TEXT'(?Code:code)// .
% ```abnf
% TEXT = <any OCTET except CTLs, but including LWS>
% ```

'TEXT'(C) --> 'OCTET'(C), {\+ 'CTL'(C, _, _)}.



%! time(-Time:compound)// is det.
% ```abnf
% time = 2DIGIT ":" 2DIGIT ":" 2DIGIT
% ```

time(time(H,Mi,S)) -->
  #(2, 'DIGIT', Ds1),
  {pos_sum(Ds1, H)},
  ":",
  #(2, 'DIGIT', Ds2),
  {pos_sum(Ds2, Mi)},
  ":",
  #(2, 'DIGIT', Ds3),
  {pos_sum(Ds3, S)}.



%! token(-Token:string)// is det.
% ```abnf
% token = 1*<any CHAR except CTLs or separators>
% ```

token(S) --> +(token_code, Cs), {string_codes(S, Cs)}.
token_code(C) --> 'CHAR'(C), {\+ 'CTL'(C, _, _), \+ separators(C, _, _)}.



%! value(-Value:string)// is det.
% ```abnf
% value = token | quoted-string
% ```

value(S) --> token(S), !.
value(S) --> 'quoted-string'(S).



%! weekday(-Day:between(1,7))// is det.
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



%! wkday(=Day:between(1,7))// is det.
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
