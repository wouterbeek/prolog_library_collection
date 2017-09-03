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
@version 2017/05-2017/09
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





%! comment(?Comment:atom)// .
%
% ```abnf
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ```

comment(Comment) -->
  dcg_atom(comment_codes, Comment).

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



%! ctext(?Text:atom)// .
%
% ```abnf
% ctext = <any TEXT excluding "(" and ")">
% ```

ctext(Text) -->
  dcg_atom(ctext_codes, Text).

ctext_codes([H|T]) -->
  'TEXT'(H),
  {\+ memberchk(H, [40,41])}, !,
  ctext_codes(T).
ctext_codes([]) --> "".



%! date1(?Year:between(0,99), ?Month:between(1,12), ?Day:between(0,9999))// .
%
% ```abnf
% date1 = 2DIGIT SP month SP 4DIGIT
% ```

date1(Year, Month, Day) -->
  dcg_integer(#(2, 'DIGIT'), Day),
  'SP',
  month(Month),
  'SP',
  dcg_integer(#(4, 'DIGIT'), Year).



%! date2(?Year:between(0,99), ?Month:between(1,12), ?Day:between(0,9999))// .
%
% ```abnf
% date2 = 2DIGIT "-" month "-" 2DIGIT
% ```

date2(Year, Month, Day) -->
  dcg_integer(#(2, 'DIGIT'), Day),
  "-",
  month(Month),
  "-",
  dcg_integer(#(2, 'DIGIT'), Year).



%! http_URL(-Uri:compound)// .
%
% ```abnf
% http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
% ```

http_URL(uri(http,auth(_User,_Password,Host,Port),Segments,QueryComps)) -->
  atom_ci('http://'),
  host(Host),
  (":" -> port(Port) ; {Port = 80}),
  (abs_path(Segments) -> ("?" -> query(QueryComps) ; "") ; "").



%! 'HTTP-Version'(?Version:pair(nonneg))// .
%
% ```abnf
% HTTP-Version   = "HTTP" "/" 1*DIGIT "." 1*DIGIT
% ```

'HTTP-Version'(Major-Minor) -->
  "HTTP/",
  dcg_integer(+('DIGIT'), Major),
  ".",
  dcg_integer(+('DIGIT'), Minor).



%! 'LOALPHA'(?Code:code)// .

'LOALPHA'(Code) -->
  dcg_between(0x61, 0x7A, Code).



%! 'LWS'// .
%
% ```abnf
% LWS = [CRLF] 1*( SP | HT )
% ```

'LWS' -->
  ?('CRLF'),
  +(sp_or_ht_).

sp_or_ht_ --> 'SP'.
sp_or_ht_ --> 'HT'.



%! month(?Month:between(1,12))// .
%
% ```abnf
% month = "Jan" | "Feb" | "Mar" | "Apr"
%       | "May" | "Jun" | "Jul" | "Aug"
%       | "Sep" | "Oct" | "Nov" | "Dec"
% ```

month(1) --> atom_ci('Jan').
month(2) --> atom_ci('Feb').
month(3) --> atom_ci('Mar').
month(4) --> atom_ci('Apr').
month(5) --> atom_ci('May').
month(6) --> atom_ci('Jun').
month(7) --> atom_ci('Jul').
month(8) --> atom_ci('Aug').
month(9) --> atom_ci('Sep').
month(10) --> atom_ci('Oct').
month(11) --> atom_ci('Nov').
month(12) --> atom_ci('Dec').



%! qdtext(?Code:code)// .
%
% ```abnf
% qdtext = <any TEXT except <">>
% ```

qdtext(Code) -->
  'TEXT'(Code),
  {Code \== 0'"}.



%! 'quoted-pair'(?Code:code)// .
%
% ```abnf
% quoted-pair = "\" CHAR
% ```

'quoted-pair'(Code) -->
  "\\",
  'CHAR'(Code).



%! 'quoted-string'(?String:atom)// .
%
% ```abnf
% quoted-string = ( <"> *(qdtext | quoted-pair ) <"> )
% ```

'quoted-string'(String) -->
  "\"",
  dcg_atom(*('quoted-string_'), String),
  "\"".

'quoted-string_'(Code) --> qdtext(Code).
'quoted-string_'(Code) --> 'quoted-pair'(Code).



%! 'rfc850-date'(?Datetime:dt)// .
%
% ```abnf
% rfc850-date = weekday "," SP date2 SP time SP "GMT"
% ```

'rfc850-date'(dt(Year,Month,Day,Hour,Minute,Second)) -->
  weekday(Day),
  ",",
  'SP',
  date2(Year, Month, Day),
  'SP',
  time(Hour, Minute, Second),
  'SP',
  atom_ci('GMT').



%! 'rfc1123-date'(?Datetime:dt)// .
%
% ```abnf
% rfc1123-date = wkday "," SP date1 SP time SP "GMT"
% ```

'rfc1123-date'(dt(Year,Month,Day,Hour,Minute,Second)) -->
  wkday(_),
  ",",
  'SP',
  date1(Year, Month, Day),
  'SP',
  time(Hour, Minute, Second),
  'SP',
  atom_ci('GMT').



%! separators(?Code:code)// .
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
separators(Code) --> 'SP'(Code).
separators(Code) --> 'HT'(Code).



%! 'TEXT'(?Code:code)// .
%
% ```abnf
% TEXT = <any OCTET except CTLs, but including LWS>
% ```

'TEXT'(Code) -->
  'OCTET'(Code),
  {\+ 'CTL'(Code, _, _)}.



%! time(?Hour:between(0,99), ?Minute:between(0,99), ?Second:between(0,99))// .
%
% ```abnf
% time = 2DIGIT ":" 2DIGIT ":" 2DIGIT
% ```

time(Hour, Minute, Second) -->
  dcg_integer(#(2, 'DIGIT'), Hour),
  ":",
  dcg_integer(#(2, 'DIGIT'), Minute),
  ":",
  dcg_integer(#(2, 'DIGIT'), Second).



%! token(?Token:atom)// .
%
% ```abnf
% token = 1*<any CHAR except CTLs or separators>
% ```

token(Token) -->
  dcg_atom(+(token_), Token).

token_(Code) -->
  'CHAR'(Code),
  {
    \+ 'CTL'(Code, _, _),
    \+ separators(Code, _, _)
  }.



%! 'UPALPHA'(?Code:code)// .

'UPALPHA'(Code) -->
  dcg_between(0x41, 0x5A, Code).



%! value(?Value:atom)// .
%
% ```abnf
% value = token | quoted-string
% ```

value(Value) --> token(Value).
value(Value) --> 'quoted-string'(Value).



%! weekday(?Day:between(1,7))// .
%
% ```abnf
% weekday = "Monday" | "Tuesday" | "Wednesday"
%         | "Thursday" | "Friday" | "Saturday" | "Sunday"
% ```

weekday(1) --> atom_ci('Monday').
weekday(2) --> atom_ci('Tuesday').
weekday(3) --> atom_ci('Wednesday').
weekday(4) --> atom_ci('Thursday').
weekday(5) --> atom_ci('Friday').
weekday(6) --> atom_ci('Saturday').
weekday(7) --> atom_ci('Sunday').



%! wkday(?Day:between(1,7))// .
%
% ```abnf
% wkday = "Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun"
% ```

wkday(1) --> atom_ci('Mon').
wkday(2) --> atom_ci('Tue').
wkday(3) --> atom_ci('Wed').
wkday(4) --> atom_ci('Thu').
wkday(5) --> atom_ci('Fri').
wkday(6) --> atom_ci('Sat').
wkday(7) --> atom_ci('Sun').
