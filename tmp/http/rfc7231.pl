:- module(
  rfc7231,
  [
    accept//1,         % ?Pairs
    allow//1,          % ?Methods
    'content-type'//1, % ?MediaType
    'HTTP-date'//1,    % ?Datetime
    'media-type'//1,   % ?MediaType
    server//1          % ?Server
  ]
).
:- reexport(library(dcg/rfc4647), [
     'language-range'//1
   ]).
:- reexport(library(dcg/rfc5322), [
     mailbox//1
   ]).
:- reexport(library(dcg/rfc5646), [
     'Language-Tag'//1 as 'language-tag'
   ]).

/** <module> RFC 7231 - HTTP/1.1: Semantics and Content

@author Wouter Beek
@compat RFC 7231
@see https://tools.ietf.org/html/rfc7231
@version 2017/05-2017/08
*/

:- use_module(library(dcg)).
:- use_module(library(http/rfc7230)).
:- use_module(library(math_ext)).
:- use_module(library(uri/rfc3986)).

:- discontiguous
    http:http_header/1,
    http:http_separable/1.

:- dynamic
    http:http_header/1,
    http:http_separable/1.

:- multifile
    http:http_header/1,
    http:http_separable/1.





%! accept(?Pairs:list(pair(between(0.0,1.0),compound)))// .
%
% ```abnf
% Accept = #( media-range [ accept-params ] )
% ```

http:http_header(accept).
http:http_separable(accept).
accept(MediaTypes) -->
  parsing, !,
  *##(accept_value_, Pairs), !,
  {sort(1, @>=, Pairs, MediaTypes)}.
accept(MediaTypes1) -->
  {
    MediaTypes2 = [media('*'/'*',[])|MediaTypes1],
    length(MediaTypes2, Len),
    Delta is (1.0 - 0.001) / Len,
    weight_pairs_(MediaTypes2, 0.001, Delta, Pairs)
  },
  *##(accept_value_, Pairs), !.

weight_pairs_([], _, _, []) :- !.
weight_pairs_([H|T1], N1, Len, [N1-H|T2]) :-
  N2 is N1 + Len,
  weight_pairs_(T1, N2, Len, T2).

accept_value_(Weight-MediaType) -->
  'media-range'(MediaType),
  (   'accept-params'(Weight, [])
  ->  ""
  ;   {Weight = 1.0}
  ).



%! 'accept-charset'(-Charsets:list(atom))// .
%
% ```abnf
% Accept-Charset = 1#( ( charset | "*" ) [ weight ] )
% ```

http:http_header('accept-charset').
http:http_separable('accept-charset').
'accept-charset'(Charsets) -->
  +##(accept_charset_value_, Pairs), !,
  {sort(1, @>=, Pairs, Charsets)}.

accept_charset_value_(Weight-Charset) -->
  ("*" -> "" ; charset(Charset)),
  '[weight]'(Weight).



%! 'accept-encoding'(-Encodings:list(atom))// .
%
% ```abnf
% Accept-Encoding = #( codings [ weight ] )
% ```

http:http_header('accept-encoding').
http:http_separable('accept-encoding').
'accept-encoding'(Encodings) -->
  *##(accept_encoding_value_, Pairs), !,
  {sort(1, @>=, Pairs, Encodings)}.

accept_encoding_value_(Weight-Encoding) -->
  codings(Encoding),
  '[weight]'(Weight).



%! 'accept-ext'(-Extension:or([atom,pair(atom)]))// .
%
% ```abnf
% accept-ext = OWS ";" OWS token [ "=" ( token | quoted-string ) ]
% ```

'accept-ext'(Extension) -->
  http_sep(0';),
  token(Key),
  (   "="
  ->  (token(Val), ! ; 'quoted-string'(Val)),
      {Extension = Key-Val}
  ;   {Extension = Key}
  ).



%! 'accept-language'(-LanguageRanges:list(list(atom)))// .
%
% ```abnf
% Accept-Language = 1#( language-range [ weight ] )
% ```

http:http_header('accept-language').
http:http_separable('accept-language').
'accept-language'(LRanges) -->
  +##(accept_language_, Pairs), !,
  {sort(1, @>=, Pairs, LRanges)}.

accept_language_(Weight-LRange) -->
  'language-range'(LRange),
  '[weight]'(Weight).



%! 'accept-params'(-Weight, -Extensions)// .
%
% ```abnf
% accept-params = weight *( accept-ext )
% ```

'accept-params'(Weight, Extensions) -->
  weight(Weight),
  *('accept-ext', Extensions).



%! allow(?Methods:list(atom))// is det.
%
% ```abnf
% Allow = #method
% ```

http:http_header(allow).
http:http_separable(allow).
allow(Methods) -->
  *##(method, Methods).



%! 'asctime-date'(?Datetime:dt)// is det.
%
% ```abnf
% asctime-date = day-name SP date3 SP time-of-day SP year
% ```

'asctime-date'(dt(Year,Month,Day,Hour,Minute,Second,0)) -->
  'day-name'(Day),
  'SP',
  date3(Month, Day),
  'SP',
  'time-of-day'(Hour, Minute, Second),
  'SP',
  year(Year).



%! charset(?Charset:atom)// .
%
% ```abnf
% charset = token
% ```

charset(Charset) -->
  token(Charset).



%! codings(?Coding:atom)// .
%
% ```abnf
% codings = content-coding | "identity" | "*"
% ```

codings(Coding) --> 'content-coding'(Coding).
codings(identity) --> atom_ci(identity).
codings(_) --> "*".



%! 'content-coding'(?Coding:atom)// .
%
% ```abnf
% content-coding = token
% ```

'content-coding'(Coding) -->
  token(Coding).



%! 'content-encoding'(-Encodings:list(atom))// is det.
%
% ```abnf
% Content-Encoding = 1#content-coding
% ```

http:http_header('content-encoding').
http:http_separable('content-encoding').
'content-encoding'(Encodings) -->
  +##('content-coding', Encodings), !.



%! 'content-language'(-LTags:list(list(atom)))// .
%
% ```abnf
% Content-Language = 1#language-tag
% ```
%
% Examples:
%
%   - `Content-Language: da`
%   - `Content-Language: mi, en`

http:http_header('content-language').
http:http_separable('content-language').
'content-language'(LTags) -->
  +##('language-tag', LTags), !.



%! 'content-location'(-Uri:compound)// .
%
% ```abnf
% Content-Location = absolute-URI | partial-URI
% ```

http:http_header('content-location').
'content-location'(Uri) --> 'absolute-URI'(Uri).
'content-location'(Uri) --> 'partial-URI'(Uri).



%! 'content-type'(-MediaType:compound)// .
%
% ```abnf
% Content-Type = media-type
% ```
%
% Example: `Content-Type: text/html; charset=ISO-8859-4`

http:http_header('content-type').
'content-type'(MediaType) -->
  'media-type'(MediaType).



%! date(?Datetime:dt)// is det.
%
% ```abnf
% Date = HTTP-date
% ```
%
% Example: `Date: Tue, 15 Nov 1994 08:12:31 GMT`

http:http_header(date).
date(Datetime) -->
  'HTTP-date'(Datetime).



%! date1(?Year:between(0,9999), ?Month:between(1,12), ?Day:between(0,99))// .
%
% ```abnf
% date1 = day SP month SP year
% ```
%
% Example: `02 Jun 1982`

date1(Year, Month, Day) -->
  day(Day),
  'SP',
  month(Month),
  'SP',
  year(Year).



%! date2(?Year:between(0,9999), ?Month:between(1,12), ?Day:beween(0,99))// .
%
% ```abnf
% date2 = day "-" month "-" 2DIGIT
% ```
%
% Example: `02-Jun-82`

date2(Year, Month, Day) -->
  day(Day), "-",
  month(Month), "-",
  dcg_integer(#(2, 'DIGIT'), Year).



%! date3(?Month:between(1,12), ?Day:between(0,99))// .
%
% ```abnf
% date3 = month SP ( 2DIGIT | ( SP 1DIGIT ))
% ```
%
% Example: `Jun  2`

date3(Month, Day) -->
  month(Month), 'SP',
  (   dcg_integer(#(2, 'DIGIT'), Day)
  ;   'SP',
      #(1, 'DIGIT', Day)
  ).



%! day(?Day:between(0,99))// .
%
% ```abnf
% day = 2DIGIT
% ```

day(Day) -->
  dcg_integer(#(2, 'DIGIT'), Day).



%! 'day-name'(?Day:between(1,7))// .
%
% ```abnf
% day-name = %x4D.6F.6E   ; "Mon", case-sensitive
%          | %x54.75.65   ; "Tue", case-sensitive
%          | %x57.65.64   ; "Wed", case-sensitive
%          | %x54.68.75   ; "Thu", case-sensitive
%          | %x46.72.69   ; "Fri", case-sensitive
%          | %x53.61.74   ; "Sat", case-sensitive
%          | %x53.75.6E   ; "Sun", case-sensitive
% ```

'day-name'(1) --> atom_ci('Mon').
'day-name'(2) --> atom_ci('Tue').
'day-name'(3) --> atom_ci('Wed').
'day-name'(4) --> atom_ci('Thu').
'day-name'(5) --> atom_ci('Fri').
'day-name'(6) --> atom_ci('Sat').
'day-name'(7) --> atom_ci('Sun').



%! 'day-name-l'(?Day:between(1,7))// .
%
% ```abnf
% day-name-l = %x4D.6F.6E.64.61.79            ; "Monday", case-sensitive
%            | %x54.75.65.73.64.61.79         ; "Tuesday", case-sensitive
%            | %x57.65.64.6E.65.73.64.61.79   ; "Wednesday", case-sensitive
%            | %x54.68.75.72.73.64.61.79      ; "Thursday", case-sensitive
%            | %x46.72.69.64.61.79            ; "Friday", case-sensitive
%            | %x53.61.74.75.72.64.61.79      ; "Saturday", case-sensitive
%            | %x53.75.6E.64.61.79            ; "Sunday", case-sensitive
% ```

'day-name-l'(1) --> atom_ci('Monday').
'day-name-l'(2) --> atom_ci('Tuesday').
'day-name-l'(3) --> atom_ci('Wednesday').
'day-name-l'(4) --> atom_ci('Thursday').
'day-name-l'(5) --> atom_ci('Friday').
'day-name-l'(6) --> atom_ci('Saturday').
'day-name-l'(7) --> atom_ci('Sunday').



%! 'delay-seconds'(?Seconds:nonneg)// is det.
%
% ```abnf
% delay-seconds = 1*DIGIT
% ```

'delay-seconds'(Seconds) -->
  dcg_integer(+('DIGIT'), Seconds).



%! expect(?Expectation:atom)// .
%
% ```abnf
% Expect = "100-continue"
% ```

http:http_header(expect).
expect('100-continue') -->
  atom_ci('100-continue').



%! from(-Mailbox:atom)// .
%
% ```abnf
% From = mailbox
% ```
%
% Example: `From: webmaster@example.org`

http:http_header(from).
from(Mailbox) -->
  mailbox(Mailbox).



%! 'GMT'// .
%
% "GMT", case-sensitive
%
% ```abnf
% GMT = %x47.4D.54
% ```

'GMT' -->
  atom_ci('GMT').



%! hour(?Hour:between(0,99))// .
%
% ```abnf
% hour = 2DIGIT
% ```

hour(Hour) -->
  dcg_integer(#(2, 'DIGIT'), Hour).



%! 'HTTP-date'(?Datetime:dt)// .
%
% ```abnf
% HTTP-date = IMF-fixdate | obs-date
% ```

'HTTP-date'(Datetime) --> 'IMF-fixdate'(Datetime).
'HTTP-date'(Datetime) --> 'obs-date'(Datetime).



%! 'IMF-fixdate'(?Datetime:dt)// .
%
% Fixed length/zone/capitalization subset of the format see Section
% 3.3 of [RFC5322].
%
% ```abnf
% IMF-fixdate = day-name "," SP date1 SP time-of-day SP GMT
% ```

'IMF-fixdate'(dt(Y,Mo,D,H,Mi,S,0)) -->
  'day-name'(_DayInWeek),
  ",",
  'SP',
  date1(Y, Mo, D),
  'SP',
  'time-of-day'(H, Mi, S),
  'SP',
  'GMT'.



%! location(-Uri:compound)// is det.
%
% ```abnf
% Location = URI-reference
% ```
%
% Example: `Location: /People.html#tim`

http:http_header(location).
location(Uri) -->
  'URI-reference'(Uri).



%! 'max-forwards'(?Max:nonneg)// .
%
% ```abnf
% Max-Forwards = 1*DIGIT
% ```

http:http_header('max-forwards').
'max-forwards'(Max) -->
  dcg_integer(+('DIGIT'), Max).



%! 'media-range'(?MediaType:compound)// .
%
% Type and/or Subtype is a variable if the specified value is `*`.
%
% ```abnf
% media-range = ( "*/*"
%               | ( type "/" "*" )
%               | ( type "/" subtype )
%               ) *( OWS ";" OWS parameter )
% ```

'media-range'(media(Type/Subtype,Parameters)) -->
  media_(Type/Subtype),
  *(sep_parameter_, Parameters), !.

media_('*'/'*') -->
  "*/*", !.
media_(Type/Subtype) -->
  type(Type),
  "/",
  subtype_(Subtype).

subtype_('*') -->
  "*", !.
subtype_(Subtype) -->
  subtype(Subtype).



%! 'media-type'(?MediaType:compound)// .
%
% ```abnf
% media-type = type "/" subtype *( OWS ";" OWS parameter )
% ```

'media-type'(media(Type/Subtype,Parameters)) -->
  type(Type),
  "/",
  subtype(Subtype),
  *(sep_parameter_, Parameters).



%! minute(?Minute:between(0,99))// .
%
% ```abnf
% minute = 2DIGIT
% ```

minute(Mi) -->
  dcg_integer(#(2, 'DIGIT'), Mi).



%! month(?Month:between(1,12))// .
%
% ```abnf
% month = %x4A.61.6E ;   "Jan", case-sensitive
%       | %x46.65.62 ;   "Feb", case-sensitive
%       | %x4D.61.72 ;   "Mar", case-sensitive
%       | %x41.70.72 ;   "Apr", case-sensitive
%       | %x4D.61.79 ;   "May", case-sensitive
%       | %x4A.75.6E ;   "Jun", case-sensitive
%       | %x4A.75.6C ;   "Jul", case-sensitive
%       | %x41.75.67 ;   "Aug", case-sensitive
%       | %x53.65.70 ;   "Sep", case-sensitive
%       | %x4F.63.74 ;   "Oct", case-sensitive
%       | %x4E.6F.76 ;   "Nov", case-sensitive
%       | %x44.65.63 ;   "Dec", case-sensitive
% ```

month(1)  --> atom_ci('Jan').
month(2)  --> atom_ci('Feb').
month(3)  --> atom_ci('Mar').
month(4)  --> atom_ci('Apr').
month(5)  --> atom_ci('May').
month(6)  --> atom_ci('Jun').
month(7)  --> atom_ci('Jul').
month(8)  --> atom_ci('Aug').
month(9)  --> atom_ci('Sep').
month(10) --> atom_ci('Oct').
month(11) --> atom_ci('Nov').
month(12) --> atom_ci('Dec').



%! 'obs-date'(?Datetime:dt)// .
%
% ```abnf
% obs-date = rfc850-date | asctime-date
% ```

'obs-date'(Datetime) --> 'rfc850-date'(Datetime).
'obs-date'(Datetime) --> 'asctime-date'(Datetime).



%! parameter(?Parameter:pair(atom))// .
%
% ```abnf
% parameter = token "=" ( token | quoted-string )
% ```

parameter(Name-Value) -->
  token(Name),
  "=",
  (token(Value) ; 'quoted-string'(Value)).



%! product(?Product:or([atom,pair(atom)]))// .
%
% ```abnf
% product = token ["/" product-version]
% ```

product(Product) -->
  parsing, !,
  token(Name),
  (   "/",
      'product-version'(Version),
      {Product = Name-Version}
  ;   {Product = Name}
  ).
product(Name-Version) --> !,
  token(Name),
  "/",
  'product-version'(Version).
product(Name) -->
  token(Name).



%! 'product-version'(?Version:atom)// .
%
% ```abnf
% product-version = token
% ```

'product-version'(Version) -->
  token(Version).



%! qvalue(?Value:between(0.0,1.0))// .
%
% ```abnf
% qvalue = ( "0" [ "." 0*3DIGIT ] ) | ( "1" [ "." 0*3("0") ] )
% ```

qvalue(N) -->
  parsing, !,
  (   "0"
  ->  (   "."
      ->  'm*n'(0, 3, 'DIGIT', Digits),
          {
            fractional_weights(Frac, Digits),
            N is float(Frac)
          }
      ;   {N = 0}
      )
  ;   "1"
  ->  ("." -> 'm*n'(0, 3, "0") ; ""),
      {N = 1.0}
  ).
qvalue(1.0) --> !,
  "1.000".
qvalue(N) -->
  {
    fractional_weights(N, Weights),
    (   Weights = [X,Y,Z|_]
    ->  true
    ;   Weights = [X,Y],
        Z = 0
    ;   Weights = [X],
        Y = 0,
        Z = 0
    )
  },
  "0.",
  #(3, 'DIGIT', [X,Y,Z]).



%! referer(-Uri:compound)// .
%
% ```abnf
% Referer = absolute-URI | partial-URI
% ```
%
% Example: `Referer: http://www.example.org/hypertext/Overview.html`

http:http_header(referer).
referer(Uri) --> 'absolute-URI'(Uri).
referer(Uri) --> 'partial-URI'(Uri).



%! 'retry-after'(?Datetime:dt)// is det.
%
% ```abnf
% Retry-After = HTTP-date | delay-seconds
% ```

http:http_header('retry-after').
'retry-after'(Datetime) --> 'HTTP-date'(Datetime).
'retry-after'(Datetime) --> 'delay-seconds'(Datetime).



%! 'rfc850-date'(?Datetime:dt)// .
%
% ```abnf
% rfc850-date  = day-name-l "," SP date2 SP time-of-day SP GMT
% ```

'rfc850-date'(dt(Year,Month,Day,Hour,Minute,Second,0)) -->
  'day-name-l'(Day),
  ",",
  'SP',
  date2(Year, Month, Day),
  'SP',
  'time-of-day'(Hour, Minute, Second),
  'SP',
  'GMT'.



%! second(?Second:between(0,99))// .
%
% ```abnf
% second = 2DIGIT
% ```

second(Second) -->
  dcg_integer(#(2, 'DIGIT'), Second).



%! server(?Server:list(pair(atom)))// is det.
%
% ```abnf
% Server = product *( RWS ( product | comment ) )
% ```
%
% Example: `Server: CERN/3.0 libwww/2.17`

http:http_header(server).
server([H|T]) -->
  product(H),
  *(sep_product_or_comment_, T).



%! subtype(?Subtype:atom)// .
%
% ```abnf
% subtype = token
% ```

subtype(Subtype) -->
  token(Subtype).



%! 'time-of-day'(?Hour:between(0,99), ?Minute:between(0,99),
%!               ?Second:between(0,99))// .
%
% ```abnf
% time-of-day = hour ":" minute ":" second
%             ; 00:00:00 - 23:59:60 (leap second)
% ```

'time-of-day'(Hour, Minute, Second) -->
  hour(Hour),
  ":",
  minute(Minute),
  ":",
  second(Second).



%! type(?Type:atom)// .
%
% ```abnf
% type = token
% ```

type(Type) -->
  token(Type).



%! 'user-agent'(?UA:list(pair(atom)))// .
%
% ```abnf
% User-Agent = product *( RWS ( product | comment ) )
% ```
%
% Example: `User-Agent: CERN-LineMode/2.15 libwww/2.17b3`

http:http_header('user-agent').
'user-agent'([H|T]) -->
  product(H),
  *(sep_product_or_comment_, T).



%! vary(?FieldNames:list)// is det.
%
% ```abnf
% Vary = "*" | 1#field-name
% ```
%
% Example: `Vary: accept-encoding, accept-language`

http:http_header(vary).
vary([]) -->
  "*".
vary(L) -->
  +##('field-name', L).



%! weight(?Weight:between(0.0,1.0))// .
%
% ```abnf
% weight = OWS ";" OWS "q=" qvalue
% ```

weight(Weight) -->
  http_sep(0';),
  "q=",
  qvalue(Weight).



%! year(?Year:between(0,9999))// .
%
% ```
% year = 4DIGIT
% ```

year(Year) -->
  dcg_integer(#(4, 'DIGIT'), Year).





% HELPERS %

%! sep_parameter_(+Parameter:pair(atom))// .

sep_parameter_(Parameter) -->
  http_sep(0';),
  parameter(Parameter).



%! sep_product_or_comment_(?Product:pair(atom))// .

sep_product_or_comment_(Product) -->
  'RWS',
  (product(Product) ; comment(_)).



%! '[weight]'(-Weight:between(0.0,1.0))// .

'[weight]'(Weight) -->
  weight(Weight).
'[weight]'(1.0).
