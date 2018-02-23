:- module(
  rfc1738,
  [
    alpha//1,      % ?Code
    alphadigit//1, % ?Code
    digit//1,      % ?Weight
    digit//2,      % ?Weight, ?Code
    escape//1,     % ?Code
    hex//1,        % ?Weight
    hex//2,        % ?Weight, ?Code
    hialpha//1,    % ?Code
    lowalpha//1    % ?Code
  ]
).

/** <module> RFC 1738: Uniform Resource Locators (URL)

@tbd
```
; WAIS (see also RFC1625)

waisurl        = waisdatabase | waisindex | waisdoc
waisdatabase   = "wais://" hostport "/" database
waisindex      = "wais://" hostport "/" database "?" search
waisdoc        = "wais://" hostport "/" database "/" wtype "/" wpath
database       = *uchar
wtype          = *uchar
wpath          = *uchar

; PROSPERO

prosperourl    = "prospero://" hostport "/" ppath *[ fieldspec ]
ppath          = psegment *[ "/" psegment ]
psegment       = *[ uchar | "?" | ":" | "@" | "&" | "=" ]
fieldspec      = ";" fieldname "=" fieldvalue
fieldname      = *[ uchar | "?" | ":" | "@" | "&" ]
fieldvalue     = *[ uchar | "?" | ":" | "@" | "&" ]
```

@author Wouter Beek
@compat RFC 1738
@deprecated
@see https://tools.ietf.org/html/rfc1738
@version 2015/11-2015/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).





%! alpha(?Code:code)// .
% Alphabetic ASCII character.
%
% ```abnf
% alpha = lowalpha | upalpha
% ```

alpha(C) --> lowalpha(C).
alpha(C) --> hialpha(C).



%! alphadigit(?Code:code)// .
% Alphabetic ASCII character or decimal digit.
%
% ```abnf
% alphadigit = alpha | digit
% ```

alphadigit(C) --> alpha(C).
alphadigit(C) --> digit(_, C).



%! article(-Article:string, -Host)// is det.
% ```abnf
% article = 1*[ uchar | ";" | "/" | "?" | ":" | "&" | "=" ] "@" host
% ```

article(Article, Host) -->
  +(article_code, Cs),
  {string_codes(Article, Cs)},
  "@",
  host(Host).
article_code(C)   --> uchar(C).
article_code(0';) --> ";".
article_code(0'/) --> "/".
article_code(0'?) --> "?".
article_code(0':) --> ":".
article_code(0'&) --> "&".
article_code(0'=) --> "=".



%! digit(?Weight:between(0,9))// is det.
% Wrapper around digit//2.

digit(D) --> digit(D, _).


%! digit(?Weight:between(0,9), ?Code:code)// is det.
% ```abnf
% digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
% ```

digit(0, 0'0) --> "0", !.
digit(1, 0'1) --> "1", !.
digit(2, 0'2) --> "2", !.
digit(3, 0'3) --> "3", !.
digit(4, 0'4) --> "4", !.
digit(5, 0'5) --> "5", !.
digit(6, 0'6) --> "6", !.
digit(7, 0'7) --> "7", !.
digit(8, 0'8) --> "8", !.
digit(9, 0'9) --> "9".



%! digits(-Number:nonneg)// is det.
% ```abnf
% digits = 1*digit
% ```

digits(N) -->
  dcg_integer(+(digit), N).



%! domainlabel(-DomainLabel:string)// is det.
% ```abnf
% domainlabel = alphadigit | alphadigit *[ alphadigit | "-" ] alphadigit
% ```

domainlabel(S) --> dcg_string(domainlabel_codes, S).
domainlabel_codes(L)   -->
  alphadigit(H),
  *(alphadigit_hyphen, T),
  alphadigit(X),
  {append([H|T], [X], L)}.
domainlabel_codes([C]) --> alphadigit(C).



%! encoded822addr(-EncodedAddress:string)// is det.
% ```abnf
% encoded822addr = 1*xchar   ; further defined in RFC822
% ```

encoded822addr(S) --> +(xchar, Cs), {string_codes(S, Cs)}.



%! escape(?Code:code)// .
% ```abnf
% escape = "%" hex hex
% ```

escape(C) --> "%", hex(D1), hex(D2), {C is D1 * 16 + D2}.



%! extra(?Code:code)// .
% ```abnf
% extra = "!" | "*" | "'" | "(" | ")" | ","
% ```

extra(0'!) --> "!".
extra(0'*) --> "*".
extra(0'') --> "'".
extra(0'() --> "(".
extra(0')) --> ")".
extra(0',) --> ",".



%! fileurl(-Url:dict)// is det.
% ```abnf
% fileurl = "file://" [ host | "localhost" ] "/" fpath
% ```

fileurl(url{host: Host, path: Path, scheme: file}) -->
  atom_ci(file),
  "://",
  (host(Host), ! ; atom_ci(localhost), {Host = localhost}),
  "/",
  fpath(Path).



%! ftpurl(-Url:dict)// is det.
% ```abnf
% ftpurl = "ftp://" login [ "/" fpath [ ";type=" ftptype ]]
% ```

ftpurl(D) -->
  atom_ci(ftp),
  "://",
  login(User, Password, Host, Port),
  (   "/"
  ->  fpath(Path),
      (";", atom_ci(type), "=" -> ftptype(Type), {T = [type-Type]} ; {T = []})
  ;   {Path = []}
  ),
  {
    L = [host-Host,password-Password,path-Path,port-Port,scheme-ftp,user-User|T],
    dict_pairs(D, url, L)
  }.



%! fpath(-Path:list(string))// is det.
% ```abnf
% fpath = fsegment *[ "/" fsegment ]
% ```

fpath([H|T]) --> fsegment(H), *(sep_fsegment, T).
sep_fsegment(X) --> "/", fsegment(X).



%! fsegment(-Segment:string)// is det.
% ```abnf
% fsegment = *[ uchar | "?" | ":" | "@" | "&" | "=" ]
% ```

fsegment(S) --> *(fsegment_code, Cs), {string_codes(S, Cs)}.
fsegment_code(C)   --> uchar(C).
fsegment_code(0'?) --> "?".
fsegment_code(0':) --> ":".
fsegment_code(0'@) --> "@".
fsegment_code(0'&) --> "&".
fsegment_code(0'=) --> "=".



%! ftptype(-Type:oneof(["A","D","I","a","d","i"]))// is det.
% ```abnf
% ftptype = "A" | "I" | "D" | "a" | "i" | "d"
% ```

ftptype("A") --> "A", !.
ftptype("D") --> "D", !.
ftptype("I") --> "I", !.
ftptype("a") --> "a", !.
ftptype("d") --> "d", !.
ftptype("i") --> "i".



%! genericurl(-GenericUrl:dict)// is nondet.
% ```abnf
% genericurl = scheme ":" schemepart
% ```

genericurl(D) -->
  scheme(Scheme),
  ":",
  schemepart(D0),
  {D = D0.put(scheme, Scheme)}.



%! 'gopher+_string'(?String:string) //
% ```abnf
% gopher+_string = *xchar
% ```

'gopher+_string'(S) --> *(xchar, Cs), {string_codes(S, Cs)}.



%! gopherurl(-Url:dict)// is det.
% ```abnf
% gopherurl = "gopher://" hostport [ / [ gtype [ selector
%             [ "%09" search [ "%09" gopher+_string ] ] ] ] ]
% ```

gopherurl(D) -->
  atom_ci(gopher),
  "://",
  hostport(Host, Port),
  (   "/"
  ->  (   gtype(Type)
      ->  (   selector(Selector)
          ->  (   "%09"
              ->  search(Search),
	          (   "%09"
                  ->  'gopher+_string'(String)
                  ;   ""
                  )
              ;   ""
              )
          ;   ""
          )
      ;   ""
      )
  ;   ""
  ),
  {
    L0 = [
      host-Host,
      port-Port,
      scheme-gopher,
      search-Search,
      selector-Selector,
      string-String,
      type-Type
    ],
    exclude(pair_has_var_value, L0, L),
    dict_pairs(D, url, L)
  }.



%! group(-Group:string)// is det.
% ```abnf
% group = alpha *[ alpha | digit | "-" | "." | "+" | "_" ]
% ```

group(S) --> alpha(H), *(group_code, T), {string_codes(S, [H|T])}.
group_code(C) --> alpha(C).
group_code(C) --> digit(_, C).
group_code(0'-) --> "-".
group_code(0'.) --> ".".
group_code(0'+) --> "+".
group_code(0'_) --> "_".



%! grouppart(-Pairs:list(pair))// is det.
% ```abnf
% grouppart = "*" | group | article
% ```

grouppart([]) --> "*", !.
grouppart([group-Group]) --> group(Group), !.
grouppart([article-Article,host-Host]) --> article(Article, Host).



%! gtype(-Type:string)// is det.
% ```abnf
% gtype = xchar
% ```

gtype(S) --> xchar(C), {string_codes(S, [C])}.



%! hex(?Weight:between(0,15))// is det.
% Wrapper around hex//2.

hex(I) --> hex(I, _).


%! hex(?Weight:between(0,15), ?Code:code)// is det.
% Hexadecimal digit, supporting both uppercase and lowercase letters.
%
% ```abnf
% hex = digit
%     | "A" | "B" | "C" | "D" | "E" | "F"
%     | "a" | "b" | "c" | "d" | "e" | "f"
% ```

hex(I, C)    --> digit(I, C), !.
hex(10, 0'A) --> "A", !.
hex(11, 0'B) --> "B", !.
hex(12, 0'C) --> "C", !.
hex(13, 0'D) --> "D", !.
hex(14, 0'E) --> "E", !.
hex(15, 0'F) --> "F", !.
hex(10, 0'a) --> "a", !.
hex(11, 0'b) --> "b", !.
hex(12, 0'c) --> "c", !.
hex(13, 0'd) --> "d", !.
hex(14, 0'e) --> "e", !.
hex(15, 0'f) --> "f".



%! hialpha(?Code:code)// .
% Upper-case alphabetic ASCII character code.
%
% ```abnf
% hialpha = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I"
%         | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R"
%         | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
% ```

hialpha(0'A) --> "A".
hialpha(0'B) --> "B".
hialpha(0'C) --> "C".
hialpha(0'D) --> "D".
hialpha(0'E) --> "E".
hialpha(0'F) --> "F".
hialpha(0'G) --> "G".
hialpha(0'H) --> "H".
hialpha(0'I) --> "I".
hialpha(0'J) --> "J".
hialpha(0'K) --> "K".
hialpha(0'L) --> "L".
hialpha(0'M) --> "M".
hialpha(0'N) --> "N".
hialpha(0'O) --> "O".
hialpha(0'P) --> "P".
hialpha(0'Q) --> "Q".
hialpha(0'R) --> "R".
hialpha(0'S) --> "S".
hialpha(0'T) --> "T".
hialpha(0'U) --> "U".
hialpha(0'V) --> "V".
hialpha(0'W) --> "W".
hialpha(0'X) --> "X".
hialpha(0'Y) --> "Y".
hialpha(0'Z) --> "Z".



%! host(-Host:or([list(nonneg),list(string)]))// is det.
% ```abnf
% host = hostname | hostnumber
% ```

host(Host) --> hostname(Host).
host(Host) --> hostnumber(Host).



%! hostname(-Host:list(string))// is det.
% ```abnf
% hostname = *[ domainlabel "." ] toplabel
% ```

hostname([H|T]) --> domainlabel(H), ".", !, hostname(T).
hostname([H])   --> toplabel(H).



%! hostnumber(-HostNumber:list(nonneg))// is det.
% ```abnf
% hostnumber = digits "." digits "." digits "." digits
% ```

hostnumber([N1,N2,N3,N4]) -->
  digits(N1), ".", digits(N2), ".", digits(N3), ".", digits(N4).



%! hostport(-Host:or([list(nonneg),list(string)]), -Port:nonneg)// is det.
% ```abnf
% hostport = host [ ":" port ]
% ```

hostport(Host, Port) --> host(Host), (":" -> port(Port) ; {Port = 80}).



%! hpath(-Path:list(string))// is det.
% ```abnf
% hpath = hsegment *[ "/" hsegment ]
% ```

hpath([H|T]) --> hsegment(H), *(sep_hsegment, T).
sep_hsegment(C) --> "/", hsegment(C).



%! hsegment(-Segment:string)// is det.
% ```abnf
% hsegment = *[ uchar | ";" | ":" | "@" | "&" | "=" ]
% ```

hsegment(S) --> *(code2, Cs), {string_codes(S, Cs)}.



%! httpurl(-Url:dict)// is det.
% ```abnf
% httpurl = "http://" hostport [ "/" hpath [ "?" search ]]
% ```

httpurl(D) -->
  atom_ci(http),
  "://",
  hostport(Host, Port),
  ("/" -> hpath(Path), ("?" -> search(Search) ; "") ; {Path = []}),
  {
    T0 = [host-Host,path-Path,port-Port,search-Search],
    exclude(pair_has_var_value, T0, T),
    dict_pairs(D, url, [scheme-http|T])
  }.



%! 'ip-schemepart'(-SchemeSpecificPart:dict)// is det.
% ```abnf
% ip-schemepart = "//" login [ "/" urlpath ]
% ```

'ip-schemepart'(D) -->
  "//",
  login(User, Password, Host, Port),
  ("/" -> urlpath(Path) ; {Path = []}),
  {
    exclude(
      pair_has_var_value,
      [host-Host,password-Password,path-Path,port-Port,user-User],
      L
    ),
    dict_pairs(D, scheme_part, L)
  }.



%! login(
%!   -User:string,
%!   -Password:string,
%!   -Host:or([list(nonneg),list(string)]),
%!   -Port:nonneg
%! )// .
% ```abnf
% login = [ user [ ":" password ] "@" ] hostport
% ```

login(User, Password, Host, Port) -->
  (user(User) -> (":" -> password(Password) ; ""), "@" ; ""),
  hostport(Host, Port).



%! lowalpha(?Code:code)// .
% Lower-case alphabetic ASCII character code.
%
% ```abnf
% lowalpha = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i"
%          | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r"
%          | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
% ```

lowalpha(0'a) --> "a".
lowalpha(0'b) --> "b".
lowalpha(0'c) --> "c".
lowalpha(0'd) --> "d".
lowalpha(0'e) --> "e".
lowalpha(0'f) --> "f".
lowalpha(0'g) --> "g".
lowalpha(0'h) --> "h".
lowalpha(0'i) --> "i".
lowalpha(0'j) --> "j".
lowalpha(0'k) --> "k".
lowalpha(0'l) --> "l".
lowalpha(0'm) --> "m".
lowalpha(0'n) --> "n".
lowalpha(0'o) --> "o".
lowalpha(0'p) --> "p".
lowalpha(0'q) --> "q".
lowalpha(0'r) --> "r".
lowalpha(0's) --> "s".
lowalpha(0't) --> "t".
lowalpha(0'u) --> "u".
lowalpha(0'v) --> "v".
lowalpha(0'w) --> "w".
lowalpha(0'x) --> "x".
lowalpha(0'y) --> "y".
lowalpha(0'z) --> "z".



%! mailtourl(-Url:dict)// is det.
% ```abnf
% mailtourl = "mailto:" encoded822addr
% ```

mailtourl(url{address: Addr, scheme:mailto}) -->
  atom_ci(mailto),
  ":",
  encoded822addr(Addr).



%! national(?Code:code)// .
% ```abnf
% national = "{" | "}" | "|" | "\" | "^" | "~" | "[" | "]" | "`"
% ```

national(0'{)  --> "{".
national(0'})  --> "}".
national(0'|)  --> "|".
national(0'\\) --> "\\".
national(0'^)  --> "^".
national(0'~)  --> "~".
national(0'[)  --> "[".
national(0'])  --> "]".
national(0'`)  --> "`".



%! newsurl(-Url:dict)// is det.
% ```abnf
% newsurl = "news:" grouppart
% ```

newsurl(D) -->
  atom_ci(news),
  ":",
  grouppart(T),
  {dict_pairs(D, url, [scheme-news|T])}.



%! nntpurl(-Url:dict)// is det.
% ```abnf
% nntpurl = "nntp://" hostport "/" group [ "/" digits ]
% ```

nntpurl(D) -->
  atom_ci(nntp),
  "://",
  hostport(Host, Port),
  "/",
  group(Group),
  ("/" -> digits(N) ; ""),
  {
    exclude(pair_has_var_value, [group-Group,host-Host,n-N,port-Port], T),
    dict_pairs(D, url, [type-nntp|T])
  }.



%! otherurl(-Url:dict)// is det.
% ```abfn
% otherurl = genericurl
% ```

otherurl(D) --> genericurl(D).



%! password(-Password:string)// is det.
% ```abnf
% password = *[ uchar | ";" | "?" | "&" | "=" ]
% ```

password(S) --> *(code1, Cs), {string_codes(S, Cs)}.



%! port(-Port:nonneg)// is det.
% ```abnf
% port = digits
% ```

port(N) --> digits(N).



%! punctuation(?Code:code)// .
% ```abnf
% punctuation = "<" | ">" | "#" | "%" | <">
% ```

punctuation(0'<) --> "<".
punctuation(0'>) --> ">".
punctuation(0'#) --> "#".
punctuation(0'%) --> "%".
punctuation(0'") --> "\"".   %"



%! reserved(?Code:Code)// .
% ```abnf
% reserved = ";" | "/" | "?" | ":" | "@" | "&" | "="
% ```

reserved(0';) --> ";".
reserved(0'/) --> "/".
reserved(0'?) --> "?".
reserved(0':) --> ":".
reserved(0'@) --> "@".
reserved(0'&) --> "&".
reserved(0'=) --> "=".



%! safe(?Code:code)// .
% ```abnf
% safe = "$" | "-" | "_" | "." | "+"
% ```

safe(0'$) --> "$".
safe(0'-) --> "-".
safe(0'_) --> "_".
safe(0'.) --> ".".
safe(0'+) --> "+".



%! scheme(-Scheme:string)// is det.
% The scheme is in lower case; interpreters should use case-ignore
%
% ```abnf
% scheme = 1*[ lowalpha | digit | "+" | "-" | "." ]
% ```

scheme(S) --> +(scheme_code, Cs), {string_codes(S, Cs)}.
scheme_code(C)   --> lowalpha(C).
scheme_code(C)   --> digit(C).
scheme_code(0'+) --> "+".
scheme_code(0'-) --> "-".
scheme_code(0'.) --> ".".



%! schemepart(-SchemeSpecificPart)// is det.
% The scheme-specific part of a URL.
%
% ```abnf
% schemepart = *xchar | ip-schemepart
% ```

schemepart(D) --> 'ip-schemepart'(D), !.
schemepart(S) --> *(xchar, Cs), {string_codes(S, Cs)}.



%! search(-Search:string)// is det.
% ```abnf
% search = *[ uchar | ";" | ":" | "@" | "&" | "=" ]
% ```

search(S) --> *(code2, Cs), {string_codes(S, Cs)}.



%! selector(-Selector:string)// is det.
% ```abnf
% selector = *xchar
% ```

selector(S) --> *(xchar, Cs), {string_codes(S, Cs)}.



%! telneturl(-Url:dict)// is det.
% ```abnf
% telneturl = "telnet://" login [ "/" ]
% ```

telneturl(D) -->
  atom_ci(telnet),
  "://",
  login(User, Password, Host, Port),
  ?("/"),
  {
    exclude(
      pair_has_var_value,
      [host-Host,password-Password,port-Port,user-User],
      T
    ),
    dict_pairs(D, scheme_part, [type-telnet|T])
  }.



%! toplabel(-TopLabel:string)// is det.
% ```abnf
% toplabel = alpha | alpha *[ alphadigit | "-" ] alphadigit
% ```

toplabel(S) --> dcg_string(toplabel_codes, S).
toplabel_codes(L)   -->
  alpha(H),
  *(alphadigit_hyphen, T),
  alphadigit(X),
  {append([H|T], [X], L)}.
toplabel_codes([H]) --> alpha(H).



%! uchar(?Code:code)// .
% ```abnf
% uchar = unreserved | escape
% ```

uchar(C) --> unreserved(C).
uchar(C) --> escape(C).



%! unreserved(?Code:code)// .
% ```abnf
% unreserved = alpha | digit | safe | extra
% ```

unreserved(C) --> alpha(C).
unreserved(C) --> digit(C).
unreserved(C) --> safe(C).
unreserved(C) --> extra(C).



%! url(-Url:dict)// is det.
% ```abnf
% url = httpurl | ftpurl | newsurl | nntpurl | telneturl | gopherurl
%     | waisurl | mailtourl | fileurl | prosperourl | otherurl
% ```

url(D) --> fileurl(D), !.
url(D) --> ftpurl(D), !.
url(D) --> gopherurl(D), !.
url(D) --> httpurl(D), !.
url(D) --> mailtourl(D), !.
url(D) --> newsurl(D), !.
url(D) --> nntpurl(D), !.
url(D) --> otherurl(D).
%url(D) --> prosperourl(D), !.
url(D) --> telneturl(D), !.
%url(D) --> waisurl(D), !.



%! urlpath(?Path:string)// .
% ```abnf
% urlpath = *xchar   ; depends on protocol see section 3.1
% ```

urlpath(S) --> *(xchar, Cs), {string_codes(S, Cs)}.



%! user(?User:string)// .
% ```abnf
% user = *[ uchar | ";" | "?" | "&" | "=" ]
% ```

user(S) --> *(code1, Cs), {string_codes(S, Cs)}.



%! xchar(?Code:code)// .
% ```abnf
% xchar = unreserved | reserved | escape
% ```

xchar(C) --> unreserved(C).
xchar(C) --> reserved(C).
xchar(C) --> escape(C).





% HELPERS %

alphadigit_hyphen(C)   --> alphadigit(C).
alphadigit_hyphen(0'-) --> "-".


code1(C)   --> uchar(C).
code1(0';) --> ";".
code1(0'?) --> "?".
code1(0'&) --> "&".
code1(0'=) --> "=".

code2(C)   --> code1(C).
code2(0':) --> ":".
