:- module(
  rfc1738,
  [
    genericurl//2, % ?Scheme:string
                   % ?SchemeSpecificPart:string
    genericurl//6, % ?Scheme:string
                   % ?User:string
                   % ?Password:string
                   % ?Host:or([list(nonneg),list(string)])
                   % ?Port:nonneg
                   % ?Path:string
    'ip-schemepart'//5, % ?User:string
                        % ?Password:string
                        % ?Host:or([list(nonneg),list(string)])
                        % ?Port:nonneg
                        % ?Path:string
    otherurl//2, % ?Scheme:string
                 % ?SchemeSpecificPart:string
    otherurl//6, % ?Scheme:string
                 % ?User:string
                 % ?Password:string
                 % ?Host:or([list(nonneg),list(string)])
                 % ?Port:nonneg
                 % ?Path:string
    schemepart//1, % ?SchemeSpecificPart:string
    schemepart//5, % ?User:string
                   % ?Password:string
                   % ?Host:or([list(nonneg),list(string)])
                   % ?Port:nonneg
                   % ?Path:string
    url//1, % ?Url:compound
    urlpath//1 % ?Path:string
  ]
).

/** <module> RFC 1738: Uniform Resource Locators (URL)

@author Wouter Beek
@compat RFC 1738
@deprecated Use module `rfc????` instead.
@version 2015/11
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(url/rfc1738_component)).
:- use_module(library(url/rfc1738_file)).
:- use_module(library(url/rfc1738_ftp)).
:- use_module(library(url/rfc1738_gopher)).
:- use_module(library(url/rfc1738_http)).





%! genericurl(?Scheme:string, ?SchemeSpecificPart:string)// .
%! genericurl(
%!   ?Scheme:string,
%!   ?User:string,
%!   ?Password:string,
%!   ?Host:or([list(nonneg),list(string)]),
%!   ?Port:nonneg,
%!   ?Path:string
%! )// .
% ```abnf
% genericurl = scheme ":" schemepart
% ```

genericurl(Scheme, S) --> scheme(Scheme), ":", schemepart(S).
genericurl(Scheme, User, Password, Host, Port, Path) -->
  scheme(Scheme),
  ":",
  schemepart(User, Password, Host, Port, Path).



%! 'ip-schemepart'(
%!   ?User:string,
%!   ?Password:string,
%!   ?Host:or([list(nonneg),list(string)]),
%!   ?Port:nonneg,
%!   ?Path:string
%1 )// .
% ```abnf
% ip-schemepart = "//" login [ "/" urlpath ]
% ```

'ip-schemepart'(User, Password, Host, Port, Path) -->
  "//",
  login(User, Password, Host, Port),
  ("/", urlpath(Path) ; "").



%! otherurl(?Scheme:string, ?SchemeSpecificPart:string)// .
%! otherurl(
%!   ?Scheme:string,
%!   ?User:string,
%!   ?Password:string,
%!   ?Host:or([list(nonneg),list(string)]),
%!   ?Port:nonneg,
%!   ?Path:string
%! )// .
% ```abfn
% otherurl = genericurl
% ```

otherurl(Scheme, S) --> genericurl(Scheme, S).
otherurl(Scheme, User, Password, Host, Port, Path) -->
  genericurl(Scheme, User, Password, Host, Port, Path).



%! schemepart(?SchemeSpecificPart:string)// .
%! schemepart(
%!   ?User:string,
%!   ?Password:string,
%!   ?Host:or([list(nonneg),list(string)]),
%!   ?Port:nonneg,
%!   ?Path:string
%! )// .
% The scheme-specific part of a URL.
%
% ```abnf
% schemepart = *xchar | ip-schemepart
% ```

schemepart(S) --> *(xchar, Cs), {string_codes(S, Cs)}.
schemepart(User, Password, Host, Port, Path) -->
  'ip-schemepart'(User, Password, Host, Port, Path).



%! url(?Url:compound)// .
% ```abnf
% url = httpurl | ftpurl | newsurl | nntpurl | telneturl | gopherurl
%     | waisurl | mailtourl | fileurl | prosperourl | otherurl
% ```

url(http(Host, Port, Path, Search)) --> httpurl(Host, Port, Path, Search).
url(ftp(User,Password,Host,Port,Path,Type)) -->
  ftpurl(User, Password, Host, Port, Path, Type).
%url(S) --> newsurl(S).
%url(S) --> nntpurl(S).
%url(S) --> telneturl(S).
url(gopher(Host,Port,Type,Selector,Search,String)) -->
  gopherurl(Host, Port, Type, Selector, Search, String).
%url(S) --> waisurl(S).
%url(S) --> mailtourl(S).
url(file(Host,Path)) --> fileurl(Host, Path).
%url(S) --> prosperourl(S).
url(other(Scheme,SchemeSpecificPart)) --> otherurl(Scheme, SchemeSpecificPart).
url(other(Scheme,User,Password,Host,Port,Path)) -->
  otherurl(Scheme, User, Password, Host, Port, Path).



%! urlpath(?Path:string)// .
% ```abnf
% urlpath = *xchar   ; depends on protocol see section 3.1
% ```

urlpath(S) --> *(xchar, Cs), {string_codes(S, Cs)}.

/*
% MAILTO %

mailtourl      = "mailto:" encoded822addr
encoded822addr = 1*xchar               ; further defined in RFC822

; NEWS (see also RFC1036)

newsurl        = "news:" grouppart
grouppart      = "*" | group | article
group          = alpha *[ alpha | digit | "-" | "." | "+" | "_" ]
article        = 1*[ uchar | ";" | "/" | "?" | ":" | "&" | "=" ] "@" host

; NNTP (see also RFC977)

nntpurl        = "nntp://" hostport "/" group [ "/" digits ]

; TELNET

telneturl      = "telnet://" login [ "/" ]

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
*/
