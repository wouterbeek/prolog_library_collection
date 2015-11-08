:- module(
  rfc1738_component,
  [
    domainlabel//1, % ?DomainLabel:string
    host//1, % ?Host:or([list(nonneg),list(string)])
    hostname//1, % ?HostName:list(string)
    hostnumber//1, % ?HostNumber:list(nonneg)
    hostport//2, % ?Host:or([list(nonneg),list(string)])
                 % ?Port:nonneg
    login//4, % ?User:string
              % ?Password:string
              % ?Host:or([list(nonneg),list(string)])
              % ?Port:nonneg
    password//1, % ?Password:string
    port//1, % ?Port:nonneg
    scheme//1, % ?Scheme:string
    toplabel//1, % ?TopLabel:string
    user//1 % ?User:string
  ]
).

/** <module> RFC 1738: Components

@author Wouter Beek
@compat RFC 1739
@deprecated
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(lists)).
:- use_module(library(url/rfc1738_code)).
:- use_module(library(url/rfc1738_helpers)).





%! domainlabel(?DomainLabel:string)// .
% ```abnf
% domainlabel = alphadigit | alphadigit *[ alphadigit | "-" ] alphadigit
% ```

domainlabel(S) --> dcg_string(domainlabel_codes, S).
domainlabel_codes([C]) --> alphadigit(C).
domainlabel_codes(L) -->
  alphadigit(H),
  alphadigits(T),
  alphadigit(X),
  {append([H|T], [X], L)}.



%! host(?Host:or([list(nonneg),list(string)]))// .
% ```abnf
% host = hostname | hostnumber
% ```

host(Host) --> hostname(Host).
host(Host) --> hostnumber(Host).



%! hostname(?Host:list(string))// .
% ```abnf
% hostname = *[ domainlabel "." ] toplabel
% ```

hostname([H|T]) --> domainlabel(H), ".", !, hostname(T).
hostname([H]) --> toplabel(H).



%! hostnumber(?HostNumber:list(nonneg))// .
% ```abnf
% hostnumber = digits "." digits "." digits "." digits
% ```

hostnumber([N1,N2,N3,N4]) -->
  digits(N1), ".", digits(N2), ".", digits(N3), ".", digits(N4).



%! hostport(?Host:or([list(nonneg),list(string)]), ?Port:nonneg)// .
% ```abnf
% hostport = host [ ":" port ]
% ```

hostport(Host, Port) --> host(Host), (":", port(Port) ; {Port = 80}).



%! login(
%!   ?User:string,
%!   ?Password:string,
%!   ?Host:or([list(nonneg),list(string)]),
%!   ?Port:nonneg
%! )// .
% ```abnf
% login = [ user [ ":" password ] "@" ] hostport
% ```

login(User, Password, Host, Port) -->
  (user(User), (":", password(Password) ; ""), "@" ; ""),
  hostport(Host, Port).



%! password(?Password:string)// .
% ```abnf
% password = *[ uchar | ";" | "?" | "&" | "=" ]
% ```

password(S) --> content1(S).



%! port(?Port:nonneg)// .
% ```abnf
% port = digits
% ```

port(N) --> digits(N).



%! scheme(?Scheme:string)// .
% The scheme is in lower case; interpreters should use case-ignore
%
% ```abnf
% scheme = 1*[ lowalpha | digit | "+" | "-" | "." ]
% ```

scheme(Scheme) --> +(scheme_code, Scheme, [convert(1-string)]).
scheme_code(C) --> lowalpha(C).
scheme_code(C) --> digit(C).
scheme_code(0'+) --> "+".
scheme_code(0'-) --> "-".
scheme_code(0'.) --> ".".



%! toplabel(?TopLabel:string)// .
% ```abnf
% toplabel = alpha | alpha *[ alphadigit | "-" ] alphadigit
% ```

toplabel(S) --> dcg_string(toplabel_codes, S).
toplabel_codes([H]) --> alpha(H).
toplabel_codes(L) -->
  alpha(H),
  alphadigits(T),
  alphadigit(X),
  {append([H|T], [X], L)}.



%! user(?User:string)// .
% ```abnf
% user = *[ uchar | ";" | "?" | "&" | "=" ]
% ```

user(S) --> content1(S).
