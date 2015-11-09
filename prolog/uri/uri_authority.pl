:- module(
  uri_authority,
  [
    authority//3 % +Iri:boolean
                 % +Scheme:string
                 % ?Authority:compound
  ]
).

/** <module> RFC 3986 & RFC 3987: Authority component

@author Wouter Beek
@compat RFC 3986
@compat RFC 3987
@see http://tools.ietf.org/html/rfc3987
@version 2015/08, 2015/11
*/

:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(uri/uri_code)).
:- use_module(library(uri/uri_host)).
:- use_module(library(uri/uri_port)).





%! authority(+Iri:boolean, ?Scheme:string, ?Authority:compound)// .
%
% # Syntax
%
% ```abnf
%  authority = [  userinfo "@" ]  host [ ":" port ]
% iauthority = [ iuserinfo "@" ] ihost [ ":" port ]
% ```
%
% If the user info occurs, it is separated from the host with an ampesat.
% If the port occurs, it is separated from the host with a colon.
%
% # Semantics
%
% The authority component determines who has the right to respond
% authoritatively to requests that target the identified resource.
%
% --
%
% @compat RFC 3986
% @compat RFC 3987

authority(I, Scheme, authority(UserInfo,Host,Port)) -->
  (userinfo(I, UserInfo), "@" ; ""),
  host(I, Host),
  % If the port subcomponent is empty or not given,
  % TCP port 80 (the reserved port for WWW services) is the default.
  (   ":", port(Port)
  ;   {default_port(Scheme, Port)}
  ).



%! userinfo(?UserInfo:string)// .
%
% # Syntax
%
% ```abnf
%  userinfo = *(  unreserved / pct-encoded / sub-delims / ":" )
% iuserinfo = *( iunreserved / pct-encoded / sub-delims / ":" )
% ```
%
% # Semantics
%
% This subcomponent may consist of a user name and, optionally,
% scheme-specific information about how to gain authorization to access
% the denoted resource.
%
% # Security
%
% Applications that render a URI for the sake of user feedback, such as
% in graphical hypertext browsing, should render userinfo in a way that
% is distinguished from the rest of a URI, when feasible.  Such
% rendering will assist the user in cases where the userinfo has been
% misleadingly crafted to look like a trusted domain name
%
% # Legacy
%
% Use of the format `user:password` in the userinfo field is deprecated.
% The passing of authentication information in clear text has proven to be
% a security risk.
%
% --
%
% @compat RFC 3986
% @compat RFC 3987

userinfo(I, S) --> dcg_string(userinfo_codes(I), S).

userinfo_codes(I, [H|T]) -->
  (   unreserved(I, H)
  ;   'pct-encoded'(H)
  ;   'sub-delims'(H)
  ;   colon(H)
  ), !,
  userinfo_codes(I, T).
userinfo_codes(_, []) --> "".





% HELPERS %

%! default_port(+Scheme:atom, -Port:oneof([443,80])) is det.
% The default port for the given scheme.

default_port(http, 80).
default_port(https, 443).
