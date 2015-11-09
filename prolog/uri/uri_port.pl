:- module(
  uri_port,
  [
    port//1 % ?Port:nonneg
  ]
).

/** <module> RFC 3986 & RFC 3987: Port component

Grammar for port subcomponent of URI.

@author Wouter Beek
@compat RFC 3986
@compat RFC 3987
@version 2015/08, 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(math/positional)).





%! port(?Port:nonneg)// .
% ```abnf
% port = *DIGIT
% ```
%
% @compat RFC 3986
% @compat RFC 3987

port(Port) --> *('DIGIT', Port, [convert1(clpfd_positional)]).
