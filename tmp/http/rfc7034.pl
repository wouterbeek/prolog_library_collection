:- module(
  rfc7034,
  [
    'x-frame-options'//1 % -X
  ]
).

/** <module> RFC 7034: HTTP Header Field X-Frame-Options

@author Wouter Beek
@compat RFC 7034
@see https://tools.ietf.org/html/rfc7034
@version 2015/12, 2016/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
     'HTAB'//0,
     'SP'//0
   ]).
:- use_module(library(http/rfc6454)).





%! 'RWS'// is det.
%
% ```abnf
% RWS = 1*( SP / HTAB )
% ```

'RWS' -->
  +('SP' ; 'HTAB'), !.



%! 'x-frame-options'(-X)// is det.
%
% ```abnf
% X-Frame-Options = "DENY"
%                 / "SAMEORIGIN"
%                 / ( "ALLOW-FROM" RWS SERIALIZED-ORIGIN )
% ```
%
% The following values are supported:
%   * `DENY'
%   * `SAMEORIGIN'
%   * `ALLOW-FROM'
%     Followed by a serialized-origin [RFC6454].

'x-frame-options'(deny) -->
  atom_ci('DENY').
'x-frame-options'(sameorigin) -->
  atom_ci('SAMEORIGIN').
'x-frame-options'(Origin) -->
  atom_ci('ALLOW-FROM'),
  'RWS',
  'serialized-origin'(Origin).
