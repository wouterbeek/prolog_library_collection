:- module(
  licenses,
  [
    cc_license/5, % +Version, +Attribution, +Commercial, +Sharing, -Uri
    license/2,    % ?Name, ?Uri
    license/3,    % ?Name, ?Uri, ?Lbl
    norms/2,      % ?Name, ?Uri
    norms/3       % ?Name, ?Uri, ?Lbl
  ]
).

/** <module> License

@author Wouter Beek
@version 2016/10
*/

:- use_module(library(lists)).
:- use_module(library(uri)).





%! cc_license(+Version, +Attribution, +Commercial, +Sharing, -Uri) is det.
%
%   - *Attribution*
%
%     You must give appropriate credit, provide a link to the license,
%     and indicate if changes were made.  You may do so in any
%     reasonable manner, but not in any way that suggests the licensor
%     endorses you or your use.
%
%   - *NonCommercial*
%
%     You may not use the material for commercial purposes.
%
%   - *Sharing*
%
%     - `false`
%
%       If you remix, transform, or build upon the material, you may
%       not distribute the modified material.
%
%    - `share_alike`
%
%     - `true`

cc_license(Version, Attribution, Commercial, Sharing, Uri) :-
  (Attribution = true -> L1 = [by] ; L1 = []),
  (Commercial == true -> L2 = [] ; L2 = [nc]),
  (   Sharing == false
  ->  L3 = [nd]
  ;   Sharing == share_alike
  ->  L3 = [sa]
  ;   Sharing == true
  ->  L3 = []
  ),
  append([L1,L2,L3], L),
  atomic_list_concat(L, -, Properties),
  atomic_list_concat(['',licenses,Properties,Version], /, Path),
  uri_components(Uri, uri_components(https,'creativecommons.org',Path,_,_)).


%! license(?Name, ?Uri) is multi.
%! license(?Name, ?Uri, ?Lbl) is multi.

license(Name, Uri) :-
  license(Name, Uri, _).


% Dataset licenses
license(
  cc0,
  'http://creativecommons.org/publicdomain/zero/1.0/',
  "CC0 1.0 Universal"
).
license(
  odc_by,
  'http://www.opendatacommons.org/licenses/by/',
  "Open Data Commons Attribution"
).
license(
  odc_odbl,
  'http://www.opendatacommons.org/licenses/odbl/',
  "Open Database License"
).
license(
  pddl,
  'http://www.opendatacommons.org/licenses/pddl/',
  "Public Domain Dedication and License"
).



%! norms(?Name, ?Uri) is multi.
%! norms(?Name, ?Uri, ?Lbl) is multi.

norms(Name, Uri) :-
  norms(Name, Uri, _).


norms(
  odc_by_sa,
  'http://www.opendatacommons.org/norms/odc-by-sa/',
  "ODC Attribution-Sharealike Community Norms"
).
