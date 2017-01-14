:- module(
  ckan_api,
  [
    ckan_group/2,        % ?Uri, -Group
    ckan_license/2,      % ?Uri, -License
    ckan_organization/2, % ?Uri, -Org
    ckan_package/2,      % ?Uri, -Pack
    %%%%ckan_resource/2,     % ?Uri, -Res
    ckan_site/1,         % -Site
    ckan_site_url/1,     % -Uri
    ckan_tag/2,          % ?Uri, -Tag
    ckan_user/2          % ?Uri, -User
  ]
).
:- reexport(library(print_ext)).

/** <module> CKAN API

@author Wouter Beek
@version 2016/04, 2017/01
*/

:- use_module(library(json_ext)).
:- use_module(library(lists)).
:- use_module(library(service/ckan_rest)).





%! ckan_group(+Uri, -Group:dict) is nondet.
%! ckan_group(-Uri, -Group:dict) is nondet.
%
% Enumerates CKAN groups.
%
% The following keys appear:
%   - approval_status
%   - created
%   - description
%   - display_name
%   - extras
%   - groups
%   - id
%   - image_display_url
%   - image_url
%   - is_organization
%   - name
%   - num_followers
%   - package_count
%   - revision_id
%   - state
%   - tags
%   - title
%   - type
%   - users

ckan_group(Uri, Dict) :-
  (var(Uri) -> ckan_site_url(Uri) ; true),
  ckan_group_list(Uri, _{page_size: 100}, Groups),
  member(Group, Groups),
  ckan_group_show(Uri, Group, Dict).



%! ckan_license(+Uri, -License) is nondet.
%! ckan_license(-Uri, -License) is nondet.
%
% Enumerates CKAN licenses.

ckan_license(Uri, License) :-
  (var(Uri) -> ckan_site_url(Uri) ; true),
  catch_list_request(ckan_license_list(Uri), Licenses),
  member(License, Licenses).



%! ckan_organization(+Uri, -Org) is nondet.
%! ckan_organization(-Uri, -Org) is nondet.
%
% Enumerates CKAN organizations.

ckan_organization(Uri, Dict) :-
  (var(Uri) -> ckan_site_url(Uri) ; true),
  ckan_organization_list(Uri, _{page_size: 100}, Orgs),
  member(Org, Orgs),
  ckan_organization_show(Uri, Org, Dict).



%! ckan_package(+Uri, -Pack) is nondet.
%! ckan_package(-Uri, -Pack) is nondet.
%
% Enumerates CKAN packages.

ckan_package(Uri, Dict) :-
  (var(Uri) -> ckan_site_url(Uri) ; true),
  ckan_package_list(Uri, Packs),
  member(Pack, Packs),
  ckan_package_show(Uri, Pack, Dict).



/*
%! ckan_resource(+Uri, -Res) is nondet.
%! ckan_resource(-Uri, -Res) is nondet.
%
% Enumerates CKAN resources.

ckan_resource(Uri, Dict) :-
  (var(Uri) -> ckan_site_url(Uri) ; true),
  ckan_resource_view_list(Uri, Ress),
  member(Res, Ress),
  ckan_resource_show(Uri, Res, Dict).
*/



%! ckan_site(-Site) is nondet.

ckan_site(Site) :-
  json_read_any('http://instances.ckan.org/config/instances.json', Sites),
  member(Site, Sites).



%! ckan_site_url(-Uri) is det.

ckan_site_url(Uri) :-
  ckan_site(Site),
  (get_dict('url-api', Site, Uri0) -> true ; get_dict(url, Site, Uri0)),
  atom_string(Uri, Uri0).



%! ckan_tag(+Uri, -Tag) is nondet.
%! ckan_tag(-Uri, -Tag) is nondet.
%
% Enumerates CKAN tags.

ckan_tag(Uri, Dict) :-
  (var(Uri) -> ckan_site_url(Uri) ; true),
  ckan_tag_list(Uri, Tags),
  member(Tag, Tags),
  ckan_tag_show(Uri, Tag, Dict).



%! ckan_user(+Uri, -User) is nondet.
%! ckan_user(-Uri, -User) is nondet.
%
% Enumerates CKAN users.

ckan_user(Uri, Dict) :-
  (var(Uri) -> ckan_site_url(Uri) ; true),
  ckan_user_list(Uri, Users),
  member(User, Users),
  ckan_user_show(Uri, User, Dict).




% HELPERS %

:- meta_predicate
    catch_list_request(1, -),
    call_catcher_cleanup(0, ?, 0).

%! catch_list_request(:Goal_1, -L) is det.
%
% @tbd This still throws exceptions!  Why?

catch_list_request(Mod:Goal_1, L) :-
  Goal_1 =.. [Pred|Args1],
  append(Args1, [L], Args2),
  Goal_0 =.. [Pred|Args2],
  call_catcher_cleanup(Mod:Goal_0, Catcher, cleanup_list_request(Catcher, L)).

cleanup_list_request(exit, _) :- !.
cleanup_list_request(E, []) :-
  msg_warning("[CKAN] ~w~n", [E]).

call_catcher_cleanup(Goal_0, Catcher, Cleanup_0) :-
  setup_call_catcher_cleanup(true, Goal_0, Catcher, Cleanup_0).
