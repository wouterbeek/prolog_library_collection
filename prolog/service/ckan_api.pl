:- module(
  ckan_api,
  [
    ckan_current_package_list_with_resources/3, % +Site, +Args, -Packs
    ckan_group_list/3,                          % +Site, +Data, -Groups
    ckan_group_list_authz/3,                    % +Site, +Data, -Groups
    ckan_group_package_show/4,                  % +Site, +Group, +Data, -Packs
    ckan_group_revision_list/4,                 % +Site, +Group, +Data, -Revs
    ckan_group_show/4,                          % +Site, +Group, +Data, -M
    ckan_license_list/3,                        % +Site, +Data, -Licenses
    ckan_member_list/4,                         % +Site, +Group, +Data, -Triples
    ckan_organization_list/3,                   % +Site, +Data, -Orgs
    ckan_organization_list_for_user/3,          % +Site, +Data, -Orgs
    ckan_organization_revision_list/4,          % +Site, +Org, +Data, -Revs
    ckan_organization_show/4,                   % +Site, +Org, +Data, -M
    ckan_package_autocomplete/4,                % +Site, +Query, +Data, -Packs
    ckan_package_list/3,                        % +Site, +Data, -Packs
    ckan_package_relationships_list/5,          % +Site, +Pack1, +Pack2, +Data, -Rels
    ckan_package_revision_list/4,               % +Site, +Pack,  +Data, -Revs
    ckan_package_show/4,                        % +Site, +Pack,  +Data, -M
    ckan_resource_show/4,                       % +Site, +Res, +Data, -M
    ckan_resource_status_show/4,                % +Site, +Res, +Data, -Statuses
    ckan_resource_view_list/4,                  % +Site, +Res, +Data, -ResViews
    ckan_resource_view_show/4,                  % +Site, +ResView, +Data, -M
    ckan_revision_list/3,                       % +Site, +Data, -Revs
    ckan_revision_show/4,                       % +Site, +Rev, +Data, -M
    ckan_request/4,                             % +Site, +Uri, +Action, +Args
    ckan_request/5,                             % +Site, +Uri, +Action, +Args, +Opts
    ckan_site/1,                                % -Site
    ckan_site_read/1,                           % +Site
    ckan_site_url/1,                            % -Uri
    ckan_tag_list/3,                            % +Site, +Args, -Tags
    ckan_tag_show/4,                            % +Site, +Tag, +Data, -M
    ckan_user_list/3,                           % +Site, +Data, -Users
    ckan_user_show/4                            % +Site, +User, +Data, -M
  ]
).

/** <module> CKAN API

# Abbreviations

| **Abbreviation** | **Expansion** |
|:----------------:|:-------------:|
| Org              | Organization  |
| Pack             | Package       |
| Rev              | Revision      |

@author Wouter Beek
@compat Based on CKAN API 2.6.0
@see http://docs.ckan.org/en/latest/api.html
@version 2016/03-2016/04, 2017/01
*/

:- use_module(library(apply)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_io)).
:- use_module(library(http/json)).
:- use_module(library(json_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(uri/uri_ext)).





%! ckan_current_package_list_with_resources(+Site, +Args, -Packs) is det.
%
% Return a list of the site's datasets (packages) and their resources.
%
% The following arguments are supported:
%
%   * limit(+nonneg)
%
%   * offset(+nonneg)

ckan_current_package_list_with_resources(Site, Args, Packs) :-
  ckan_request(Site, current_package_list_with_resources, Args, Packs).



%! ckan_request(+Uri, +Action, -Result) is det.
%! ckan_request(+Uri, +Action, +Args, -Result) is det.
%! ckan_request(+Uri, +Action, +Args, -Result, +Opts) is det.
%
% The following options are supported:
%
%   * version(+positive_integer)
%
%     Optional, using the server-side default otherwise.

ckan_request(Uri, Action, Result) :-
  ckan_request(Uri, Action, _{}, Result).


ckan_request(Uri, Action, Args, Result) :-
  ckan_request(Uri, Action, Args, Result, []).


ckan_request(Uri1, Action, Args, Result, Opts1) :-
  merge_options(Opts1, [request_header('Accept'='application/json')], Opts2),
  uri_comps(Uri1, uri(Scheme,Auth,Segments1,_,_)),
  ignore(option(version(Version), Opts1)),
  include(ground, [api,Version,action,Action], Segments2),
  append(Segments1, Segments2, Segments3),
  uri_comps(Uri2, uri(Scheme,Auth,Segments3,Args,_)),
  http_get(Uri2, ckan_reply(Result), Opts2),
  (boolean(Result) -> !, true ; Result == [] -> !, true ; true).

ckan_reply(Result, In, InPath, InPath) :-
  json_read_dict(In, Reply),
  (   dict_has_key(error, Reply)
  ->  throw(error(Reply.error.'__type',context(Reply.help,Reply.error.message)))
  ;   dict_get(result, Reply, Result)
  ).

boolean(false).
boolean(true).



%! ckan_site(-Site) is nondet.

ckan_site(Site) :-
  json_read_any('http://instances.ckan.org/config/instances.json', Sites),
  member(Site, Sites).



%! ckan_site_url(-Uri) is det.

ckan_site_url(Uri) :-
  ckan_site(Site),
  (get_dict('url-api', Site, Uri0) -> true ; get_dict(url, Site, Uri0)),
  atom_string(Uri, Uri0).



%! ckan_site_read(+Uri) is semidet.

ckan_site_read(Uri) :-
  ckan_request(Uri, site_read, true).



%! ckan_tag_list(+Uri, +Data, -Tags) is det.
%
% Returns a list of the site's tags.
%
% By default only free tags (tags that don't belong to a vocabulary)
% are returned.  If the ‘vocabulary_id’ argument is given then only
% tags belonging to that vocabulary will be returned instead.
%
% The following arguments are supported:
%
%   * all_fields(+boolean)
%
%     Return full tag dictionaries instead of just names.  Default is
%     `false`.
%
%   * query(string)
%
%     A tag name query to search for, if given only tags whose names
%     contain this string will be returned.
%
%   * vocabulary_id(+string)
%
%     The id or name of a vocabulary, if given only tags that belong
%     to this vocabulary will be returned.

ckan_tag_list(Site, Args, Tags) :-
  ckan_request(Uri, tag_list, Args, Tags).
