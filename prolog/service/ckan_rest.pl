:- module(
  ckan_rest,
  [
    ckan_current_package_list_with_resources/2, % +Site, -Packs
    ckan_current_package_list_with_resources/3, % +Site, +Args, -Packs
    ckan_format_autocomplete/3,                 % +Site, +Query, -Formats
    ckan_format_autocomplete/4,                 % +Site, +Query, +Args, -Formats
    ckan_group_list/2,                          % +Site, -Groups
    ckan_group_list/3,                          % +Site, +Data, -Groups
    ckan_group_list_authz/2,                    % +Site, -Groups
    ckan_group_list_authz/3,                    % +Site, +Data, -Groups
    ckan_group_package_show/3,                  % +Site, +Group, -Packs
    ckan_group_package_show/4,                  % +Site, +Group, +Data, -Packs
    ckan_group_revision_list/3,                 % +Site, +Group, -Revs
    ckan_group_revision_list/4,                 % +Site, +Group, +Data, -Revs
    ckan_group_show/3,                          % +Site, +Group, -Dict
    ckan_group_show/4,                          % +Site, +Group, +Data, -Dict
    ckan_license_list/2,                        % +Site, -Licenses
    ckan_member_list/3,                         % +Site, +Group, -Triples
    ckan_member_list/4,                         % +Site, +Group, +Data, -Triples
    ckan_organization_list/2,                   % +Site, -Orgs
    ckan_organization_list/3,                   % +Site, +Data, -Orgs
    ckan_organization_list_for_user/2,          % +Site, -Orgs
    ckan_organization_list_for_user/3,          % +Site, +Data, -Orgs
    ckan_organization_revision_list/3,          % +Site, +Org, -Revs
    ckan_organization_show/3,                   % +Site, +Org, -Dict
    ckan_organization_show/4,                   % +Site, +Org, +Data, -Dict
    ckan_package_autocomplete/3,                % +Site, +Query, -Packs
    ckan_package_autocomplete/4,                % +Site, +Query, +Data, -Packs
    ckan_package_list/2,                        % +Site, -Packs
    ckan_package_list/3,                        % +Site, +Data, -Packs
    ckan_package_relationships_list/4,          % +Site, +Pack1, +Pack2, -Rels
    ckan_package_relationships_list/5,          % +Site, +Pack1, +Pack2, +Data, -Rels
    ckan_package_revision_list/3,               % +Site, +Pack, -Revs
    ckan_package_revision_list/4,               % +Site, +Pack, +Data, -Revs
    ckan_package_show/3,                        % +Site, +Pack, -Dict
    ckan_package_show/4,                        % +Site, +Pack, +Data, -Dict
    ckan_resource_show/3,                       % +Site, +Res, -Dict
    ckan_resource_show/4,                       % +Site, +Res, +Data, -Dict
    ckan_resource_status_show/3,                % +Site, +Res, -Statuses
    ckan_resource_status_show/4,                % +Site, +Res, +Data, -Statuses
    ckan_resource_view_list/3,                  % +Site, +Res, -ResViews
    ckan_resource_view_list/4,                  % +Site, +Res, +Data, -ResViews
    ckan_resource_view_show/3,                  % +Site, +ResView, -Dict
    ckan_resource_view_show/4,                  % +Site, +ResView, +Data, -Dict
    ckan_revision_list/2,                       % +Site, -Revs
    ckan_revision_list/3,                       % +Site, +Data, -Revs
    ckan_revision_show/3,                       % +Site, +Rev, -Dict
    ckan_site_read/1,                           % +Site
    ckan_tag_list/2,                            % +Site, -Tags
    ckan_tag_list/3,                            % +Site, +Args, -Tags
    ckan_tag_show/3,                            % +Site, +Tag, -Dict
    ckan_tag_show/4,                            % +Site, +Tag, +Data, -Dict
    ckan_user_list/2,                           % +Site, -Users
    ckan_user_list/3,                           % +Site, +Data, -Users
    ckan_user_show/3,                           % +Site, +User, -Dict
    ckan_user_show/4                            % +Site, +User, +Data, -Dict
  ]
).

/** <module> CKAN REST API

For most purposes the CKAN Prolog API (‘service/ckan_api’) should be
used instead.

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
:- use_module(library(http/http_header)).
:- use_module(library(http/http_io)).
:- use_module(library(http/json)).
:- use_module(library(json_ext)).
:- use_module(library(lists)).
:- use_module(library(math/math_ext)).
:- use_module(library(option)).
:- use_module(library(print_ext)).
:- use_module(library(uri/uri_ext)).





%! ckan_current_package_list_with_resources(+Site, -Packs) is det.
%! ckan_current_package_list_with_resources(+Site, +Args, -Packs) is det.
%
% Return a list of the site's datasets (packages) and their resources.
%
% The following arguments are supported:
%
%   * limit(+nonneg)
%
%   * offset(+nonneg)

ckan_current_package_list_with_resources(Site, Packs) :-
  ckan_current_package_list_with_resources(Site, _{}, Packs).


ckan_current_package_list_with_resources(Site, Args, Packs) :-
  ckan_request(Site, current_package_list_with_resources, Args, Packs).



%! ckan_format_autocomplete(+Site, +Query, -Formats) is det.
%! ckan_format_autocomplete(+Site, +Query, +Args, -Formats) is det.
%  
% Return a list of resource formats whose names contain a string.
%
% @arg Query The string to search for.
%
% The following arguments are supported:
%
%   * limit(+nonneg)
%
%     The maximum number of resource formats to return.  Default is 5.

ckan_format_autocomplete(Site, Query, Formats) :-
  ckan_format_autocomplete(Site, Query, _{}, Formats).


ckan_format_autocomplete(Site, Query, Args1, Formats) :-
  Args2 = Args1.put(_{q: Query}),
  ckan_request(Site, format_autocomplete, Args2, Formats).



%! ckan_group_list(+Site, -Groups) is det.
%! ckan_group_list(+Site, +Args, -Groups) is det.
%
% Return a list of the names of the site's groups.
% 
% The following arguments are supported:
%
%   * all_fields(+boolean)
%
%     Return group dictionaries instead of just names.  Only core
%     fields are returned - get some more using the ‘include_*’
%     options.  Returning a list of packages is too expensive, so the
%     packages property for each group is deprecated, but there is a
%     count of the packages in the ‘package_count’ property.  Default
%     is ‘false’.
%
%   * groups(+list(string))
%
%     A list of names of the groups to return, if given only groups
%     whose names are in this list will be returned.
%
%   * include_extras(+boolean)
%
%     If ‘all_fields’, include the group extra fields.  Default is
%     ‘false’.
%
%   * include_groups(+boolean)
%
%     If ‘all_fields’, include the groups the groups are in.  Default
%     is ‘false’.
%
%   * include_tags(+boolean)
%
%     If ‘all_fields’, include the group tags.  Default is ‘false’.
%
%   * include_users(+boolean)
%
%     If ‘all_fields’, include the group users.  Default is ‘false’.
%
%   * limit(+nonneg)
%
%   * offset(+nonneg)
%
%   * sort(+string)

ckan_group_list(Site, Groups) :-
  ckan_group_list(Site, _{}, Groups).


ckan_group_list(Site, Args, Groups) :-
  ckan_request(Site, group_list, Args, Groups).



%! ckan_group_list_authz(+Site, -Groups) is det.
%! ckan_group_list_authz(+Site, +Args, -Groups) is det.
%
% Return the list of groups that the user is authorized to edit.
%
% The following arguments are supported:
%
%   * am_member(+boolean)
%
%     If ‘true’ return only the groups the logged-in user is a member
%     of, otherwise return all groups that the user is authorized to
%     edit (for example, sysadmin users are authorized to edit all
%     groups).  Default is ‘false’.
%
%   * available_only(+boolean)
%
%     Remove the existing groups in the package.  Default is ‘false’.
%
% @arg Groups List of dictized groups that the user is authorized to
%      edit.

ckan_group_list_authz(Site, Groups) :-
  ckan_group_list_authz(Site, _{}, Groups).


ckan_group_list_authz(Site, Args, Groups) :-
  ckan_request(Site, group_list_authz, Args, Groups).



%! ckan_group_package_show(+Site, +Group, -Packs) is det.
%! ckan_group_package_show(+Site, +Group, +Args, -Packs) is det.
%
% Return the datasets (packages) of a group.
%
% @arg Group The id or name of the group.
%
% The following arguments are supported:
%
%   * limit(+nonneg)
%
%     The maximum number of datasets to return.

ckan_group_package_show(Site, Group, Packs) :-
  ckan_group_package_show(Site, Group, _{}, Packs).


ckan_group_package_show(Site, Group, Args1, Packs) :-
  Args2 = Args1.put(_{id: Group}),
  ckan_request(Site, group_package_show, Args2, Packs).



%! ckan_group_revision_list(+Site, +Group, -Revs) is det.
%! ckan_group_revision_list(+Site, +Group, +Args1, -Revs) is det.
%
% Return a group's revisions.
%
% @arg Group The name or id of the group.
% @arg Revs List of dictionaries.

ckan_group_revision_list(Site, Group, Revs) :-
  ckan_group_revision_list(Site, Group, _{}, Revs).


ckan_group_revision_list(Site, Group, Args1, Revs) :-
  Args2 = Args1.put(_{id: Group}),
  ckan_request(Site, group_revision_list, Args2, Revs).



%! ckan_group_show(+Site, +Group, -Dict) is det.
%! ckan_group_show(+Site, +Group, +Args, -Dict) is det.
%
% Return the details of a group.
%
% @arg Group The id or name of the group.
%
% The following arguments are supported:
%
%   * include_datasets(+boolean)
%
%     Include a list of the group's datasets.  Default is ‘false’.
%
%   * include_extras(+boolean)
%
%     Include the group's extra fields.  Default is ‘true’.
%
%   * include_followers(+boolean)
%
%     Include the group's number of followers.  Default is ‘true’.
%
%   * include_groups(+boolean)
%
%     Include the group's sub groups.  Default is ‘true’.
%
%   * include_tags(+boolean)
%
%     Include the group's tags.  Default is ‘true’.
%
%   * include_users(+boolean)
%
%     Include the group's users.  Default is ‘true’.
%
% @arg Dict Only its first 1000 datasets are returned

ckan_group_show(Site, Group, Dict) :-
  ckan_group_show(Site, Group, _{}, Dict).


ckan_group_show(Site, Group, Args1, Dict) :-
  Args2 = Args1.put(_{id: Group}),
  ckan_request(Site, group_show, Args2, Dict).



%! ckan_license_list(+Site, -Licenses:list(dict)) is det.
%
% Return the list of licenses available for datasets on the site.

ckan_license_list(Site, Licenses) :-
  ckan_request(Site, license_list, _{}, Licenses).



%! ckan_member_list(+Site, +Group, -Triples) is det.
%! ckan_member_list(+Site, +Group, +Args, -Triples) is det.
%
% Return the members of a group.
%
% The user must have permission to ‘get’ the group.
%
% @arg Group The id or name of the group.
%
% The following arguments are supported:
%
%   * capacity(+string)
%
%     Restrict the members returned to those with a given capacity,
%     e.g. ‘"member"’, ‘"editor"’, ‘"admin"’, ‘"public"’, ‘"private"’
%     Default is ‘"None"’.
%
%   * object_type(+string)
%
%     Restrict the members returned to those of a given type,
%     e.g. ‘"user"’ or ‘"package"’.  Default is ‘"None"’.
%
% @arg Triples A list of 〈id,type,capacity〉 triples.

ckan_member_list(Site, Group, Triples) :-
  ckan_member_list(Site, Group, _{}, Triples).


ckan_member_list(Site, Group, Args1, Triples) :-
  Args2 = Args1.put(_{id: Group}),
  ckan_request(Site, member_list, Args2, Triples).



%! ckan_organization_list(+Site, -Orgs) is det.
%! ckan_organization_list(+Site, +Args, -Orgs) is det.
%
% Return a list of the names of the site's organizations.
%
% The following arguments are supported:
%
%   * all_fields(+boolean)
%
%     return group dictionaries instead of just names. Only core
%     fields are returned - get some more using the include_*
%     options. Returning a list of packages is too expensive, so the
%     packages property for each group is deprecated, but there is a
%     count of the packages in the package_count property. (optional,
%     default: False)
%
%   * include_extras(+boolean)
%
%     If ‘all_fields’, include the organization extra fields.  Default
%     is ‘false’.
%
%   * include_groups(+boolean)
%
%     If ‘all_fields’, include the organizations the organizations are
%     in.  Default is ‘false’.
%
%   * include_tags(+boolean)
%
%     If ‘all_fields’, include the organization tags.  Default is
%     ‘false’.
%
%   * include_users(+boolean)
%
%     If ‘all_fields’, include the organization users.  Default is
%     ‘false’.
%
%   * limit(+nonneg)
%
%   * offset(+nonneg)
%
%   * organizations(+list(string))
%
%     A list of names of the groups to return, if given only groups
%     whose names are in this list will be returned.
%
%   * sort(+string)

ckan_organization_list(Site, Orgs) :-
  ckan_organization_list(Site, _{}, Orgs).


ckan_organization_list(Site, Args, Orgs) :-
  ckan_request(Site, organization_list, Args, Orgs).



%! ckan_organization_list_for_user(+Site, -Orgs) is det.
%! ckan_organization_list_for_user(+Site, +Args, -Orgs) is det.
%
% Return the organizations that the user has a given permission for.
%
% By default this returns the list of organizations that the currently
% authorized user can edit, i.e. the list of organizations that the
% user is an admin of.  Specifically it returns the list of
% organizations that the currently authorized user has a given
% permission (for example: ‘"manage_group"’) against.
%
% When a user becomes a member of an organization in CKAN they’re
% given a “capacity” (sometimes called a “role”), for example
% ‘"member"’, ‘"editor"’ or ‘"admin"’.  Each of these roles has
% certain permissions associated with it.  For example the admin role
% has the ‘"admin"’ permission (which means they have permission to do
% anything).  The editor role has permissions like ‘"create_dataset"’,
% ‘"update_dataset"’ and ‘"delete_dataset"’.  The member role has the
% ‘"read"’ permission.
%
% This function returns the list of organizations that the authorized
% user has a given permission for.  For example the list of
% organizations that the user is an admin of, or the list of
% organizations that the user can create datasets in.  This takes
% account of when permissions cascade down an organization hierarchy.
%
% The following arguments are supported:
%
%   * permission(+string)
%
%     The permission the user has against the returned organizations,
%     for example ‘"read"’ or ‘"create_dataset"’.  Default is
%     ‘"edit_group"’.
%
% @arg Orgs A list of organizations that the user has the given
%      permission for.

ckan_organization_list_for_user(Site, Orgs) :-
  ckan_organization_list_for_user(Site, _{}, Orgs).


ckan_organization_list_for_user(Site, Args, Orgs) :-
  ckan_request(Site, organization_list_for_user, Args, Orgs).



%! ckan_organization_revision_list(+Site, +Org, -Revs) is det.
%
% Return an organization's revisions.
%
% @arg Org The name or id of the organization.
%
% @arg Revs A list of dictionaries.

ckan_organization_revision_list(Site, Org, Revs) :-
  ckan_request(Site, organization_revision_list, _{id: Org}, Revs).



%! ckan_organization_show(+Site, +Org, -Dict) is det.
%! ckan_organization_show(+Site, +Org, +Args, -Dict) is det.
%
% Return the details of a organization.
%
% @arg Org The id or name of the organization.
%
% The following arguments are supported:
%
%   * include_datasets(+boolean)
%
%     Include a list of the group's datasets.  Default is ‘false’.
%
%   * include_extras(+boolean)
%
%     Include the group's extra fields.  Default is ‘true’.
%
%   * include_followers(+boolean)
%
%     Include the group's number of followers.  Default is ‘true’.
%
%   * include_groups(+boolean)
%
%     Include the group's sub groups.  Default is ‘true’.
%
%   * include_tags(+boolean)
%
%     Include the group's tags.  Default is ‘true’.
%
%   * include_users(+boolean)
%
%     Include the group's users.  Default is ‘true’.
%
% @arg Dict Only its first 1,000 datasets are returned

ckan_organization_show(Site, Org, Dict) :-
  ckan_organization_show(Site, Org, _{}, Dict).


ckan_organization_show(Site, Org, Args1, Dict) :-
  Args2 = Args1.put(_{id: Org}),
  ckan_request(Site, organization_show, Args2, Dict).



%! ckan_package_autocomplete(+Site, +Query, -Packs) is det.
%! ckan_package_autocomplete(+Site, +Query, +Args, -Packs) is det.
%
% Return a list of datasets (packages) that match a string.
%
% Argssets with names or titles that contain the query string will be returned.
%
% @arg Query The string to search for.
%
% The following arguments are supported:
%
%   * limit(+nonneg)
%
%     The maximum number of resource formats to return (optional,
%     default: 10).
%
% @arg Packs list of dictionaries.

ckan_package_autocomplete(Site, Query, Packs) :-
  ckan_package_autocomplete(Site, Query, _{}, Packs).


ckan_package_autocomplete(Site, Query, Args1, Packs) :-
  Args2 = Args1.put(_{q: Query}),
  ckan_request(Site, package_autocomplete, Args2, Packs).



%! ckan_package_list(+Site, -Packs) is det.
%! ckan_package_list(+Site, +Args, -Packs) is det.
%
% Return a list of the names of the site's datasets (packages).
%
% The following arguments are supported:
%
%      * limit(+nonneg)
%
%      * offset(+nonneg)
%
% @arg Packs A list of strings representing package/dataset names.
% The list is sorted by modification data (most recently modified
% first).

ckan_package_list(Site, Packs) :-
  ckan_package_list(Site, _{}, Packs).


ckan_package_list(Site, Args, Packs) :-
  ckan_request(Site, package_list, Args, Packs).



%! ckan_package_relationships_list(+Site, +Pack1, +Pack2, -Rels) is det.
%! ckan_package_relationships_list(+Site, +Pack1, +Pack2, +Args, -Rels) is det.
%
% Return a dataset (package)'s relationships.
%
% @arg Pack1 The id or name of the first package.
%
% @arg Pack2 The id or name of the second package.
%
% The following arguments are supported:
%
%   * rel(+string)
%
%     Rel as string.  See ‘package_relationship_create()’ for the
%     relationship types.
%
% @arg Rels List of dictionaries.

ckan_package_relationships_list(Site, Pack1, Pack2, Rels) :-
  ckan_package_relationships_list(Site, Pack1, Pack2, _{}, Rels).


ckan_package_relationships_list(Site, Pack1, Pack2, Args1, Rels) :-
  Args2 = Args1.put(_{id: Pack1, id2: Pack2}),
  ckan_request(Site, package_relationship_list, Args2, Rels).



%! ckan_package_revision_list(+Site, +Pack, -Revs:list(string)) is det.
%! ckan_package_revision_list(+Site, +Pack, +Args, -Revs:list(string)) is det.
%
% Return a dataset (package)'s revisions as a list of dictionaries.
%
% @arg Pack The id or name of the dataset.
%
% @arg Revs A list of revision terms.

ckan_package_revision_list(Site, Pack, Revs) :-
  ckan_package_revision_list(Site, Pack, _{}, Revs).


ckan_package_revision_list(Site, Pack, Args1, Revs) :-
  Args2 = Args1.put(_{id: Pack}),
  ckan_request(Site, package_revision_list, Args2, Revs).



%! ckan_package_show(+Site, +Pack, -Dict) is det.
%! ckan_package_show(+Site, +Pack, +Args, -Dict) is det.
%
% Return the metadata of a dataset (package) and its resources.
%
% @arg Pack The id or name of the dataset.
%
% The following arguments are supported:
%
%   * include_tracking(+boolean)
%
%     Add tracking information to dataset and resources.  Default is
%     ‘false’.
%
%   * use_default_schema(+boolean)
%
%     Use default package schema instead of a custom schema defined
%     with an ‘IArgssetForm’ plugin.  Default is ‘false’.

ckan_package_show(Site, Pack, Dict) :-
  ckan_package_show(Site, Pack, _{}, Dict).


ckan_package_show(Site, Pack, Args1, Dict) :-
  Args2 = Args1.put(_{id: Pack}),
  ckan_request(Site, package_show, Args2, Dict).



%! ckan_request(+Uri, +Action, -Result) is nondet.
%! ckan_request(+Uri, +Action, +Args, -Result) is nondet.
%
% The arguments are supported:
%
%   * api_key(+atom)
%
%   * page_size(+nonneg)
%
%     Sets the ‘limit’ and ‘offset’ arguments and iterates over all
%     result pages.
%
%   * version(+positive_integer)
%
%     Optional, using the server-side default otherwise.

ckan_request(Uri, Action, Result) :-
  ckan_request(Uri, Action, _{}, Result).


ckan_request(Uri1, Action, Args1, Result) :-
  uri_comps(Uri1, uri(Scheme,Auth,Segments1,_,_)),
  (del_dict(version, Args1, Version, Args2) -> true ; Args2 = Args1),
  include(ground, [api,Version,action,Action], Segments2),
  append(Segments1, Segments2, Segments3),
  (   del_dict(page_size, Args2, PageSize, Args3)
  ->  betwixt(0, inf, PageSize, Offset),
      Args4 = Args3.put(_{limit: PageSize, offset: Offset})
  ;   Args4 = Args2
  ),
  (   del_dict(api_key, Args4, Key, Args5)
  ->  Opts1 = [request_header('Authorization'=Key)]
  ;   Args5 = Args4,
      Opts1 = []
  ),
  uri_comps(Uri2, uri(Scheme,Auth,Segments3,Args5,_)),
  merge_options(Opts1, [request_header('Accept'='application/json')], Opts2),
  http_get(Uri2, ckan_reply(Uri2, Result), Opts2),
  (boolean(Result) -> !, true ; Result == [] -> !, true ; true).

ckan_reply(_, Result, In, InPath, InPath) :-
  dicts_getchk(headers, InPath, Headers),
  dict_get('content-type', Headers, Val),
  http_parse_header_value(content_type, Val, media(application/json,_)), !,
  json_read_dict(In, Reply),
  (   dict_key(Reply, error)
  ->  throw(error(Reply.error.'__type',context(Reply.help,Reply.error.message)))
  ;   dict_get(result, Reply, Result)
  ).
ckan_reply(Uri, _, In, InPath, InPath) :-
  peek_string(In, 50, Str),
  msg_warning("[CKAN] No JSON from site ‘~a’ (~s)~n", [Uri,Str]).

boolean(false).
boolean(true).



%! ckan_resource_show(+Site, +Res, -Dict) is det.
%! ckan_resource_show(+Site, +Res, +Args, -Dict) is det.
%
% Return the metadata of a resource.
%
% @arg Res The id of the resource.
%
% The following arguments are supported:
%
%   * include_tracking(+boolean)
%
%     Add tracking information to dataset and resources.  Default is
%     ‘false’.

ckan_resource_show(Site, Res, Dict) :-
  ckan_resource_show(Site, Res, _{}, Dict).


ckan_resource_show(Site, Res, Args1, Dict) :-
  Args2 = Args1.put(_{id: Res}),
  ckan_request(Site, resource_show, Args2, Dict).



%! ckan_resource_status_show(
%!   +Site,
%!   +Res,
%!   -Statuses:list(oneof([date_done,traceback,task_status]))
%! ) is det.
%! ckan_resource_status_show(
%!   +Site,
%!   +Res,
%!   +Args,
%!   -Statuses:list(oneof([date_done,traceback,task_status]))
%! ) is det.
%
% Return the statuses of a resource's tasks.
%
% @arg Res The id of the resource.
% 
% @deprecated

ckan_resource_status_show(Site, Res, Statuses) :-
  ckan_resource_status_show(Site, Res, _{}, Statuses).


ckan_resource_status_show(Site, Res, Args1, Statuses) :-
  Args2 = Args1.put(_{id: Res}),
  ckan_request(Site, resource_status_show, Args2, Statuses).



%! ckan_resource_view_list(+Site, +Res, -ResViews:list(dict)) is det.
%! ckan_resource_view_list(+Site, +Res, +Args, -ResViews:list(dict)) is det.
%
% Return the list of resource views for a particular resource.
%
% @arg Res The id of the resource.

ckan_resource_view_list(Site, Res, ResViews) :-
  ckan_resource_view_list(Site, Res, _{}, ResViews).


ckan_resource_view_list(Site, Res, Args1, ResViews) :-
  Args2 = Args1.put(_{id: Res}),
  ckan_request(Site, resource_view_list, Args2, ResViews).



%! ckan_resource_view_show(+Site, +ResView, -Dict) is det.
%! ckan_resource_view_show(+Site, +ResView, +Args, -Dict) is det.
%
% Return the metadata of a resource_view.
%
% @arg ResView The id of the resource_view The following arguments are
%      supported:

ckan_resource_view_show(Site, ResView, Dict) :-
  ckan_resource_view_show(Site, ResView, _{}, Dict).


ckan_resource_view_show(Site, ResView, Args1, Dict) :-
  Args2 = Args1.put(_{id: ResView}),
  ckan_request(Site, resource_view_show, Args2, Dict).



%! ckan_revision_list(+Site, -Revs:list(string)) is det.
%! ckan_revision_list(+Site, +Args, -Revs:list(string)) is det.
%
% Return a list of the IDs of the site's revisions.  They are sorted
% with the newest first.
%
% Since the results are limited to 50 IDs, you can page through them
% using parameter ‘since_id’.
%
% The following arguments are supported:
%
%   * since_id(+string)
%
%   The revision ID after which you want the revisions.
%
%   * since_time(+???)
%
%   The timestamp after which you want the revisions.
%
%   * sort(+oneof([time_asc,time_desc]))
%
%   The order to sort the related items in, possible values are
%   ‘time_asc’, ‘time_desc’ (default).
%
% @arg Revs A list of revision IDs, limited to 50 (supposedly).

ckan_revision_list(Site, Revs) :-
  ckan_revision_list(Site, _{}, Revs).


ckan_revision_list(Site, Args, Revs) :-
  ckan_request(Site, revision_list, Args, Revs).



%! ckan_revision_show(+Site, +Rev, -Dict) is det.
%
% Return the details of a revision.
%
% @arg Rev The ‘id’ of the revision.

ckan_revision_show(Site, Rev, Dict) :-
  ckan_request(Site, revision_show, _{id: Rev}, Dict).



%! ckan_site_read(+Uri) is semidet.

ckan_site_read(Uri) :-
  ckan_request(Uri, site_read, true).



%! ckan_tag_list(+Uri, -Tags) is det.
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

ckan_tag_list(Site, Tags) :-
  ckan_tag_list(Site, _{}, Tags).


ckan_tag_list(Site, Args, Tags) :-
  ckan_request(Site, tag_list, Args, Tags).



%! ckan_tag_show(+Site, +Tag, -Dict) is det.
%! ckan_tag_show(+Site, +Tag, +Args, -Dict) is det.
%
% Return the details of a tag and all its datasets.
%
% @arg Tag The name or id of the tag.
%
% The following arguments are supported:
%
%   * include_datasets(+boolean)
%
%     Include a list of the tag's datasets.  (Up to a limit of 1000 -
%     for more flexibility, use ‘package_search’ - see
%     ‘package_search()’ for an example.)  Default is ‘false’.
%
%   * vocabulary_id(+string)
%
%     The id or name of the tag vocabulary that the tag is in - if it
%     is not specified it will assume it is a free tag.
%
% @arg Dict The details of the tag, including a list of all of the
%      tag's datasets and their details.

ckan_tag_show(Site, Tag, Dict) :-
  ckan_tag_show(Site, Tag, _{}, Dict).


ckan_tag_show(Site, Tag, Args1, Dict) :-
  Args2 = Args1.put(_{id: Tag}),
  ckan_request(Site, tag_show, Args2, Dict).



%! ckan_user_list(+Site, -Users:list(dict)) is det.
%! ckan_user_list(+Site, +Args, -Users:list(dict)) is det.
%
% Return a list of the site's user accounts.
%
% The following arguments are supported:
%
%   * all_fields(+boolean)
%
%     Return full user dictionaries instead of just names. (optional,
%     default: ‘true’).
%
%   * order_by (string)
%
%     Which field to sort the list by.  Can be any user field or edits
%     (i.e. ‘number_of_edits’).  Default is ‘"name"’.
%
%   * q(+string)
%
%     Restrict the users returned to those whose names contain a
%     string.
%
% @arg Users List of user dictionaries.  User properties include:
%      ‘number_of_edits’ which counts the revisions by the user and
%      ‘number_created_packages’ which excludes datasets which are
%      private or draft state.

ckan_user_list(Site, Users) :-
  ckan_user_list(Site, _{}, Users).


ckan_user_list(Site, Args, Users) :-
  ckan_request(Site, user_list, Args, Users).



%! ckan_user_show(+Site, +User, -Dict) is det.
%! ckan_user_show(+Site, +User, +Args, -Dict) is det.
%
% Return a user account.
%
% @arg User Either the ‘id’ or the ‘user_obj’ parameter (a dictionary)
%      must be given.
%
% The following arguments are supported:
%
%   * include_datasets(+boolean)
%
%     Include a list of datasets the user has created.  If it is the
%     same user or a sysadmin requesting, it includes datasets that
%     are draft or private.  Default is ‘false’.  Limit is 50.
%
%   * include_num_followers(+boolean)
%
%     Include the number of followers the user has.  Default is
%     ‘false’.
%
%   * include_password_hash(+boolean)
%
%     Include the stored password hash.  Sysadmin only.  Default is
%     ‘false’.
%
% @arg Dict The details of the user.  Includes ‘email_hash’,
%      ‘number_of_edits’ and ‘number_created_packages’ (which excludes
%      draft or private datasets unless it is the same user or
%      sysadmin making the request).  Excludes the password (hash) and
%      ‘reset_key’.  If it is the same user or a sysadmin requesting,
%      the email and apikey are included.

ckan_user_show(Site, User, Dict) :-
  ckan_user_show(Site, User, _{}, Dict).


ckan_user_show(Site, User, Args1, Dict) :-
  (is_dict(User) -> Id = User.id ; Id = User),
  Args2 = Args1.put(_{id: Id}),
  ckan_request(Site, user_show, Args2, Dict).
