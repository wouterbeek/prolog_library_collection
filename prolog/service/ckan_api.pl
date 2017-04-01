:- module(
  ckan_api,
  [
  % HIGH-LEVEL API
    ckan_group/2,        % ?Uri, -Group
    ckan_license/2,      % ?Uri, -License
    ckan_organization/2, % ?Uri, -Org
    ckan_package/2,      % ?Uri, -Pack
    ckan_resource/2,     % ?Uri, -Res
    ckan_site/1,         % -Site
    ckan_site_uri/1,     % -Uri
    ckan_tag/2,          % ?Uri, -Tag
    ckan_user/2,         % ?Uri, -User
  % REST API
    ckan_current_package_list_with_resources/2, % +Uri, -Packs
    ckan_current_package_list_with_resources/3, % +Uri, +Args, -Packs
    ckan_format_autocomplete/3,                 % +Uri, +Query, -Formats
    ckan_format_autocomplete/4,                 % +Uri, +Query, +Args, -Formats
    ckan_group_list/2,                          % +Uri, -Groups
    ckan_group_list/3,                          % +Uri, +Data, -Groups
    ckan_group_list_authz/2,                    % +Uri, -Groups
    ckan_group_list_authz/3,                    % +Uri, +Data, -Groups
    ckan_group_package_show/3,                  % +Uri, +Group, -Packs
    ckan_group_package_show/4,                  % +Uri, +Group, +Data, -Packs
    ckan_group_revision_list/3,                 % +Uri, +Group, -Revs
    ckan_group_revision_list/4,                 % +Uri, +Group, +Data, -Revs
    ckan_group_show/3,                          % +Uri, +Group, -Dict
    ckan_group_show/4,                          % +Uri, +Group, +Data, -Dict
    ckan_license_list/2,                        % +Uri, -Licenses
    ckan_member_list/3,                         % +Uri, +Group, -Triples
    ckan_member_list/4,                         % +Uri, +Group, +Data, -Triples
    ckan_organization_list/2,                   % +Uri, -Orgs
    ckan_organization_list/3,                   % +Uri, +Data, -Orgs
    ckan_organization_list_for_user/2,          % +Uri, -Orgs
    ckan_organization_list_for_user/3,          % +Uri, +Data, -Orgs
    ckan_organization_revision_list/3,          % +Uri, +Org, -Revs
    ckan_organization_show/3,                   % +Uri, +Org, -Dict
    ckan_organization_show/4,                   % +Uri, +Org, +Data, -Dict
    ckan_package_autocomplete/3,                % +Uri, +Query, -Packs
    ckan_package_autocomplete/4,                % +Uri, +Query, +Data, -Packs
    ckan_package_list/2,                        % +Uri, -Packs
    ckan_package_list/3,                        % +Uri, +Data, -Packs
    ckan_package_relationships_list/4,          % +Uri, +Pack1, +Pack2, -Rels
    ckan_package_relationships_list/5,          % +Uri, +Pack1, +Pack2, +Data, -Rels
    ckan_package_revision_list/3,               % +Uri, +Pack, -Revs
    ckan_package_revision_list/4,               % +Uri, +Pack, +Data, -Revs
    ckan_package_show/3,                        % +Uri, +Pack, -Dict
    ckan_package_show/4,                        % +Uri, +Pack, +Data, -Dict
    ckan_resource_show/3,                       % +Uri, +Res, -Dict
    ckan_resource_show/4,                       % +Uri, +Res, +Data, -Dict
    ckan_resource_status_show/3,                % +Uri, +Res, -Statuses
    ckan_resource_status_show/4,                % +Uri, +Res, +Data, -Statuses
    ckan_resource_view_list/3,                  % +Uri, +Res, -ResViews
    ckan_resource_view_list/4,                  % +Uri, +Res, +Data, -ResViews
    ckan_resource_view_show/3,                  % +Uri, +ResView, -Dict
    ckan_resource_view_show/4,                  % +Uri, +ResView, +Data, -Dict
    ckan_revision_list/2,                       % +Uri, -Revs
    ckan_revision_list/3,                       % +Uri, +Data, -Revs
    ckan_revision_show/3,                       % +Uri, +Rev, -Dict
    ckan_site_read/1,                           % +Uri
    ckan_tag_list/2,                            % +Uri, -Tags
    ckan_tag_list/3,                            % +Uri, +Args, -Tags
    ckan_tag_show/3,                            % +Uri, +Tag, -Dict
    ckan_tag_show/4,                            % +Uri, +Tag, +Data, -Dict
    ckan_user_list/2,                           % +Uri, -Users
    ckan_user_list/3,                           % +Uri, +Data, -Users
    ckan_user_show/3,                           % +Uri, +User, -Dict
    ckan_user_show/4                            % +Uri, +User, +Data, -Dict
  ]
).

/** <module> CKAN API

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
@version 2016/04, 2017/01, 2017/03
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(lists)).

:- use_module(library(call_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(uri/uri_ext)).





% HIGH-LEVEL API %

%! ckan_group(+Uri, -Group:dict) is nondet.
%! ckan_group(-Uri, -Group:dict) is nondet.
%
% Enumerates CKAN groups.
%
% The following keys appear:
%   * approval_status
%   * created
%   * description
%   * display_name
%   * extras
%   * groups
%   * id
%   * image_display_url
%   * image_url
%   * is_organization
%   * name
%   * num_followers
%   * package_count
%   * revision_id
%   * state
%   * tags
%   * title
%   * type
%   * users

ckan_group(Uri, Dict) :-
  (var(Uri) -> ckan_site_uri(Uri) ; true),
  ckan_group_list(Uri, _{page_size: 100}, Groups),
  member(Group, Groups),
  ckan_group_show(Uri, Group, Dict).



%! ckan_license(+Uri, -License) is nondet.
%! ckan_license(-Uri, -License) is nondet.
%
% Enumerates CKAN licenses.

ckan_license(Uri, License) :-
  (var(Uri) -> ckan_site_uri(Uri) ; true),
  call_msg([], ckan_license_list(Uri), Licenses),
  member(License, Licenses).



%! ckan_organization(+Uri, -Org) is nondet.
%! ckan_organization(-Uri, -Org) is nondet.
%
% Enumerates CKAN organizations.

ckan_organization(Uri, Dict) :-
  (var(Uri) -> ckan_site_uri(Uri) ; true),
  ckan_organization_list(Uri, _{page_size: 100}, Orgs),
  member(Org, Orgs),
  ckan_organization_show(Uri, Org, Dict).



%! ckan_package(+Uri, -Pack) is nondet.
%! ckan_package(-Uri, -Pack) is nondet.
%
% Enumerates CKAN packages.

ckan_package(Uri, Dict) :-
  (var(Uri) -> ckan_site_uri(Uri) ; true),
  ckan_package_list(Uri, Packs),
  member(Pack, Packs),
  ckan_package_show(Uri, Pack, Dict).



%! ckan_resource(+Uri, -Res) is nondet.
%! ckan_resource(-Uri, -Res) is nondet.

ckan_resource(Uri, Res) :-
  ckan_package(Uri, Pack),
  member(Res, Pack.resources).



%! ckan_site(-Site) is nondet.

ckan_site(Site) :-
  setup_call_cleanup(
    http_open(
      'https://ckan.github.io/ckan-instances/config/instances.json',
      In,
      [request_header('Accept'='application/json'),status_code(Status)]
    ),
    (between(200, 299, Status) -> json_read_dict(In, Sites) ; fail),
    close(In)
  ),
  member(Site, Sites).



%! ckan_site_uri(-Uri) is det.

ckan_site_uri(Uri) :-
  ckan_site(Site),
  (get_dict('url-api', Site, Uri0) -> true ; get_dict(url, Site, Uri0)),
  atom_string(Uri, Uri0).



%! ckan_tag(+Uri, -Tag) is nondet.
%! ckan_tag(-Uri, -Tag) is nondet.
%
% Enumerates CKAN tags.

ckan_tag(Uri, Dict) :-
  (var(Uri) -> ckan_site_uri(Uri) ; true),
  ckan_tag_list(Uri, Tags),
  member(Tag, Tags),
  ckan_tag_show(Uri, Tag, Dict).



%! ckan_user(+Uri, -User) is nondet.
%! ckan_user(-Uri, -User) is nondet.
%
% Enumerates CKAN users.

ckan_user(Uri, Dict) :-
  (var(Uri) -> ckan_site_uri(Uri) ; true),
  ckan_user_list(Uri, Users),
  member(User, Users),
  ckan_user_show(Uri, User, Dict).





% REST API %

%! ckan_current_package_list_with_resources(+Uri, -Packs) is det.
%! ckan_current_package_list_with_resources(+Uri, +Args, -Packs) is det.
%
% Return a list of the site's datasets (packages) and their resources.
%
% The following arguments are supported:
%
%   * limit(+nonneg)
%
%   * offset(+nonneg)

ckan_current_package_list_with_resources(Uri, Packs) :-
  ckan_current_package_list_with_resources(Uri, _{}, Packs).


ckan_current_package_list_with_resources(Uri, Args, Packs) :-
  ckan_request(Uri, current_package_list_with_resources, Args, Packs).



%! ckan_format_autocomplete(+Uri, +Query, -Formats) is det.
%! ckan_format_autocomplete(+Uri, +Query, +Args, -Formats) is det.
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

ckan_format_autocomplete(Uri, Query, Formats) :-
  ckan_format_autocomplete(Uri, Query, _{}, Formats).


ckan_format_autocomplete(Uri, Query, Args1, Formats) :-
  Args2 = Args1.put(_{q: Query}),
  ckan_request(Uri, format_autocomplete, Args2, Formats).



%! ckan_group_list(+Uri, -Groups) is det.
%! ckan_group_list(+Uri, +Args, -Groups) is det.
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

ckan_group_list(Uri, Groups) :-
  ckan_group_list(Uri, _{}, Groups).


ckan_group_list(Uri, Args, Groups) :-
  ckan_request(Uri, group_list, Args, Groups).



%! ckan_group_list_authz(+Uri, -Groups) is det.
%! ckan_group_list_authz(+Uri, +Args, -Groups) is det.
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

ckan_group_list_authz(Uri, Groups) :-
  ckan_group_list_authz(Uri, _{}, Groups).


ckan_group_list_authz(Uri, Args, Groups) :-
  ckan_request(Uri, group_list_authz, Args, Groups).



%! ckan_group_package_show(+Uri, +Group, -Packs) is det.
%! ckan_group_package_show(+Uri, +Group, +Args, -Packs) is det.
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

ckan_group_package_show(Uri, Group, Packs) :-
  ckan_group_package_show(Uri, Group, _{}, Packs).


ckan_group_package_show(Uri, Group, Args1, Packs) :-
  Args2 = Args1.put(_{id: Group}),
  ckan_request(Uri, group_package_show, Args2, Packs).



%! ckan_group_revision_list(+Uri, +Group, -Revs) is det.
%! ckan_group_revision_list(+Uri, +Group, +Args1, -Revs) is det.
%
% Return a group's revisions.
%
% @arg Group The name or id of the group.
% @arg Revs List of dictionaries.

ckan_group_revision_list(Uri, Group, Revs) :-
  ckan_group_revision_list(Uri, Group, _{}, Revs).


ckan_group_revision_list(Uri, Group, Args1, Revs) :-
  Args2 = Args1.put(_{id: Group}),
  ckan_request(Uri, group_revision_list, Args2, Revs).



%! ckan_group_show(+Uri, +Group, -Dict) is det.
%! ckan_group_show(+Uri, +Group, +Args, -Dict) is det.
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

ckan_group_show(Uri, Group, Dict) :-
  ckan_group_show(Uri, Group, _{}, Dict).


ckan_group_show(Uri, Group, Args1, Dict) :-
  Args2 = Args1.put(_{id: Group}),
  ckan_request(Uri, group_show, Args2, Dict).



%! ckan_license_list(+Uri, -Licenses:list(dict)) is det.
%
% Return the list of licenses available for datasets on the site.

ckan_license_list(Uri, Licenses) :-
  ckan_request(Uri, license_list, _{}, Licenses).



%! ckan_member_list(+Uri, +Group, -Triples) is det.
%! ckan_member_list(+Uri, +Group, +Args, -Triples) is det.
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

ckan_member_list(Uri, Group, Triples) :-
  ckan_member_list(Uri, Group, _{}, Triples).


ckan_member_list(Uri, Group, Args1, Triples) :-
  Args2 = Args1.put(_{id: Group}),
  ckan_request(Uri, member_list, Args2, Triples).



%! ckan_organization_list(+Uri, -Orgs) is det.
%! ckan_organization_list(+Uri, +Args, -Orgs) is det.
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

ckan_organization_list(Uri, Orgs) :-
  ckan_organization_list(Uri, _{}, Orgs).


ckan_organization_list(Uri, Args, Orgs) :-
  ckan_request(Uri, organization_list, Args, Orgs).



%! ckan_organization_list_for_user(+Uri, -Orgs) is det.
%! ckan_organization_list_for_user(+Uri, +Args, -Orgs) is det.
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

ckan_organization_list_for_user(Uri, Orgs) :-
  ckan_organization_list_for_user(Uri, _{}, Orgs).


ckan_organization_list_for_user(Uri, Args, Orgs) :-
  ckan_request(Uri, organization_list_for_user, Args, Orgs).



%! ckan_organization_revision_list(+Uri, +Org, -Revs) is det.
%
% Return an organization's revisions.
%
% @arg Org The name or id of the organization.
%
% @arg Revs A list of dictionaries.

ckan_organization_revision_list(Uri, Org, Revs) :-
  ckan_request(Uri, organization_revision_list, _{id: Org}, Revs).



%! ckan_organization_show(+Uri, +Org, -Dict) is det.
%! ckan_organization_show(+Uri, +Org, +Args, -Dict) is det.
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

ckan_organization_show(Uri, Org, Dict) :-
  ckan_organization_show(Uri, Org, _{}, Dict).


ckan_organization_show(Uri, Org, Args1, Dict) :-
  Args2 = Args1.put(_{id: Org}),
  ckan_request(Uri, organization_show, Args2, Dict).



%! ckan_package_autocomplete(+Uri, +Query, -Packs) is det.
%! ckan_package_autocomplete(+Uri, +Query, +Args, -Packs) is det.
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

ckan_package_autocomplete(Uri, Query, Packs) :-
  ckan_package_autocomplete(Uri, Query, _{}, Packs).


ckan_package_autocomplete(Uri, Query, Args1, Packs) :-
  Args2 = Args1.put(_{q: Query}),
  ckan_request(Uri, package_autocomplete, Args2, Packs).



%! ckan_package_list(+Uri, -Packs) is det.
%! ckan_package_list(+Uri, +Args, -Packs) is det.
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

ckan_package_list(Uri, Packs) :-
  ckan_package_list(Uri, _{}, Packs).


ckan_package_list(Uri, Args, Packs) :-
  ckan_request(Uri, package_list, Args, Packs).



%! ckan_package_relationships_list(+Uri, +Pack1, +Pack2, -Rels) is det.
%! ckan_package_relationships_list(+Uri, +Pack1, +Pack2, +Args, -Rels) is det.
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

ckan_package_relationships_list(Uri, Pack1, Pack2, Rels) :-
  ckan_package_relationships_list(Uri, Pack1, Pack2, _{}, Rels).


ckan_package_relationships_list(Uri, Pack1, Pack2, Args1, Rels) :-
  Args2 = Args1.put(_{id: Pack1, id2: Pack2}),
  ckan_request(Uri, package_relationship_list, Args2, Rels).



%! ckan_package_revision_list(+Uri, +Pack, -Revs:list(string)) is det.
%! ckan_package_revision_list(+Uri, +Pack, +Args, -Revs:list(string)) is det.
%
% Return a dataset (package)'s revisions as a list of dictionaries.
%
% @arg Pack The id or name of the dataset.
%
% @arg Revs A list of revision terms.

ckan_package_revision_list(Uri, Pack, Revs) :-
  ckan_package_revision_list(Uri, Pack, _{}, Revs).


ckan_package_revision_list(Uri, Pack, Args1, Revs) :-
  Args2 = Args1.put(_{id: Pack}),
  ckan_request(Uri, package_revision_list, Args2, Revs).



%! ckan_package_show(+Uri, +Pack, -Dict) is det.
%! ckan_package_show(+Uri, +Pack, +Args, -Dict) is det.
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

ckan_package_show(Uri, Pack, Dict) :-
  ckan_package_show(Uri, Pack, _{}, Dict).


ckan_package_show(Uri, Pack, Args1, Dict) :-
  Args2 = Args1.put(_{id: Pack}),
  ckan_request(Uri, package_show, Args2, Dict).



%! ckan_resource_show(+Uri, +Res, -Dict) is det.
%! ckan_resource_show(+Uri, +Res, +Args, -Dict) is det.
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

ckan_resource_show(Uri, Res, Dict) :-
  ckan_resource_show(Uri, Res, _{}, Dict).


ckan_resource_show(Uri, Res, Args1, Dict) :-
  Args2 = Args1.put(_{id: Res}),
  ckan_request(Uri, resource_show, Args2, Dict).



%! ckan_resource_status_show(+Uri, +Res, -Statuses:list(atom)) is det.
%! ckan_resource_status_show(+Uri, +Res, +Args, -Statuses:list(atom)) is det.
%
% Statuses can contain the following atoms: `date_done', `traceback',
% `task_status'.
%
% Return the statuses of a resource's tasks.
%
% @arg Res The id of the resource.
% 
% @deprecated

ckan_resource_status_show(Uri, Res, Statuses) :-
  ckan_resource_status_show(Uri, Res, _{}, Statuses).


ckan_resource_status_show(Uri, Res, Args1, Statuses) :-
  Args2 = Args1.put(_{id: Res}),
  ckan_request(Uri, resource_status_show, Args2, Statuses).



%! ckan_resource_view_list(+Uri, +Res, -ResViews:list(dict)) is det.
%! ckan_resource_view_list(+Uri, +Res, +Args, -ResViews:list(dict)) is det.
%
% Return the list of resource views for a particular resource.
%
% @arg Res The id of the resource.

ckan_resource_view_list(Uri, Res, ResViews) :-
  ckan_resource_view_list(Uri, Res, _{}, ResViews).


ckan_resource_view_list(Uri, Res, Args1, ResViews) :-
  Args2 = Args1.put(_{id: Res}),
  ckan_request(Uri, resource_view_list, Args2, ResViews).



%! ckan_resource_view_show(+Uri, +ResView, -Dict) is det.
%! ckan_resource_view_show(+Uri, +ResView, +Args, -Dict) is det.
%
% Return the metadata of a resource_view.
%
% @arg ResView The id of the resource_view The following arguments are
%      supported:

ckan_resource_view_show(Uri, ResView, Dict) :-
  ckan_resource_view_show(Uri, ResView, _{}, Dict).


ckan_resource_view_show(Uri, ResView, Args1, Dict) :-
  Args2 = Args1.put(_{id: ResView}),
  ckan_request(Uri, resource_view_show, Args2, Dict).



%! ckan_revision_list(+Uri, -Revs:list(string)) is det.
%! ckan_revision_list(+Uri, +Args, -Revs:list(string)) is det.
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

ckan_revision_list(Uri, Revs) :-
  ckan_revision_list(Uri, _{}, Revs).


ckan_revision_list(Uri, Args, Revs) :-
  ckan_request(Uri, revision_list, Args, Revs).



%! ckan_revision_show(+Uri, +Rev, -Dict) is det.
%
% Return the details of a revision.
%
% @arg Rev The ‘id’ of the revision.

ckan_revision_show(Uri, Rev, Dict) :-
  ckan_request(Uri, revision_show, _{id: Rev}, Dict).



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

ckan_tag_list(Uri, Tags) :-
  ckan_tag_list(Uri, _{}, Tags).


ckan_tag_list(Uri, Args, Tags) :-
  ckan_request(Uri, tag_list, Args, Tags).



%! ckan_tag_show(+Uri, +Tag, -Dict) is det.
%! ckan_tag_show(+Uri, +Tag, +Args, -Dict) is det.
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

ckan_tag_show(Uri, Tag, Dict) :-
  ckan_tag_show(Uri, Tag, _{}, Dict).


ckan_tag_show(Uri, Tag, Args1, Dict) :-
  Args2 = Args1.put(_{id: Tag}),
  ckan_request(Uri, tag_show, Args2, Dict).



%! ckan_user_list(+Uri, -Users:list(dict)) is det.
%! ckan_user_list(+Uri, +Args, -Users:list(dict)) is det.
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

ckan_user_list(Uri, Users) :-
  ckan_user_list(Uri, _{}, Users).


ckan_user_list(Uri, Args, Users) :-
  ckan_request(Uri, user_list, Args, Users).



%! ckan_user_show(+Uri, +User, -Dict) is det.
%! ckan_user_show(+Uri, +User, +Args, -Dict) is det.
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

ckan_user_show(Uri, User, Dict) :-
  ckan_user_show(Uri, User, _{}, Dict).


ckan_user_show(Uri, User, Args1, Dict) :-
  (is_dict(User) -> Id = User.id ; Id = User),
  Args2 = Args1.put(_{id: Id}),
  ckan_request(Uri, user_show, Args2, Dict).





% HELPERS %

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
  uri_comps(Uri1, uri(Scheme,Auth,Segments1,_,_)), !,
  (del_dict(version, Args1, Version, Args2) -> true ; Args2 = Args1),
  include(ground, [api,Version,action,Action], Segments2),
  append(Segments1, Segments2, Segments3),
  (   del_dict(page_size, Args2, PageSize, Args3)
  ->  betwixt(0, inf, PageSize, Offset),
      Args4 = Args3.put(_{limit: PageSize, offset: Offset})
  ;   Args4 = Args2
  ),
  (   del_dict(api_key, Args4, Key, Args5)
  ->  Opts = [request_header('Authorization'=Key)]
  ;   Args5 = Args4,
      Opts = []
  ),
  uri_comps(Uri2, uri(Scheme,Auth,Segments3,Args5,_)),
  setup_call_cleanup(
    http_open(
      Uri2,
      In,
      [
        header(content_type, ContentType),
        request_header('Accept'='application/json'),
        status_code(Status)
      | Opts
      ]
    ),
    (   between(200, 299, Status)
    ->  http_parse_header_value(content_type, ContentType, MT),
        ckan_request_stream(In, MT, Result)
    ;   fail
    ),
    close(In)
  ).
ckan_request(Uri, _, _, _) :-
  type_error(uri, Uri).

ckan_request_stream(In, media(application/json,_), Result) :- !,
  json_read_dict(In, Reply),
  (   \+ is_dict(Reply)
  ->  type_error(ckan_reply, Reply)
  ;   dict_key(Reply, error)
  ->  throw(error(Reply.error.'__type',context(Reply.help,Reply.error.message)))
  ;   dict_get(result, Reply, Result),
      (boolean(Result) -> !, true ; Result == [] -> !, true ; true)
  ).
ckan_request_stream(In, _, _) :-
  peek_string(In, 50, Str),
  throw(error(no_json(Str),context(ckan_request_stream,ckan_api))).

boolean(false).
boolean(true).
