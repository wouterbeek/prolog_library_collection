:- module(
  uri_ext,
  [
    relative_uri_path/3, % ?Uri:atom
                         % ?RelativeTo:atom
                         % ?RelativeUri:atom
    uri_path/2, % +PathComponents:list
                % -Path:atom
    uri_authority_directory/2, % +Uri:atom
                               % -Directory:atom
    uri_file_extension/2, % +Uri:atom
                          % ?FileExtension:atom
    uri_flat_directory/3, % +ParentDirectory:atom
                          % +Uri:atom
                          % -UriDirectory:atom
    uri_nested_directory/3, % +ParentDirectory:atom
                            % +Uri:atom
                            % -Directory:atom
    uri_nested_file/2 % +Uri:atom
                      % -File:atom
    uri_nested_file/3 % +ParentDirectory
                      % +Uri:atom
                      % -File:atom
  ]
).

/** <module> URI extensions

@author Wouter Beek
@version 2013/05, 2013/09, 2013/11-2014/04, 2014/08, 2014/11
*/

:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(uri)).

:- use_module(plc(io/dir_ext)).
:- use_module(plc(io/file_ext)).

:- multifile(error:has_type/2).
error:has_type(email, Term):-
  sub_atom(Term, Before, 1, After, '@'),
  Before > 0,
  After > 0.
error:has_type(uri, Term):-
  error:has_type(iri, Term).
error:has_type(iri, Term):-
  uri_components(
    Term,
    uri_components(Scheme,Authority,Path,_Search,_Fragment)
  ),
  maplist(nonvar, [Scheme,Authority,Path]).





%! relative_uri_path(+Url:url, +RelativeTo:url, -RelativeUrl:url) is det.
%! relative_uri_path(-Url:url, +RelativeTo:url, +RelativeUrl:url) is det.

relative_uri_path(Url, RelativeTo1, Relative1):-
  maplist(nonvar, [Url,RelativeTo1]), !,
  uri_components(Url, uri_components(Scheme,Authority,Path,Search,Fragment)),
  uri_components(
    RelativeTo1,
    uri_components(Scheme,Authority,RelativeTo2,'','')
  ),
  relative_file_path(Path, RelativeTo2, Relative2),
  uri_components(Relative1, uri_components('','',Relative2,Search,Fragment)).
relative_uri_path(Url, RelativeTo1, Relative1):-
  maplist(nonvar, [RelativeTo1,Relative1]), !,
  uri_components(
    RelativeTo1,
    uri_components(Scheme,Authority,RelativeTo2,'','')
  ),
  uri_components(
    Relative1,
    uri_components(Scheme,Authority,Relative2,Search,Fragment)
  ),
  relative_file_path(Path, RelativeTo2, Relative2),
  uri_components(Url, uri_components(Scheme,Authority,Path,Search,Fragment)).



%! uri_path(+PathComponents:list(atom), -Path:atom) is det.
% Constructs absolute URI paths out of their constituent components.
%
% # Variable path components
%
% Path components are allowed to be variables.
%
% A sample usage of this is a variable `ApiVersion` which may or may not
% be instantiated with the version number of an online API.
%
% Many Web services automatically resolve paths like [1] to paths like [2].
% ```
% [1]   /api/something
% [2]   /api/default-version/something
% ```

uri_path(PathComponents1, Path):-
  % Exclude the variable components.
  exclude(var, PathComponents1, PathComponents2),

  % A URI path is similar enough to a POSIX path.
  directory_subdirectories(Path, PathComponents2).


%! uri_authority_directory(+Url:atom, -Directory:atom) is det.

uri_authority_directory(Url, Dir):-
  uri_component(Url, scheme, Scheme),
  uri_component(Url, authority, Authority),
  absolute_file_name(data(.), DataDir, [access(read),file_type(directory)]),
  directory_subdirectories(DataDir, DataDirComponents),
  append(DataDirComponents, [Scheme,Authority], DirComponents),
  directory_subdirectories(Dir, DirComponents).


%! uri_file_extension(+Url:url, +FileExtension:atom) is semidet.
%! uri_file_extension(+Url:url, -FileExtension:atom) is semidet.
% Returns the empty atom in case there is no file extension.

uri_file_extension(Url, FileExtension):-
  % Extract the path.
  uri_components(Url, uri_components(_,_,Path,_,_)),

  % Extract the file.
  atomic_list_concat(PathComponents, '/', Path),
  last(PathComponents, FileComponent),

  % Extract the file extensions.
  atomic_list_concat(FileComponents, '.', FileComponent),
  length(FileComponents, Length),
  Length > 1,
  last(FileComponents, FileExtension).


%! uri_flat_directory(
%!   +ParentDirectory:or([atom,compound]),
%!   +Url:url,
%!   -UrlDirectory:atom
%! ) is det.
% Creates a directory for the given URL that is a subdirectory
% of the given parent directory.
%
% This is an easy way to store files related to separate URLs
% in separate places.
%
% This merely gives the directory name,
% but does *not* ensure that the directory exists.
%
% Remote directories are denoted by
% `remote(User:atom,Machine:atom,Directory:atom)`.

uri_flat_directory(
  remote(User,Machine,ParentDir),
  Url,
  remote(User,Machine,UrlDir)
):- !,
  uri_flat_directory(ParentDir, Url, UrlDir).
uri_flat_directory(ParentDir, Url, UrlDir):-
  % A unique name for each URL that does not contain characters
  % that do not comply with POSIX file names.
  rdf_atom_md5(Url, 1, Md5),

  % Make it a subdirectory of the given parent directory
  directory_file_path(ParentDir, Md5, UrlDir).


%! uri_nested_directory(
%!   +ParentDirectory:atom,
%!   +Uri:atom,
%!   -Directory:atom
%! ) is det.
% Returns a nested path denoting a directory
% that is rather similar to the original URI.

uri_nested_directory(ParentDir1, Url, Dir):-
  uri_component(Url, scheme, Scheme),
  uri_component(Url, authority, Authority),
  uri_component(Url, path, Path),
  directory_subdirectories(Path, PathComponents),
  directory_subdirectories(UrlPath, [Scheme,Authority|PathComponents]),
  absolute_file_name(ParentDir1, ParentDir2, [file_type(directory)]),
  relative_file_path(Dir, ParentDir2, UrlPath),
  make_directory_path(Dir).
