:- module(
  lexvo,
  [
    find_country/3, % +Options:list(nvpair)
                    % ?String:atom
                    % ?Country:uri
    find_language/3 % +Options:list(nvpair)
                    % ?String:atom
                    % ?Language:uri
  ]
).

/** <module> Lexvo

Lexvo standard for natural language URIs. Version 2012/03/04.

WWW: [http://www.lexvo.org/index.html]

See also: [http://www.w3.org/wiki/Languages_as_RDF_Resources]

@author Wouter Beek
@version 2013/01
*/

:- use_module(pgc(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdfs(rdfs_read)).

:- rdf_register_prefix(lexvo, 'http://lexvo.org/id/').

:- debug(lexvo).



%% find_country(+Options:list(nvpair), +Query:atom, -Country:uri) is nondet.
% Finds and returns the language URI that is closest to the given query.
%
% @param Options A list of name-value pairs.
%        1. =language(Code:atom)= a language code.
% @param Query
% @param Country

find_country(Options, Query, Country):-
  load,
  find_resource(Options, Query, Country).

%% find_language(+Options:list(nvpair), +Query:atom, -Language:uri) is nondet.
% Finds and returns the language URI that is closest to the given query.
%
% @param Options A list of name-value pairs.
%        1. =language(Code:atom)= a language code.
% @param Query
% @param Language

find_language(Options, Query, Language):-
  load,
  find_resource(Options, Query, Language).

find_resource(Options, Query, Resource):-
  % If a language is given in options, then interpret the given query in this
  % language. If this does not give any results, or if the language option is
  % left out, then pose the query regardless of its language.
  (
    option(language(LanguageCode), Options),
    setoff(
      Resource,
      rdfs_label(Resource, LanguageCode, Query),
      Resources
    ),
    Resource \== [],
    !
  ;
    setoff(
      Resource,
      rdfs_label(Resource, Query),
      Resources
    )
  ),
  (
    Resources = [Resource]
  ->
    debug(lexvo, 'Found resource ~w for query ~w.', [Resource, Query])
  ;
    Resources == []
  ->
    debug(lexvo, 'Could not find a resource for query ~w.', [Query])
  ;
    length(Resources, NumberOfResources),
    debug(lexvo, 'Found ~w resources that match query ~w.', [NumberOfResources, Query])
  ).

%% load is det.
% Loads the Lexvo RDF/XML dataset.
%
% @author Lexvo
% @version 2012/03/04

load:-
  rdf_graph(lexvo),
  !,
  debug(lexvo, 'The lexvo dataset is already loaded.', []).
load:-
  absolute_file_name(
    data_standards(lexvo),
    File,
    [access(read), file_type(rdf)]
  ),
  rdf_load2(File, xml, lexvo).

