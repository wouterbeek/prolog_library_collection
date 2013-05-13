:- module(
  rdf_serial,
  [
    rdf_convert/3, % +FromFile:atom
                   % +ToFormat:atom
                   % +ToFile:atom
    rdf_guess_data_format/2, % +Stream:stream
                             % ?Format:atom
    rdf_serialization/2, % ?SerializationFormat:atom
                         % ?URI:uri

% RDF LOAD
    rdf_load2/1, % +File:atom
    rdf_load2/2, % +File:atom
                 % +Options:list(nvpair)

% RDF SAVE
    rdf_save2/0,
    rdf_save2/2 % ?File:atom
                % +Options:list(nvpair)
  ]
).

/** <module> RDF serialization

Helper predicates for loading/saving RDF graphs.

@author Wouter Beek
@version 2012/01, 2012/03, 2012/09, 2012/11, 2013/01-2013/05
*/

:- use_module(generics(db_ext)).
:- use_module(generics(file_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_namespace)).
:- use_module(xml(xml)).
:- use_module(xml(xml_namespace)).

:- db_add_novel(user:prolog_file_type(nt, n_triples)).
:- db_add_novel(user:prolog_file_type(rdf, rdf)).
:- db_add_novel(user:prolog_file_type(ttl, turtle)).

:- debug(rdf_serial).



%! rdf_convert(+FromFile:atom, +ToFormat:atom, +ToFile:atom) is det.

rdf_convert(FromFile, ToFormat, ToFile):-
  TempGraph = rdf_convert,
  rdf_unload_graph(TempGraph),
  rdf_load2(FromFile, [graph(TempGraph)]),
  rdf_save2(ToFile, [format(ToFormat), graph(TempGraph)]),
  rdf_unload_graph(TempGraph).

%! rdf_guess_data_format(+Stream, ?Format:oneof([turtle,xml])) is det.
% Guess the format of an RDF file from the actual content.
% Currently, this seeks for a valid XML document upto the rdf:RDF
% element before concluding that the file is RDF/XML. Otherwise it
% assumes that the document is Turtle.
%
% @author Jan Wielemaker
% @version 2011

rdf_guess_data_format(_, Format):-
  nonvar(Format),
  !.
rdf_guess_data_format(Stream, xml):-
  xml_doctype(Stream, _),
  !.
rdf_guess_data_format(_, turtle).

rdf_guess_serialization_format(Stream, SerializationFormat):-
  rdf_guess_data_format(Stream, DataFormat),
  rdf_serialization_format(SerializationFormat, DataFormat).

rdf_load2(File):-
  rdf_load2(File, []).

% A wrapper for semweb's rdf_load/2.
rdf_load2(File, O1):-
  access_file(File, read),
  option(format(Format), O1),
  option(graph(Graph), O1),

  % Combine the given with the standard options.
  merge_options([register_namespaces(true), silent(true)], O1, O2),

  % The real job is performed by a predicate from the semweb library.
  rdf_load(File, O2),

  % Send a debug message notifying that the RDF file was successfully loaded.
  debug(
    rdf_serial,
    'Graph ~w was loaded in ~w serialization from file ~w.',
    [Graph, Format, File]
  ),
  !.
% Load all files from a given directory.
rdf_load2(Directory, O1):-
  exists_directory(Directory),
  !,

  % If the =format= option is specified, then we already have an
  % instantiation for variable =Serialization=.
  (
    option(format(Format), O1)
  ->
    rdf_serialization_format(Serialization, Format)
  ;
    true
  ),

  % Find extensions that match the serialization.
  % Backtracking over serialization variants, file types,
  % and extensions is possible.
  rdf_serialization_file_type(Serialization, FileType),
  prolog_file_type(Extension, FileType),

  % Extract the list of matching files.
  format(atom(RE), '~w/*.~w', [Directory, Extension]),
  expand_file_name(RE, Files),

  forall(
    member(File, Files),
    rdf_load2(File, O1)
  ).
% The graph is missing, extrapolate it from the file.
rdf_load2(File, O1):-
  access_file(File, read),
  \+ option(graph(Graph), O1),
  !,

  merge_options([graph(Graph)], O1, O2),
  rdf_load2(File, O2).
% The format is missing, extrapolate it from the file.
rdf_load2(File, O1):-
  access_file(File, read),
  \+ option(fornat(Format), O1),
  !,

  % The local name (without extension) of the file is used as the graph name.
  file_name(File, _Directory, _Base, Extension),
  prolog_file_type(Extension, FileType),
  rdf_serialization_file_type(Serialization, FileType),
  rdf_serialization_format(Serialization, Format),
  merge_options([format(Format)], O1, O2),

  rdf_load2(File, O2).

% Save all!
rdf_save2:-
  forall(
    rdf_graph(Graph),
    rdf_save2(_File, [graph(Graph)])
  ).

rdf_save2(File, O1):-
  access_file(File, write),
  option(format(Format), O1),
  once(rdf_serialization_format(_Serialization, Format)),
  option(graph(Graph), O1),
  rdf_graph(Graph),
  !,

  open(File, write, Stream),

  (
    Format == 'turtle'
  ->
    merge_options(
      [
        align_prefixes(true),
        indent(2),
        only_known_prefixes(true),
        tab_distance(0)
      ],
      O1,
      O2
    ),
    rdf_save_turtle(stream(Stream), O2)
%  ;
%    Format == 'ntriples'
%  ->
%    merge_options(
%      [
%        align_prefixes(true),
%        indent(2),
%        only_known_prefixes(true),
%        tab_distance(0)
%      ],
%      O1,
%      O2
%    ),
%    rdf_save_turtle(stream(Stream), O2)
  ;
    rdf_save(stream(Stream), O1)
  ),

  debug(
    rdf_serial,
    'Graph ~w was saved in ~w serialization to file ~w.',
    [Graph, Format, File]
  ),
  close(Stream),
  !.
% Throw an exception if no graph with the given name exists.
rdf_save2(_File, O1):-
  \+ ((
    option(graph(Graph), O1),
    rdf_graph(Graph)
  )),
  !,
  existence_error(atom, Graph).
% If the graph was loaded from a file, then save it to that same file again.
rdf_save2(File, O1):-
  var(File),
  option(graph(Graph), O1),
  rdf_graph_source_file(Graph, File),
  !,
  % Recurse once, to extract the serialization format.
  rdf_save2(File, O1).
% Make up the format.
rdf_save2(File, O1):-
  access_file(File, write),
  option(graph(Graph), O1),
  rdf_graph(Graph),
  !,
  file_name(File, _Directory, _Graph, Extension),
  prolog_file_type(Extension, FileType),
  rdf_serialization_file_type(Serialization, FileType),
  rdf_serialization_format(Serialization, Format),
  merge_options([format(Format)], O1, O2),
  rdf_save(File, O2).

rdf_serialization('N-Triples', 'http://www.w3.org/ns/formats/N-Triples').
rdf_serialization('N3',        'http://www.w3.org/ns/formats/N3'       ).
rdf_serialization('RDF/XML',   'http://www.w3.org/ns/formats/RDF_XML'  ).
rdf_serialization('RDFa',      'http://www.w3.org/ns/formats/RDFa'     ).
rdf_serialization('Turtle',    'http://www.w3.org/ns/formats/Turtle'   ).

rdf_serialization_file_type('N-Triples', n_triples).
rdf_serialization_file_type('N3'       , n3       ).
rdf_serialization_file_type('RDF/XML'  , owl      ).
rdf_serialization_file_type('RDF/XML'  , rdf      ).
rdf_serialization_file_type('RDFa'     , rdfa     ).
rdf_serialization_file_type('Turtle'   , turtle   ).

rdf_serialization_format('N-Triples', ntriples).
rdf_serialization_format('RDF/XML',   xml     ).
rdf_serialization_format('Turtle',    turtle  ).

