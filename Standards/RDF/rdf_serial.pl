:- module(
  rdf_serial,
  [
    convert_turtle_to_rdf/2, % +TurtleFile:file
                             % +RDFFile:file
    rdf_guess_data_format/2, % +Stream:stream
                             % ?Format:oneof([turtle,xml])
    rdf_load2/1, % +Graph:atom
    rdf_load2/2, % +File:atom
                 % ?Graph:atom
    rdf_load2/3, % ?File:atom
                 % ?SerializationFormat:oneof(['N-Triples','N3','RDF/XML','RDFa','Turtle'])
                 % ?Graph:atom
    rdf_save2/1, % +Graph:atom
    rdf_save2/2, % +Graph:atom
                 % ?File:atom
    rdf_save2/3, % ?Graph:atom
                 % ?SerializationFormat:oneof(['N-Triples','N3','RDF/XML','RDFa','Turtle'])
                 % ?File:atom
    rdf_serialization/4 % ?FileType:oneof([n_triples,n3,rdf,rdfa,turtle]).
                        % ?SerializationFormat:oneof(['N-Triples','N3','RDF/XML','RDFa','Turtle'])
                        % ?Supported:boolean
                        % ?URI:uri
  ]
).

/** <module> RDF serialization

Predicates forturinging serializations into RDF graphs and vice versa.

Supported serialization formats:
* RDF/XML
* TURTLE

@author Wouter Beek
@version 2012/01, 2012/03, 2012/09, 2012/11, 2013/01-2013/04
*/

:- use_module(generics(file_ext)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(rdf(rdf_export)).
:- use_module(rdf(rdf_namespace)).
:- use_module(standards(xml)).

:- assert(user:prolog_file_type(rdf, rdf)).
:- assert(user:prolog_file_type(ttl, turtle)).

:- debug(rdf_serial).



%% convert_turtle_to_rdf(+Turtle, +RDF) is det.
% Convert the given Turtle contents to the RDF format and stores then in the
% given RDF file using the RDF/XML serialization.
%
% @param Turtle A Turtle file with read access.
% @param RDF An RDF file with write access.

convert_turtle_to_rdf(Turtle, RDF):-
  rdf_read_turtle(Turtle, Triples, []),
  forall(
    member(rdf(Subject, Predicate, Object), Triples),
    rdf_assert(Subject, Predicate, Object, conversion)
  ),
  rdf_save2(RDF, xml, [graph(conversion), encoding(utf8)]),
  rdf_retractall(_Subject, _Predicate, _Object, conversion).

%% rdf_guess_data_format(+Stream, ?Format:oneof([turtle,xml]) is det.
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

%% rdf_load2(+Graph:atom) is semidet.
% Loads the graph with the given name.
% This graph is assumed to be located in the data subdirectory of the
% user's personal project directory.
%
% It is possible to load graphs in subdirectories of the data directory.
% For instance: =|rdf_load2('STCN/STCN_Publications').|=.
%
% @param Graph The atomic name of a graph.

rdf_load2(Graph):-
  rdf_load2(_File, Graph).

%% rdf_load2(+File:atom, ?Graph:atom) is semidet.

rdf_load2(File, Graph):-
  rdf_load2(File, _SerializationFormat, Graph).

%% rdf_load2(
%%   ?File:atom,
%%   ?SerializationFormat:oneof([turtle,unsupported,xml]),
%%   ?Graph:atom
%% ) is semidet.
% Possible instantiations:
% +,+,+
% +,?,+
% +,?,-
% ?,?,+

% All arguments must be set for this predicate to succeeds.
% The other clauses of this predicate are meant to instantiate these
% three variables.
rdf_load2(File, SerializationFormat, Graph):-
  % Check whether the file is given, exists, and can be accessed.
  nonvar(File),
  exists_file(File),
  access_file(File, read),

  % Check whether the format is given, exists, and is supported.
  nonvar(SerializationFormat),
  % Make sure the serialization format is supported, and take the display
  % name for use in the debug message.
  once(rdf_serialization(_FileType, SerializationFormat, true, _URI)),

  % Check whether the graph name is given.
  nonvar(Graph),
  !,

  % Since we use more descriptive names for the serialization formats,
  % we translate them to the format names that are using in the semweb
  % library.
  translate_serialization_format(SerializationFormat, Format),

  % The real job is performed by a predicate from the semweb library.
  rdf_load(
    File,
    [
      format(Format),
      graph(Graph)
      %register_namespaces(true),
      %silent(fail) % We like to get feedback.
    ]
  ),

  % Send a debug message notifying that the RDF file was successfully loaded.
  debug(
    rdf_serial,
    'Graph ~w was loaded in ~w serialization from file ~w.',
    [Graph, SerializationFormat, File]
  ).
% File and graph are given and are not necessarily the same.
% The serialization format is derived from the file extension.
rdf_load2(File, SerializationFormat, Graph):-
  nonvar(File),
  nonvar(Graph),
  !,
  file_name_type(_Name, FileType, File),
  rdf_serialization(FileType, SerializationFormat, true, _URI),
  % We prefer serialization formats that occur earlier in this file.
  rdf_load2(File, SerializationFormat, Graph),
  % Since there can be multiple serialization formats, we add a cut here.
  !.
% The file is given but the graph is not.
% The graph is derived from the file.
rdf_load2(File, SerializationFormat, Graph):-
  nonvar(File),
  var(Graph),
  !,
  % The local name (without extension) of the file is used as the graph name.
  file_name(File, _Directory, Graph, Extension),
  user:prolog_file_type(Extension, FileType),
  rdf_serialization(FileType, SerializationFormat, true, _URI),
  rdf_load2(File, SerializationFormat, Graph),
  !.
% The graph is given and only the file name and/or the file type have to be
% determined.
rdf_load2(File, SerializationFormat, Graph):-
  nonvar(Graph),
  !,
  % Try various file types.
  rdf_serialization(FileType, SerializationFormat, true, _URI),
  catch(
    absolute_file_name(
      data(Graph),
      File,
      [access(read), file_type(FileType)]
    ),
    _ExistenceError,
    fail
  ),
  rdf_load2(File, SerializationFormat, Graph),
  % Only load one file, i.e. use one serialization.
  !.

rdf_save2(Graph):-
  forall(
    rdf_serialization(FileType, SerializationFormat, true, _URI),
    (
      absolute_file_name(
        data(Graph),
        File,
       [access(write), file_type(FileType)]
      ),
      rdf_save2(Graph, SerializationFormat, File)
    )
  ).

%% rdf_save2(+Graph:atom, ?File:atom) is det.
% Save using all serializations.

rdf_save2(Graph, File):-
  rdf_save2(Graph, _SerializationFormat, File).

%% rdf_save2(
%%   +Graph:atom,
%%   ?SerializationFormat:oneof(['N-Triples','N3','RDF.XML','RDFa','Turtle']),
%%   ?File:atom
%% ) is det.

% Throw an exception if no graph with the given name exists.
rdf_save2(Graph, _SerializationFormat, _File):-
  nonvar(Graph),
  \+ rdf_graph(Graph),
  !,
  existence_error(atom, Graph).
% In case the file name is not given it is reconstructred from the graph name
% and serialization format.
rdf_save2(Graph, SerializationFormat, File):-
  nonvar(Graph),
  nonvar(SerializationFormat),
  var(File),
  !,
  rdf_serialization(FileType, SerializationFormat, true, _URI),
  absolute_file_name(data(Graph), File, [access(write), file_type(FileType)]),
  rdf_save2(Graph, SerializationFormat, File),
  !.
% Save to Turtle serialization.
rdf_save2(Graph, 'Turtle', File):-
  nonvar(Graph),
  nonvar(File),
  !,
  open(File, write, Stream, [close_on_abort(true), type(text)]),
  rdf_save_turtle(
    stream(Stream),
    [
      align_prefixes(true),
      graph(Graph),
      indent(2),
      only_known_prefixes(true),
      tab_distance(0)
    ]
  ),
  debug(
    rdf_serial,
    'Graph ~w was saved in Turtle serialization to file ~w.',
    [Graph, File]
  ),
  close(Stream).
% Save to RDF/XML serialization.
rdf_save2(Graph, 'RDF/XML', File):-
  nonvar(Graph),
  nonvar(File),
  !,
  open(File, write, Stream, [close_on_abort(true), type(text)]),
  rdf_current_namespaces(Graph, Namespaces),
  rdf_save(stream(Stream), [graph(Graph), namespaces(Namespaces)]),
  debug(
    rdf_serial,
    'Graph ~w was saved in XML/RDF serialization to file ~w.',
    [Graph, File]
  ),
  close(Stream).

%% rdf_serialization(
%%   ?FileType:oneof([n_triples,n3,rdf,rdfa,turtle]),
%%   ?SerializationFormat:oneof(['N-Triples','N3','RDF.XML','RDFa','Turtle']),
%%   ?Supported:boolean,
%%   ?URI:uri
%% ) is nondet.

rdf_serialization(n_triples, 'N-Triples', false, 'http://www.w3.org/ns/formats/N-Triples').
rdf_serialization(n3,        'N3',        false, 'http://www.w3.org/ns/formats/N3'       ).
rdf_serialization(rdf,       'RDF/XML',   true,  'http://www.w3.org/ns/formats/RDF_XML'  ).
rdf_serialization(rdfa,      'RDFa',      false, 'http://www.w3.org/ns/formats/RDFa'     ).
rdf_serialization(turtle,    'Turtle',    true,  'http://www.w3.org/ns/formats/Turtle'   ).

translate_serialization_format('RDF/XML', xml   ).
translate_serialization_format('Turtle',  turtle).

