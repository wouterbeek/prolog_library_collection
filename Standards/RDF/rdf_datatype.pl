:- module(
  rdf_datatype,
  [
    rdf_convert_datatype/4, % +FromDatatype:uri
                            % +FromValue
                            % +ToDatatype:uri
                            % -ToValue
    rdf_datatype/2, % ?DatatypeName:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,image,int])
                    % ?Datatype:uri
    rdf_datatype/4 % ?DatatypeName:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,image,int])
                   % ?LexicalValue
                   % ?Datatype:uri
                   % ?CanonicalValue
  ]
).

/** <module> RDF datatype

RDF datatypes. The XML Schema datatype plus custom defined datatypes.

This module uses the =|img|= search file name for finding images.

@author Wouter Beek
@version 2013/03-2013/04
*/

:- use_module(generics(type_checking)).
:- use_module(library(semweb/rdf_db)).
:- use_module(standards(xmls)).

:- rdf_register_prefix(prasem, 'http://www.wouterbeek.com/prasem.owl#', [keep(true)]).

:- assert(user:file_search_path(jpg, jpg)).
:- assert(user:file_search_path(png, png)).

:- rdf_meta(rdf_convert_datatype(r,+,r,-)).
:- rdf_meta(rdf_datatype(?,r)).
:- rdf_meta(rdf_datatype(?,?,r,?)).



image_file_type(jpg).
image_file_type(png).

rdf_convert_datatype(FromDatatype, FromValue, ToDatatype, ToValue):-
  rdf_datatype(FromDatatype, FromValue, Canonical),
  rdf_datatype(ToDatatype, ToValue, Canonical).

rdf_datatype(image, prasem:image).

%% rdf_datatype(Datatype, LexicalValue, CanonicalValue)
% @param Datatype Either the atomic name of a datatype or a URI
%        representing a datatype.

rdf_datatype(Datatype, LexicalValue, CanonicalValue):-
  is_uri(Datatype),
  !,
  rdf_datatype(_DatatypeName, LexicalValue, Datatype, CanonicalValue).
rdf_datatype(DatatypeName, LexicalValue, CanonicalValue):-
  rdf_datatype(DatatypeName, LexicalValue, _Datatype, CanonicalValue).

rdf_datatype(image, LexicalValue, prasem:image, CanonicalValue):-
  nonvar(CanonicalValue),
  !,
  image_file_type(FileType),
  absolute_file_name(
    img(CanonicalValue),
    LexicalValue,
    [access(read), file_type(FileType)]
  ),
  % Do not backtrack on iamge file types.
  !.
rdf_datatype(image, LexicalValue, prasem:image, CanonicalValue):-
  nonvar(LexicalValue),
  !,
  file_base_name(LexicalValue, CanonicalValue).
rdf_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue):-
  xmls_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue).

