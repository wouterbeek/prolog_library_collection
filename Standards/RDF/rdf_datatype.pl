:- module(
  rdf_datatype,
  [
    rdf_datatype/2, % ?DatatypeName:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,image,integer])
                    % ?Datatype:uri
    rdf_datatype/4 % ?DatatypeName:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,image,integer])
                   % ?LexicalValue
                   % ?Datatype:uri
                   % ?CanonicalValue
  ]
).

/** <module> RDF datatype

RDF datatypes. The XML Schema datatype plus custom defined datatypes.

@author Wouter Beek
@version 2013/03
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(standards(xmls)).

:- rdf_register_prefix(prasem, 'http://www.wouterbeek.com/prasem.owl#', [keep(true)]).

:- rdf_meta(rdf_datatype(?,r)).
:- rdf_meta(rdf_datatype(?,?,r,?)).

user:file_search_path(www, project(www)).
user:file_search_path(www_img, www(img)).



rdf_datatype(image, prasem:image).

rdf_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue):-
  xmls_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue).
rdf_datatype(image, LexicalValue, prasem:image, CanonicalValue):-
  nonvar(CanonicalValue),
  !,
  absolute_file_name(www_img(CanonicalValue), LexicalValue, [access(read), file_type(png)]).
rdf_datatype(image, LexicalValue, prasem:image, CanonicalValue):-
  nonvar(LexicalValue),
  !,
  file_base_name(LexicalValue, CanonicalValue).

