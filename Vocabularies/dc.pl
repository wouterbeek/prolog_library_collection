module(
  dc,
  [
    dc_cass/1, % ?Class:uri
    dc_namespace/2, % ?Prefix:atom
                    % ?URI:uri
    dc_property/2, % ?Property:uri
                   % ?Datatype:oneof([literal,uri])
    license/2 % ?Name:atom
              % ?URI:uri
  ]
).

/** <module> Dublin Core

Support for the Dublin Core vocabulary.

@author Wouter Beek
@version 2013/03, 2013/05
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(xml(xml_namespace)).

:- rdf_meta(dc_class(r,r)).
:- rdf_meta(dc_property(r,r)).

:- xml_register_namespace(dc, 'http://purl.org/dc/terms/').
:- xml_register_namespace(xsd, 'http://www.w3.org/2001/XMLSchema#').



dc_class(dc:'Organization').
dc_class(dc:'Person').

dc_namespace(dc, 'http://purl.org/dc/terms/').

dc_property(dc:contributor, uri     ).
dc_property(dc:created,     xsd:date).
dc_property(dc:creator,     uri     ).
dc_property(dc:date,        xsd:date).
dc_property(dc:description, literal ).
dc_property(dc:issued,      xsd:date).
dc_property(dc:license,     oneof(Licenses)):-
  findall(
    URI,
    license(Name, URI),
    Licenses
  ).
dc_property(dc:modified, xsd:date).
dc_property(dc:publisher, uri).
dc_property(dc:source, uri).
dc_property(dc:title, literal).

license(cc0,      'http://creativecommons.org/publicdomain/zero/1.0/').
license(odc_by,   'http://www.opendatacommons.org/licenses/by/'      ).
license(odc_odbl, 'http://www.opendatacommons.org/licenses/odbl/'    ).
license(pddl,     'http://www.opendatacommons.org/licenses/pddl/'    ).

