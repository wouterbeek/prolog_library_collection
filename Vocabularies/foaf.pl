:- module(
  foaf,
  [
    foaf_class/1, % ?Class:uri
    foaf_namespace/2, % ?Prefix:atom
                      % ?URI:uri
    foaf_property/2 % ?Property:uri
                    % ?Datatype:oneof([literal,uri])
  ]
).

/** <module> FOAF

FOAF stands for Friend of a Friend

@author Wouter Beek
@version 2013/03
*/

:- use_module(rdf(rdf_namespace)).

:- rdf_register_namespace(foaf).



foaf_class(foaf:'Agent').
foaf_class(foaf:'Organization').
foaf_class(foaf:'Person').

foaf_namespace(foaf, 'http://xmlns.com/foaf/0.1/').

foaf_property(foaf:homepage, uri  ).
foaf_property(foaf:mbox,     email).
foaf_property(foaf:page,     uri  ).

