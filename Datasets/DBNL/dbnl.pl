:- module(
  dbnl,
  [
    dbnl_scrape/0
  ]
).

/** <module> DBNL

Digitale Bibliotheek der Nederlanden

---+ URI that currently fail

  * =|http://www.dbnl.org/titels/titel.php?id=alph002jidn01|=
  * =|http://www.dbnl.org/titels/titel.php?id=bild002meng02|=
  * =|http://www.dbnl.org/titels/titel.php?id=busk001litt01|=
  * =|http://www.dbnl.org/titels/titel.php?id=crem001dokt01|=
  * =|http://www.dbnl.org/titels/titel.php?id=daal002janp01|=

@author Wouter Beek
@version 2013/05
*/

:- use_module(dbnl(dbnl_index)).
:- use_module(generics(db_ext)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xlink)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').

:- db_add_novel(user:file_search_path(dtd, dbnl(.))).



dbnl_assert_schema(Graph):-
  rdfs_assert_subproperty(dbnl:downloads, dbnl:text, Graph),
  rdfs_assert_subproperty(dbnl:downloads, dbnl:toc, Graph),
  rdfs_assert_subproperty(dbnl:downloads, dbnl:volume_collection, Graph).

dbnl_scrape:-
  dbnl_scrape('Alle titels', 'alfabetisch op auteur'),
  dbnl_assert_schema(dbnl).

