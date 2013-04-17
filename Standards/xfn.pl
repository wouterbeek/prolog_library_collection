:- module(xfn, []).

/** <module> XFN

@author Wouter Beek
@version 2012/07
*/

:- use_module(pgc(file_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_read)).



go:-
  absolute_file_name(debug(tests), AbsoluteFileName, [file_type(hypertext)]),
  parse_html(AbsoluteFileName, DOM),
  format(user_output, '~w', [DOM]),
  rdf_assert('http://www.wouterbeek.com', rdf:type, xfn:person, xfn),
  rdf_assert_literal('http://www.wouterbeek.com', xfn:name, 'Wouter Beek', xfn),
  find_contacts('http://www.wouterbeek.com', DOM),
  export_contacts.

export_contacts:-
  flag(contacts_id, ID, ID + 1),
  format(atom(Name), 'contacts_~w', [ID]),
  create_file(debug(ce), Name, graphviz, File),
  open(
    File,
    write,
    _Stream,
    [alias(xfn), close_on_abort(true), type(text)]
  ),

  format(xfn, 'digraph circuit {\n', []),
  forall(
    rdfs_individual_of(Person, xfn:person),
    (
      rdf_literal(Person, xfn:name, Name, xfn),
      format(xfn, '  ~w\n', [Name])
    )
  ),
  forall(
    rdfs_subproperty_of(Relationship, xfn:relation),
    (
      (
        rdf(Person1, Relationship, Person2, xfn)
      ->
        rdf_literal(Person1, xfn:name, Name1, xfn),
        rdf_global_id(xfn:RelationshipName, Relationship),
        rdf_literal(Person2, xfn:name, Name2, xfn),
        format(
          xfn,
          '  ~w -> ~w [label="~w"]\n',
          [Name1, Name2, RelationshipName]
        )
      ;
        true
      )
    )
  ),

  format(xfn, '}\n', []),
  close(xfn).

find_contacts(_Person1, []):-
  !.
find_contacts(Person1, List):-
  is_list(List),
  !,
  maplist(find_contacts(Person1), List).
find_contacts(Person1, element(a, LinkAttributes, [Name])):-
  !,
  find_link(LinkAttributes, Person2),
  find_relationship(LinkAttributes, RelationshipsAtom),
  atomic_list_concat(RelationshipAtoms, ' ', RelationshipsAtom),
  rdf_assert_literal(Person2, xfn:name, Name, xfn),
  forall(
    member(RelationshipAtom, RelationshipAtoms),
    (
      rdf_global_id(xfn:RelationshipAtom, Relationship),
      (
        rdfs_subproperty_of(Relationship, xfn:relation),
        !
      ;
        rdf_assert(Relationship, rdfs:subPropertyOf, xfn:relation, xfn)
      ),
      rdf_assert(Person1, Relationship, Person2, xfn),
      rdf_assert(Person2, rdf:type, xfn:person, xfn)
    )
  ).
find_contacts(Person1, element(_Tag, _Attributes, ListOfContents)):-
  !,
  find_contacts(Person1, ListOfContents).
find_contacts(_Person1, _Atom).

find_link(L, Link):-
  member(href=Link, L),
  !.
find_link(_L, nil).

find_relationship(L, Relationship):-
  member(rel=Relationship, L),
  !.
find_relationship(_L, nil).

parse_html(File, DOM):-
  setup_call_cleanup(
    open(File, read, Stream, [type(text)]),
    (
      dtd(html, DTD),
      load_structure(
        stream(Stream),
        DOM,
        [
          dialect(sgml),
          dtd(DTD),
          max_errors(-1),
          shorttag(false),
          syntax_errors(quiet)
        ]
      )
    ),
    close(Stream, [force(true)])
  ).
