load:-
  assert(user:file_search_path(generics,     pgc('Generics'))),
  assert(user:file_search_path(graph_theory, pgc('Graph Theory'))),
  assert(user:file_search_path(math,         pgc('Math'))),
  assert(user:file_search_path(server,       pgc('Server'))),
  assert(user:file_search_path(standards,    pgc('Standards'))),
    assert(user:file_search_path(html,         pgc('HTML'))),
    assert(user:file_search_path(iso,          pgc('ISO'))),
    assert(user:file_search_path(owl,          pgc('OWL'))),
    assert(user:file_search_path(rdf,          pgc('RDF'))),
    assert(user:file_search_path(rdfs,         pgc('RDFS'))),
    assert(user:file_search_path(sparql,       pgc('SPARQL'))),
    assert(user:file_search_path(svg,          pgc('SVG'))),
  assert(user:file_search_path(vocabularies, pgc('Vocabularies'))),
    assert(user:file_search_path(skos,         pgc('SKOS'))).

:- load.
