:- module(
  dbpedia_spotlight,
  [
    annotate/2, % +Txt, -Anns
    annotate/3  % +Txt, -Anns, +Opts
  ]
).

/** <module> DBpedia Spotlight

Prolog interface to the DBpedia Spotlight Web Service.

### Example request

```bash
$ curl -H "Accept: application/json" \
       -H "Content-Type: application/x-www-form-urlencoded" \
       --data "text=President Obama called Wednesday." \
       http://spotlight.dbpedia.org/rest/annotate
```

### Example request & response

```bash
$ curl --data "text=De apen zijn terug&confidence=0" -H "Accept: application/json" "http://spotlight.sztaki.hu:2232/rest/annotate"
{
  "@text": "De apen zijn terug",
  "@confidence": "0.0",
  "@support": "0",
  "@types": "",
  "@sparql": "",
  "@policy": "whitelist",
  "Resources":   [
        {
      "@URI": "http://nl.dbpedia.org/resource/Lidwoord",
      "@support": "176",
      "@types": "",
      "@surfaceForm": "De",
      "@offset": "0",
      "@similarityScore": "0.9888031144055698",
      "@percentageOfSecondRank": "0.0"
    },
        {
      "@URI": "http://nl.dbpedia.org/resource/Apen",
      "@support": "455",
      "@types": "DBpedia:Species,DBpedia:Eukaryote,DBpedia:Animal,DBpedia:Mammal",
      "@surfaceForm": "apen",
      "@offset": "3",
      "@similarityScore": "0.9981839427365612",
      "@percentageOfSecondRank": "0.0018168792962059285"
    }
  ]
}
```

@author Wouter Beek
@compat [DBpedia Spotlight Web Service](http://dbpedia-spotlight.github.io/demo/)
@version 2015/08, 2016/05, 2016/07
*/

:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_io)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(print_ext)).
:- use_module(library(uri)).

:- use_module(detect_language).

:- dynamic
    cached_annotate/3.





%! annotate(+Txt, -Annotations:dict) is det.
%! annotate(+Txt, -Annotations:dict, +Opts) is det.
%
% Annotations are returned in a Prolog dictionary that is similar to
% the JSON output of the DBpedia Spotlight Web Service.
%
% The following options are supported:
%
%   * `concepts(-ordset(iri))` Returns the annotation concepts.
%
%   * `confidence(+float)` Default is 0.5.
%
%   * `disambiguator(+oneof(['Default','Document']))` Default is
%   `'Default'`.
%
%   * `language(?atom)` The natural language in which Text occurs as
%   an expression and relative to which annotations are made.  If not
%   given, the natural language is assessed algorithmically.
%
%   * `output(+oneof([element,list])) Whether only the top annotation
%   should be returned (`element`, default) or a list of alternative
%   annotations (`list`).
%
%   * `policy(+oneof([whitelist]))` Default is `'whitelist'`.
%
%   * `spotter(+oneof(['Default']))` Default is `'Default'`.
%
%   * `support(+integer)` Set the minimum? *prominence* of the
%   retrieved entities.  This is measured as the number of in-links in
%   Wikipedia.  Default is 0.
%
% @tbd Add support for `types` query string option.
% @tbd Add support for `sparql` query string option.
% @tbd Check whether DBpedia Spotlight now supports the value 0.0 for the
% confidence option.  (Only 0 used to work.)
% @throws failed

annotate(Txt, Anns) :-
  annotate(Txt, Anns, []).


% Cached results.
annotate(Txt, Anns, Opts) :-
  cached_annotate(Txt, Anns, Opts), !.
annotate(Txt, Anns, Opts) :-
  % Scheme.
  Scheme = http,

  % The language can be explicitly given as an option,
  % but it can also be a request for the automatically assessed one.
  (   option(language(Lang), Opts)
  ->  true
  ;   % The language is assessed algorithmically.
      detect_language(Txt, Lang)
  ),

  % IRI host.
  % Different hosts support different languages.
  % NONDET: Multiple hosts.
  remote_host(Lang, SiteId, Host),

  % IRI port.
  % Some authorities use different ports for different languages.
  (   SiteId == sztaki
  ->  language_port(Lang, Port),
      uri_authority_components(Auth, uri_authority(_,_,Host,Port))
  ;   Auth = Host
  ),

  % The IRI path depends on the chosen output: elements or lists.
  (   option(output(list), Opts)
  ->  Path = '/rest/candidates'
  ;   Path = '/rest/annotate'
  ),

  % Query string.
  option(confidence(Conf), Opts, 0.5),
  option(disambiguator(Disambiguator), Opts, 'Default'),
  option(policy(Policy), Opts, whitelist),
  option(spotter(Spotter), Opts, 'Default'),
  option(support(Support), Opts, 0),

  uri_components(Iri, uri_components(Scheme,Auth,Path,_,_)),

  % Message body.
  % Notice that the query components are also sent in the POST body.
  uri_query_components(
    Body0,
    [
      confidence=Conf,
      disambiguator=Disambiguator,
      policy=Policy,
      %sparql=Sparql
      spotter=Spotter,
      support=Support,
      text=Txt
      %types=Types
    ]
  ),
  atom_codes(Body0, Body),

  % Perform Web request.
  http_post(
    Iri,
    codes('application/x-www-form-urlencoded',Body),
    callback0(Txt, Anns, Opts),
    [request_header('Accept'='application/json')]
  ), !.
annotate(_, _, _) :-
  print_message(error, service_offline(dbpedia_spotlight)).


callback0(Txt, Anns, Opts, In, Meta, Meta) :-
  json_read_dict(In, Anns, [tag(annotations)]),
  (debugging(dbpedia_spotlight) -> print_dict(Anns) ; true), %DEB

  % Allow annotation concepts to be returned as an option.
  (   option(concepts(Concepts), Opts)
  ->  (   Anns0 = Anns.get('Resources')
      ->  true
      ;   Anns0 = []
      ),
      maplist(annotation_to_concept0, Anns0, Concepts0),
      list_to_ord_set(Concepts0, Concepts)
  ;   true
  ),

  % Memoization.
  assert(cached_annotate(Txt, Anns, Opts)).


annotation_to_concept0(Ann, Concept) :-
  atom_string(Concept, Ann.'@URI').





% HELPERS %

%! remote_host(?Language:atom, ?Service:atom, ?Host:atom) is nondet.

%remote_host(en, localhost, 'localhost:2222').
remote_host(en, dbpedia, 'spotlight.dbpedia.org') :- !.
remote_host(Lang, sztaki, 'spotlight.sztaki.hu') :-
  once(language_port(Lang, _)), !.
remote_host(Lang, _, _) :-
  existence_error(spotlight_server, Lang).



%! language_port(+LTag, -Port) is det.
%
% Maps natural languages onto the port number of the authority that
% annotates expressions from that language.

language_port(de, 2226).
language_port(en, 2222).
language_port(fr, 2225).
language_port(hu, 2229).
language_port(it, 2230).
language_port(nl, 2232).
language_port(po, 2228).
language_port(sp, 2231).
language_port(tu, 2235).
language_port(ru, 2227).





% MESSAGE %

:- multifile(prolog:message//1).

prolog:message(service_offline(Service)) -->
  ['Service ',Service,' is offline.'].
