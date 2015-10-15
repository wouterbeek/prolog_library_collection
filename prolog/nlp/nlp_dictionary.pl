:- module(
  nlp_dictionary,
  [
    nlp_random_word/3, % +Language:string
                       % -Word:string
                       % -Something:string
    nlp_word/3 % ?Language:string
               % ?Word:string
               % ?Something:string
  ]
).

/** <module> Natural Language: Dictionary

Support for natural language dictionaries.

@author Wouter Beek
@version 2015/09-2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(persistency)).
:- use_module(library(random)).
:- use_module(library(readutil)).
:- use_module(library(uri)).

:- persistent(word(language:atom,index:nonneg,word:atom,something:atom)).

cert_verify(_, _, _, _, _):- !.

:- initialization(dict_init).





%! nlp_random_word(+Language:string, -Word:string, -Something:string) is det.

nlp_random_word(Lang, Word, Something):-
  aggregate_all(count, word(Lang, _, Word, _), NumberOfWords),
  random_between(1, NumberOfWords, I),
  word(Lang, I, Word, Something).



%! nlp_word(?Language:string, ?Word:string, ?Something:string) is nondet.

nlp_word(Lang, Word, Something):-
  word(Lang, _, Word, Something).





% INITIALIZATION %

%! dict_assert_stream(+Language:string, +Read:stream) is det.

dict_assert_stream(Lang, Read):-
  setup_call_cleanup(
    archive_open(Read, Archive, []),
    dict_assert_archive(Lang, Archive),
    archive_close(Archive)
  ).



%! dict_assert_archive(+Language:string, +Archive:archive) is det.

dict_assert_archive(Lang, Archive):-
  flag(dict(Lang), _, 1),

  % Construct the entry path that is to be extracted from the archive.
  file_name_extension(Lang, dic, Local),
  atomic_list_concat([dictionaries,Local], /, Entry),
  archive_named_entry(Archive, Entry, Read),

  % @tbd Is this the number of lines?
  read_line_to_codes(Read, Cs),
  number_codes(N, Cs),
  writeln(N),

  % Assert the dictionary words.
  dict_assert_words(Lang, Read).



%! dict_assert_words(+Language:string, +Read:stream) is det.

dict_assert_words(_, Read):-
  at_end_of_stream(Read), !.
dict_assert_words(Lang, Read):-
  read_line_to_codes(Read, Cs),

  % Parse and assert a single entry in the dictionary.
  phrase(word_entry(Word, Something), Cs),
  flag(dictionary(Lang), I, I + 1),
  assert_word(Lang, I, Word, Something),

  dict_assert_words(Lang, Read).



%! dict_download(+Language:string) is det.

dict_download(Lang):-
  dict_iri(Lang, Iri),
  setup_call_cleanup(
    http_open(Iri, Read, [cert_verify_hook(cert_verify)]),
    dict_assert_stream(Lang, Read),
    close(Read)
  ).



%! dict_file(+Language:string, -File:atom) is det.

dict_file(Lang, File):-
  absolute_file_name(Lang, File, [access(write),extensions([dic])]).



%! dict_init is det.

dict_init:-
  forall(dict_iri(Lang, _), dict_init(Lang)).

dict_init(Lang):-
  dict_file(Lang, File),
  (exists_file(File) -> true ; touch(File)),
  db_attach(File, []),
  file_age(File, Age),
  dict_update(Lang, Age).



%! dict_update(+Language:string, +Age:float) is det.

% The persistent store is still fresh.
dict_update(Lang, Age):-
  once(word(Lang, _, _, _)),
  Age < 8640000, !.
% The persistent store has become stale, so refresh it.
dict_update(Lang, _):-
  retractall_word(Lang, _, _, _),
  dict_download(Lang).



%! dict_iri(?Language:string, ?Iri:atom) is nondet.

dict_iri("en-US", Iri):-
  uri_components(
    Iri,
    uri_components(
      https,
      'addons.mozilla.org',
      '/firefox/downloads/file/199368/united_states_english_spellchecker-7.0-fx+sm+tb.xpi',
      'src=dp-btn-primary',
      _
    )
  ).



%! word_entry(-Word:string, -Something:string)// is det.

word_entry(Word, Something) -->
  word_part(Word0), !,
  {string_codes(Word, Word0)},
  dcg_rest(Something0),
  {string_codes(Something, Something0)}.
word_entry(Word, "") -->
  dcg_rest(Word0),
  {string_codes(Word, Word0)}.

word_part([]) -->
  "/", !.
word_part([H|T]) -->
  [H],
  word_part(T).
