:- module(
  dict,
  [
    random_word/3, % +Language:atom
                   % -Word:atom
                   % -Something:atom
    word/3 % ?Language:atom
           % ?Word:atom
           % ?Something:atom
  ]
).

/** <module> Dictionary

Support for natural language dictionaries.

@author Wouter Beek
@version 2015/09
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





%! random_word(+Language:atom, -Word:atom, -Something:atom) is det.

random_word(Lang, Word, Something):-
  aggregate_all(
    count,
    word(Lang, _, Word, _),
    NumberOfWords
  ),
  random_between(1, NumberOfWords, I),
  word(Lang, I, Word, Something).


%! word(?Language:atom, ?Word:atom, ?Something:atom) is nondet.

word(Lang, Word, Something):-
  word(Lang, _, Word, Something).





% INITIALIZATION %

%! dict_assert_stream(+Language:atom, +Read:blob) is det.

dict_assert_stream(Lang, Read):-
  setup_call_cleanup(
    archive_open(Read, Archive, []),
    dict_assert_archive(Lang, Archive),
    archive_close(Archive)
  ).

dict_assert_archive(Lang, Archive):-
  flag(dict(Lang), _, 1),

  % Construct the entry path that is to be extracted from the archive.
  file_name_extension(Lang, dic, Local),
  atomic_list_concat([dictionaries,Local], /, Entry),
  archive_named_entry(Entry, Archive, Read),

  % @tbd Is this the number of lines?
  read_line_to_codes(Read, Codes),
  number_codes(Number, Codes),
  writeln(Number),

  % Assert the dictionary words.
  dict_assert_words(Lang, Read).

%! dict_assert_words(+Language:atom, +Read:blob) is det.

dict_assert_words(_, Read):-
  at_end_of_stream(Read), !.
dict_assert_words(Lang, Read):-
  read_line_to_codes(Read, Cs),

  % Parse and assert a single entry in the dictionary.
  phrase(word_entry(Word,Something), Cs),
  flag(dictionary(Lang), I, I + 1),
  assert_word(Lang, I, Word, Something),

  dict_assert_words(Lang, Read).


%! dict_download(+Language:atom) is det.

dict_download(Lang):-
  dict_url(Lang, UriComponents),
  uri_components(Url, UriComponents),
  setup_call_cleanup(
    http_open(Url, Read, [cert_verify_hook(cert_verify)]),
    dict_assert_stream(Lang, Read),
    close(Read)
  ).


%! dict_file(+Language:atom, -File:atom) is det.

dict_file(Lang, File):-
  absolute_file_name(Lang, File, [access(write),extensions([dic])]).


%! dict_init is det.

dict_init:-
  forall(dict_url(Lang, _), dict_init(Lang)).

dict_init(Lang):-
  dict_file(Lang, File),
  (exists_file(File) -> true ; touch(File)),
  db_attach(File, []),
  file_age(File, Age),
  dict_update(Lang, Age).


%! dict_update(+Language:atom, +Age:float) is det.

% The persistent store is still fresh.
dict_update(Lang, Age):-
  once(word(Lang, _, _, _)),
  Age < 8640000, !.
% The persistent store has become stale, so refresh it.
dict_update(Lang, _):-
  retractall_word(Lang, _, _, _),
  dict_download(Lang).


%! dict_url(?Language:atom, ?UriComponents:compound) is nondet.

dict_url(
  'en-US',
  uri_components(
    https,
    'addons.mozilla.org',
    '/firefox/downloads/file/199368/united_states_english_spellchecker-7.0-fx+sm+tb.xpi',
    'src=dp-btn-primary',
    _
  )
).


%! word_entry(-Word:atom, -Something:atom)// is det.

word_entry(Word2, Something2) -->
  word_part(Word1), !,
  {atom_codes(Word2, Word1)},
  dcg_rest(Something1),
  {atom_codes(Something2, Something1)}.
word_entry(Word2, '') -->
  dcg_rest(Word1),
  {atom_codes(Word2, Word1)}.

word_part([]) --> `/`, !.
word_part([H|T]) -->
  [H],
  word_part(T).
