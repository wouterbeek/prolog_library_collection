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

:- persistent(word(language:string,index:nonneg,word:string,something:string)).

cert_verify(_, _, _, _, _):- !.

:- initialization(dict_init).





%! nlp_random_word(+Language:string, -Word:string, -Something:string) is det.

nlp_random_word(Lang, Word, Something):-
  aggregate_all(count, word(Lang, _, Word, _), NumberOfWords),
  random_between(1, NumberOfWords, M),
  word(Lang, M, Word, Something).



%! nlp_word(?Language:string, ?Word:string, ?Something:string) is nondet.

nlp_word(Lang, Word, Something):-
  word(Lang, _, Word, Something).





% INITIALIZATION %

%! dict_assert(+Language:string, +Read:stream) is det.

dict_assert(Lang, Read):-
  dict_assert(Lang, 0, Read).

dict_assert(_, _, Read):-
  at_end_of_stream(Read), !.
dict_assert(Lang, M, Read):-
  read_line_to_codes(Read, Cs),

  % Parse and assert a single entry in the dictionary.
  phrase(word_entry(Word, Something), Cs),
  succ(M, N),
  assert_word(Lang, N, Word, Something),

  dict_assert(Lang, N, Read).



%! dict_download(+Language:string) is det.

dict_download(Lang):-
  dict_iri(Lang, Iri),

  % Construct the entry path that is to be extracted from the archive.
  file_name_extension(Lang, dic, Local),
  atomic_list_concat([dictionaries,Local], /, Entry),

  % Assert the words that appear in the dictionary.
  call_archive_entry(Iri, Entry, dict_assert(Lang)).



%! dict_file(+Language:string, -File:atom) is det.

dict_file(Lang, File):-
  absolute_file_name(Lang, File, [access(write),extensions([dic])]).



%! dict_init is det.
% Initialize the dictionary for each supported language.

dict_init:-
  forall(dict_lang(Lang), dict_init(Lang)).


%! dict_init(+Language:string) is det.
% Initialize the dictionary for the given Language.

dict_init(Lang):-
  dict_file(Lang, File),
  (exists_file(File) -> true ; touch(File)),
  db_attach(File, []),
  file_age(File, Age),
  dict_update(Lang, Age).



%! dict_iri(?Language:string, -Iri:atom) is nondet.

dict_iri("en-US", Iri):-
  atomic_list_concat(
    [
      '',
      firefox,
      downloads,
      file,
      '199368',
      'united_states_english_spellchecker-7.0-fx+sm+tb.xpi'
    ],
    /,
    Path
  ),
  uri_components(
    Iri,
    uri_components(https,'addons.mozilla.org',Path,'src=dp-btn-primary',_)
  ).



%! dict_lang(+Language:string) is semidet.
%! dict_lang(-Language:string) is multi.

dict_lang(Lang):-
  dict_iri(Lang, _).



%! dict_update(+Language:string, +Age:float) is det.

% The persistent store is still fresh.
dict_update(Lang, Age):-
  once(word(Lang, _, _, _)),
  Age < 8640000, !.
% The persistent store has become stale, so refresh it.
dict_update(Lang, _):-
  retractall_word(Lang, _, _, _),
  dict_download(Lang).



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
