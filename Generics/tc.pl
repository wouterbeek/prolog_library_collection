:- module(
  tc,
  [
  ]
).

/** <module> Text Categorization

@author Wouter Beek
@version 2013/01
@tbd This is work in progress.
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(file_ext)).
:- use_module(generics(meta_ext)).
:- use_module(standards(ascii)).

:- dynamic(doc(_Doc)).
:- dynamic(word0(_Word, _Sum, _Docs)).

:- debug(tc).



clean:-
  retractall(doc(_Doc)),
  retractall(word0(_Word, _Sum, _Docs)),
  flag(ex, _Examples, 0),
  flag(neg, _NegativeExamples, 0),
  flag(pos, _PositiveExamples, 0),
  reset_memo.

cosine_normalization(Term, Doc, Weight):-
gtrace, %TODO
  memo(tfidf(Term, Doc, TFIDF)),
  doc_to_vec(Doc, Vec),
  findall(
    TFIDF,
    (
      member(Term0-_Occur, Vec),
      memo(tfidf(Term0, Doc, TFIDF))
    ),
    TFIDFs
  ),
  sum_list(TFIDFs, Temp1),
  memo(tfidf(Term, Doc, Temp0)),
  Weight is Temp0 / sqrt(Temp1^2).

doc_to_vec(Doc, Vec):-
  doc(Doc),
  !,
  setoff(
    Word-Occur,
    (
      word0(Word, _Sum, Docs),
      (
        memberchk(Doc-Occur, Docs)
      ->
        true
      ;
        Occur = 0
      )
    ),
    Vec
  ).

document_frequency(Term, DF):-
  word0(Term, _Sum, Docs),
  !,
  length(Docs, DF).

load_examples:-
  absolute_file_name(data('Newsgroups'), Dir),
  path_walk_tree(Dir, '.+', Files),
  debug(tc, 'About to load ~w files.', [Files]),
  forall(
    member(File, Files),
    (
      open(File, read, Stream, []),
      set_stream(Stream, buffer(line)),
      load_examples(Stream),
      close(Stream)
    )
  ).

load_examples(Stream):-
  at_end_of_stream(Stream),
  !.
load_examples(Stream):-
  read_line_to_codes(Stream, Line),
  parse_char(SpaceCode, 'SP', 'White space'),
  split_codes(Line, [SpaceCode], ListOfCodes),
  flag(ex, Examples, Examples + 1),
  format(atom(Doc), 'd~w', [Examples]),
  assert(doc(Doc)),
  maplist(atom_codes, Words, ListOfCodes),
  maplist(store_word(Doc), Words),
  load_examples(Stream).

print_doc(Stream, Doc):-
  format(Stream, '~w:\n', [Doc]),
  doc_to_vec(Doc, Vec),
  print_vec(Stream, Vec).

print_docs(Stream):-
  forall(
    doc(Doc),
    print_doc(Stream, Doc)
  ).

print_vec(Stream, Vec):-
  maplist(print_vec0(Stream), Vec).

print_vec0(Stream, Word-Occur):-
  format(Stream, '\t~w\t~w\n', [Word, Occur]).

store_word(Doc, Word):-
  retract(word0(Word, Sum, Docs)),
  !,
  succ(Sum, NewSum),
  (
    selectchk(Doc-Occur, Docs, Docs0)
  ->
    succ(Occur, NewOccur),
    ord_add_element(Docs0, Doc-NewOccur, NewDocs)
  ;
    ord_add_element(Docs, Doc-1, NewDocs)
  ),
  assert(word0(Word, NewSum, NewDocs)).
store_word(Doc, Word):-
  assert(word0(Word, 1, [Doc-1])).

term_in_doc(Term, Doc, Occur):-
  word0(Term, _Sum, Docs),
  memberchk(Doc-Occur, Docs),
  !.
term_in_doc(_Term, _Doc, 0).

test:-
  clean,
  load_examples,
  print_docs(user),
  forall(
    word0(Term, _Sum, _Docs),
    forall(
      doc(Doc),
      (
        cosine_normalization(Term, Doc, Weight),
        format(user, 'weight(~w,~w)\t~w\n', [Term, Doc, Weight])
      )
    )
  ).

tfidf(Term, Doc, TFIDF):-
  document_frequency(Term, DF),
  count(doc(_), Docs),
  Temp is Docs / DF,
  
  term_in_doc(Term, Doc, Occur),
  TFIDF is Occur * log10(Temp).

