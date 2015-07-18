:- module(
  dcg_content,
  [
    indent//0,
    indent//1, % +Indent:nonneg
    nl//0,
    pl_term//1 % +Term
  ]
).

/** <module> DCG content

DCG rules for parsing/generating often-occuring content.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(pl/pl_term)).
:- use_module(library(settings)).

% The number of spaces that go into one indent.
:- setting(
  indent_size,
  integer,
  2,
  'The number of spaces that go into one indent.'
).





%! indent// is det.

indent -->
  indent(1).


%! indent(+Indent:nonneg)// is det.

indent(I) -->
  {
    setting(indent_size, Size),
    NSpaces is I * Size
  },
  '#'(NSpaces, space, []), !.



%! nl// is det.

nl -->
  "\n".



%! pl_term(+Term)// is det.

pl_term(T) -->
  {with_output_to(codes(Cs), write_canonical_blobs(T))},
  Cs.
