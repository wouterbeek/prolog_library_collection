:- module(
  dcg_content,
  [
    dcg_cp//0,
    dcg_done//0,
    dcg_rest//1, % -Rest:list(code)
    dcg_void//0,
    indent//0,
    indent//1, % +Indent:nonneg
    indent//2, % +Indent:nonneg
               % :Dcg_0
    nl//0,
    parsing//0,
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

:- meta_predicate(indent(+,//,?,?)).

% The number of spaces that go into one indent.
:- setting(
  indent_size,
  integer,
  2,
  'The number of spaces that go into one indent.'
).



%! dcg_cp// .

dcg_cp(X, X).


%! dcg_done// .

dcg_done(_, _).


%! dcg_rest(-Rest:list(code))// is det.

dcg_rest(X, X, []).


%! dcg_void// .

dcg_void --> "".


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

%! indent(+Indent:nonneg, :Dcg_0)// is det.

indent(I, Dcg_0) -->
  indent(I),
  Dcg_0.


%! nl// is det.

nl -->
  "\n".


%! parsing// is semidet.

parsing(H, H):-
   nonvar(H).


%! pl_term(+Term)// is det.

pl_term(T) -->
  {with_output_to(codes(Cs), write_canonical_blobs(T))},
  Cs.
