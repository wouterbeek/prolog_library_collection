:- module(
  dcg_content,
  [
    '...'//0,
    '...'//1, % -Codes:list(code)
    dcg_cp//0,
    dcg_done//0,
    dcg_rest//1, % -Rest:list(code)
    dcg_void//0,
    eol//0,
    indent//0,
    indent//1, % +Indent:nonneg
    indent//2, % +Indent:nonneg
               % :Dcg_0
    nl//0,
    nonblank//0,
    parsing//0,
    skip_line//0,
    string_without//1 % +EndCodes
  ]
).
:- reexport(library(dcg/basics)).

/** <module> DCG content

DCG rules for parsing/generating often-occuring content.

@author Wouter Beek
@version 2015/07-2015/08
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(settings)).

:- meta_predicate(indent(+,//,?,?)).

% The number of spaces that go into one indent.
:- setting(
  indent_size,
  integer,
  2,
  'The number of spaces that go into one indent.'
).





%! ...// .

... -->
  ...(_).

%! ...(-Codes:list(code))// .

...([]) --> "".
...([H|T]) --> [H], ...(T).



%! dcg_cp// .

dcg_cp(X, X).



%! dcg_done// .

dcg_done(_, _).



%! dcg_rest(-Rest:list(code))// is det.

dcg_rest(X, X, []).



%! dcg_void// .

dcg_void --> "".



%! eol// .

eol --> "\n".
eol --> "\r".



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

nl --> "\n".



%! nonblank// .
% Wrapper around nonblank//1 from library(dcg/basics).

nonblank --> nonblank(_).



%! parsing// is semidet.

parsing(H, H):-
   nonvar(H).



%! skip_line// .

skip_line --> eol, !.
skip_line --> [_], skip_line.



%! string_without(+EndCodes:list(code))// .

string_without(Ends) --> string_without(Ends, _).
