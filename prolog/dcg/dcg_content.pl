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
               % :Dcg_2
    indent_nl//2, % +Indent:nonneg
                  % :Dcg_2
    iri//1, % +Iri:atom
    nl//0,
    nonblank//0,
    nvpair//1, % +Pair:pair(atom)
    nvpair//2, % :Name_2
               % :Value_2
    parsing//0,
    section//3, % +Indent:nonneg
                % +Message:string
                % :Dcg_2
    skip_line//0,
    string//0,
    string_without//1 % +EndCodes:list(code)
  ]
).
:- reexport(library(dcg/basics)).

/** <module> DCG content

DCG rules for parsing/generating often-occuring content.

@author Wouter Beek
@version 2015/07-2015/08, 2015/10
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_bracketed)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(settings)).

:- meta_predicate(indent(+,//,?,?)).
:- meta_predicate(indent_nl(+,//,?,?)).
:- meta_predicate(nvpair(//,//,?,?)).
:- meta_predicate(section(+,+,//,?,?)).

% The number of spaces that go into one indent.
:- setting(
     indent_size,
     integer,
     2,
     'The number of spaces that go into one indent.'
   ).





%! ...// .
% Wrapper around ...//1 that does not return the processed codes.

... -->
  ...(_).


%! ...(-Codes:list(code))// .
% Wrapper around string//1.

...(Cs) -->
  string(Cs).



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


%! indent(+Indent:nonneg, :Dcg_2)// is det.

indent(I, Dcg_2) -->
  indent(I),
  Dcg_2.



%! indent_nl(+Indent:nonneg, :Dcg_2)// is det.

indent_nl(I, Dcg_2) -->
  indent(I),
  Dcg_2,
  nl.



%! iri(+Iri:atom)// is det.

iri(Iri) -->
  bracketed(langular, atom(Iri)).



%! nl// is det.

nl -->
  "\n".



%! nonblank// .
% Wrapper around nonblank//1 from library(dcg/basics).

nonblank --> nonblank(_).



%! nvpair(+Pair:pair(atom))// is det.

nvpair(N-V) -->
  nvpair(atom(N), atom(V)).


%! nvpair(:Name_2, :Value_2)// is det.

nvpair(N, V) -->
  N,
  ": ",
  V.



%! parsing// is semidet.

parsing(H, H):-
   nonvar(H).



%! section(+Indent:nonneg, +Message:string, :Dcg_2)// is det.

section(I, Msg, Dcg_2) -->
  indent_nl(I, atom(Msg)),
  Dcg_2.



%! skip_line// .

skip_line --> eol, !.
skip_line --> [_], skip_line.



%! string// .
% Wrapper around string//1.

string -->
  string(_).



%! string_without(+EndCodes:list(code))// .
% Wrapper around string_without//2.

string_without(End) -->
  string_without(End, _).
