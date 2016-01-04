:- module(
  dcg_content,
  [
    '...'//0,
    '...'//1, % -Codes:list(code)
    dcg_done//0,
    dcg_rest//0,
    dcg_rest//1, % -Rest:list(code)
    dcg_eol//0,
    dcg_nl//0,
    dcg_tab//0,
    dcg_tab//1, % +Indent:nonneg
    dcg_tab//2, % +Indent:nonneg
                % :Dcg_0
    dcg_tab_nl//2 % +Indent:nonneg
                  % :Dcg_0
    indent//1, % +Indent:nonneg
    indent//2, % +Indent:nonneg
               % :Dcg_0
    indent_nl//2, % +Indent:nonneg
                  % :Dcg_0
    iri//1, % +Iri:atom
    nonblank//0,
    nvpair//1, % +Pair:pair(atom)
    nvpair//2, % :Name_0
               % :Value_0
    parsing//0,
    section//3, % +Indent:nonneg
                % +Message:string
                % :Dcg_0
    skip_line//0,
    string//0,
    string_without//1, % +EndCodes:list(code)
  ]
).
:- reexport(library(dcg/basics), except([digit//1,digits//1])).

/** <module> DCG content

DCG rules for parsing/generating often-occuring content.

@author Wouter Beek
@version 2015/07-2015/08, 2015/10-2015/12
*/

:- use_module(library(settings)).

:- meta_predicate(indent(+,//,?,?)).
:- meta_predicate(indent_nl(+,//,?,?)).
:- meta_predicate(nvpair(//,//,?,?)).
:- meta_predicate(section(+,+,//,?,?)).
:- meta_predicate(dcg_tab(+,//,?,?)).
:- meta_predicate(dcg_tab_nl(+,//,?,?)).

% The number of spaces that go into one tab.
:- setting(
     tab_size,
     integer,
     2,
     'The number of spaces that go into one tab.'
   ).





%! ...// .
% Wrapper around ...//1 that does not return the processed codes.

... --> ...(_).


%! ...(-Codes:list(code))// .
% Wrapper around string//1.

...(Cs) --> string(Cs).



%! dcg_done// .

dcg_done(_, _).



%! dcg_eol// .

dcg_eol --> "\n".
dcg_eol --> "\r".



%! dcg_nl// is det.

dcg_nl --> "\n".



%! dcg_rest// is det.
% Same as `dcg_rest --> "".'

dcg_rest(X, X).


%! dcg_rest(-Rest:list(code))// is det.

dcg_rest(X, X, []).



%! dcg_tab// is det.

dcg_tab --> dcg_tab(1).


%! tab(+Indent:nonneg)// is det.

dcg_tab(I) --> {setting(tab_size, N0), N is I * N0}, indent(N).


%! dcg_tab(+Indent:nonneg, :Dcg_2)// is det.

dcg_tab(I, Dcg_0) --> dcg_tab(I), Dcg_0.



%! dcg_tab_nl(+Indent:nonneg, :Dcg_0)// is det.

dcg_tab_nl(I, Dcg_0) --> dcg_tab(I, Dcg_0), nl.



%! indent(+Indent:nonneg)// is det.

indent(0) --> !, "".
indent(N1) --> " ", !, {N2 is N1 - 1}, indent(N2).


%! indent(+Indent:nonneg, :Dcg_0)// is det.

indent(I, Dcg_0) --> indent(I), Dcg_0.


%! indent_nl(+Indent:nonneg, :Dcg_0)// is det.

indent_nl(I, Dcg_0) --> indent(I, Dcg_0), nl.



%! iri(+Iri:atom)// is det.

iri(Iri) --> "<", atom(Iri), ">".



%! nonblank// .
% Wrapper around nonblank//1 from library(dcg/basics).

nonblank --> nonblank(_).



%! nvpair(+Pair:pair(atom))// is det.

nvpair(N-V) --> nvpair(atom(N), atom(V)).


%! nvpair(:Name_0, :Value_0)// is det.

nvpair(N_0, V_0) --> N_0, ": ", V_0.



%! parsing// is semidet.

parsing(H, H):- nonvar(H).



%! section(+Indent:nonneg, +Message:string, :Dcg_0)// is det.

section(I, Msg, Dcg_0) --> indent_nl(I, atom(Msg)), Dcg_0.



%! skip_line// is det.

skip_line --> ..., eol, !.



%! string// .
% Wrapper around string//1.

string --> string(_).



%! string_without(+EndCodes:list(code))// .
% Wrapper around string_without//2.

string_without(End) --> string_without(End, _).
