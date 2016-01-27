:- module(
  dcg_content,
  [
    '...'//0,
    '...'//1,          % -Codes
    dcg_tab//0,
    done//0,
    eol//0,
    nl//0,
    indent//1,         % +Indent:nonneg
    indent//2,         % +Indent:nonneg, :Dcg_0
    indent_nl//2,      % +Indent:nonneg, :Dcg_0
    iri//1,            % +Iri
    nonblank//0,
    nvpair//1,         % +Pair:pair
    nvpair//2,         % :Name_0, :Value_0
    parsing//0,
    rest//0,
    rest//1,           % -Rest:list(code)
    section//3,        % +Indent:nonneg, +Message:string, :Dcg_0
    skip_line//0,
    string//0,
    string_without//1, % +EndCodes
    tab//1,            % +Indent:nonneg
    tab//2,            % +Indent:nonneg, :Dcg_0
    tab_nl//2          % +Indent:nonneg, :Dcg_0
  ]
).
:- reexport(library(dcg/basics), except([digit//1,digits//1])).

/** <module> DCG content

DCG rules for parsing/generating often-occuring content.

@author Wouter Beek
@version 2015/07-2015/08, 2015/10-2016/01
*/

:- use_module(library(settings)).

:- meta_predicate
    indent(+,//,?,?),
    indent_nl(+,//,?,?),
    nvpair(//,//,?,?),
    section(+,+,//,?,?),
    tab(+,//,?,?),
    tab_nl(+,//,?,?).

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



%! dcg_tab// is det.

dcg_tab --> tab(1).



%! done// .

done(_, _).



%! eol// .

eol --> "\n".
eol --> "\r".



%! indent(+Indent:nonneg)// is det.

indent(0) --> !, "".
indent(N1) --> " ", !, {N2 is N1 - 1}, indent(N2).


%! indent(+Indent:nonneg, :Dcg_0)// is det.

indent(I, Dcg_0) --> indent(I), Dcg_0.


%! indent_nl(+Indent:nonneg, :Dcg_0)// is det.

indent_nl(I, Dcg_0) --> indent(I, Dcg_0), nl.



%! iri(+Iri:atom)// is det.

iri(Iri) --> "<", atom(Iri), ">".



%! nl// is det.

nl --> "\n".



%! nonblank// .
% Wrapper around nonblank//1 from library(dcg/basics).

nonblank --> nonblank(_).



%! nvpair(+Pair:pair)// is det.

nvpair(N-V) --> nvpair(pl_term(N), pl_term(V)).


%! nvpair(:Name_0, :Value_0)// is det.

nvpair(N_0, V_0) --> N_0, ": ", V_0.



%! parsing// is semidet.

parsing(H, H):- nonvar(H).



%! rest// is det.
% Same as `rest --> "".'

rest(X, X).


%! rest(-Rest:list(code))// is det.

rest(X, X, []).



%! section(+Indent:nonneg, +Message:string, :Dcg_0)// is det.

section(I, Msg, Dcg_0) --> tab_nl(I, atom(Msg)), Dcg_0.



%! skip_line// is det.

skip_line --> ..., eol, !.



%! string// .
% Wrapper around string//1.

string --> string(_).



%! string_without(+EndCodes:list(code))// .
% Wrapper around string_without//2.

string_without(End) --> string_without(End, _).



%! tab(+Indent:nonneg)// is det.

tab(I) --> {setting(tab_size, N0), N is I * N0}, indent(N).


%! tab(+Indent:nonneg, :Dcg_2)// is det.

tab(I, Dcg_0) --> tab(I), Dcg_0.



%! tab_nl(+Indent:nonneg, :Dcg_0)// is det.

tab_nl(I, Dcg_0) --> tab(I, Dcg_0), nl.
