:- module(
  dcg_cli,
  [
    ansi_str//2,  % +Modifiers, +Str
    bold//1,      % +Str
    enumerate//1, % +L
    enumerate//2, % :Dcg_1, +L
    enumerate//3, % :Dcg_1, +L, +Opts
    itemize//1,   % +L
    itemize//2,   % :Dcg_1, +L
    itemize//3,   % :Dcg_1, +L, +Opts
    section//1,   % :Header_0
    section//2,   % +Header_0, :Content_0
    section//3    % +Indent, +Header_0, :Content_0
  ]
).

/** <module> DCG CLI

@author Wouter Beek
@version 2016/06, 2016/08, 2016/10
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(dict_ext)).

:- meta_predicate
    enumerate(3, +, ?, ?),
    enumerate(3, +, +, ?, ?),
    enumerate_or_itemize0(+, 3, +, +, ?, ?),
    itemize(3, +, ?, ?),
    itemize(3, +, +, ?, ?),
    section(//, ?, ?),
    section(//, //, ?, ?),
    section(+, //, //, ?, ?).

:- multifile
    dcg:dcg_hook//1.

dcg:dcg_hook(bold(Term)) -->
  bold(Term).





%! ansi_str(+Modifiers, +Str)// is det.
%
% # Text attributes
%
%   | **Modifier**       | **Code** |
%   |:-------------------|:---------|
%   | All attributes off | 0        |
%   | Bold on            | 1        |
%   | Dim on             | 2        |
%   | Standout on        | 3        |
%   | Underscore on      | 4        |
%   | Blink on           | 5        |
%   | Reverse video on   | 7        |
%   | Concealed on       | 8        |
%
% # Foreground colors
%
%   | **Modifier** | **Code** |
%   |:-------------|:---------|
%   | Black        | 30       |
%   | Red          | 31       |
%   | Green        | 32       |
%   | Yellow       | 33       |
%   | Blue         | 34       |
%   | Magenta      | 35       |
%   | Cyan         | 36       |
%   | White        | 37       |
%   | Default      | 39       |
%
% # Background colors
%
%   | **Modifier** | **Code** |
%   |:-------------|:---------|
%   | Black        | 40       |
%   | Red          | 41       |
%   | Green        | 42       |
%   | Yellow       | 43       |
%   | Blue         | 44       |
%   | Magenta      | 45       |
%   | Cyan         | 46       |
%   | White        | 47       |
%   | Default      | 49       |

ansi_str(Modifiers, Str) -->
  "\e[",
  *&(integer, ";", Modifiers),
  "m",
  atom(Str),
  "\e[0m", !.



%! bold(+Str)// is det.

bold(Str) -->
  ansi_str([1], Str).



%! enumerate(+L)// is det.
%! enumerate(:Dcg_1, +L)// is det.
%! enumerate(:Dcg_1, +L, +Opts)// is det.
%
% The following options are supported:
%
%   * first_item(+nonneg) The number of the first list item.  Default
%   is 1.
%
%   * indent(+nonneg) Default is 0.

enumerate(L) -->
  enumerate(pl_term, L).


enumerate(Dcg_1, L) -->
  enumerate(Dcg_1, L, _{}).


enumerate(Dcg_1, L, Opts1) -->
  {
    del_dict(first_item, Opts1, First, Opts2),
    put_dict(item_number, Opts2, First, Opts3)
  },
  enumerate_or_itemize0(enumerate, Dcg_1, L, Opts3).



%! itemize(+L)// is det.
%! itemize(:Dcg_1, +L)// is det.
%! itemize(:Dcg_1, +L, +Opts)// is det.
%
% The following options are supported:
%
%   * indent(+nonneg) Default is 0.

itemize(L) -->
  itemize(pl_term, L).


itemize(Dcg_1, L) -->
  itemize(Dcg_1, L, _{}).


itemize(Dcg_1, L, Opts) -->
  enumerate_or_itemize0(itemize, Dcg_1, L, Opts).



%! section(:Header_0)// is det.
%! section(:Header_0, :Content_0)// is det.
%! section(+Indent, :Header_0, :Content_0)// is det.

section(Header_0) -->
  Header_0, ":", nl.


section(Header_0, Content_0) -->
  section(0, Header_0, Content_0).


section(I, Header_0, Content_0) -->
  dcg_tab(I)
  Header_0,
  nl,
  Content_0.





% HELPERS %

enumerate_or_itemize0(_, _, [], _) --> !, "".
enumerate_or_itemize0(Mode, Dcg_1, [H|T], Opts) -->
  tab(Opts.indent),
  enumerate_or_itemize_prefix0(Mode, Opts),
  {dict_inc(item_number, Opts)},
  dcg_call_cp(Dcg_1, H),
  nl,
  enumerate_or_itemize0(Mode, Dcg_1, T, Opts).


enumerate_or_itemize_prefix0(enumerate, Opts) --> !,
  thousands(Opts.item_number),
  ". ".
enumerate_or_itemize_prefix0(itemize, _) -->
  "* ".
