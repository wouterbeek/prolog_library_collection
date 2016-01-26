:- module(
  ansi_ext,
  [
    ansi_format_list/1,   % +Messages:list(compound)
    ansi_format_listln/1, % +Messages:list(compound)
    ansi_formatln/3       % +AnsiAttributes, +Format, +Args
  ]
).

:- reexport(library(ansi_term)).

/** <module> ANSI extensions

Predicates for using ANSI markup in the console.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(error)).





%! ansi_format_list(+Messages:list(compound)) is det.
% Writes a list of ANSI-formatted messages to standard output.
%
% Messages can be of the following form:
%   * msg(+AnsiAttributes:list, +Format:atom, +Arguments:list)
%   * msg(+AnsiAttributes:list, +Format:atom, +Argument)
%   * msg(+AnsiAttributes:list, +Format:atom)
%   * msg(+Format:atom, +Arguments:list)
%   * msg(+Format:atom, +Argument)
%   * msg(+Format:atom)
%   * Format:atom

ansi_format_list([]):- !.
ansi_format_list([msg(Attrs,Format,Args)|T]):-
  is_list(Args), !,
  ansi_format(Attrs, Format, Args),
  ansi_format_list(T).
ansi_format_list([msg(Attrs,Format,Arg)|T]):- !,
  ansi_format_list([msg(Attrs,Format,[Arg])|T]).
ansi_format_list([msg(Attrs,Format)|T]):-
  is_list(Attrs), !,
  ansi_format_list([msg(Attrs,Format,[])|T]).
ansi_format_list([msg(Format,Args)|T]):-
  ansi_format_list([msg([],Format,Args)|T]).
ansi_format_list([msg(Format)|T]):- !,
  ansi_format_list([msg([],Format,[])|T]).
ansi_format_list([Format|T]):-
  atom(Format), !,
  ansi_format_list([msg([],Format,[])|T]).
ansi_format_list([H|_]):-
  type_error(ansi_message, H).


%! ansi_format_listln(+Messages:list(compound)) is det.

ansi_format_listln(L):-
  ansi_format_list(L),
  nl.


%! ansi_formatln(+AnsiAttributes, +Format, +Args) is det.

ansi_formatln(Attrs, Format, Args):-
  ansi_format(Attrs, Format, Args),
  nl.
