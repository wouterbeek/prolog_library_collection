:- module(
  debug_ext,
  [
    debug_dict/2,   % +Flag, +Dict
    format_debug/3, % +Flag, +Out, +Pattern
    format_debug/4, % +Flag, +Out, +Pattern, +Args
    indent_debug/3, % +Mode, +Flag, +Format
    indent_debug/4  % +Mode, +Flag, +Format, +Args
  ]
).
:- reexport(library(debug)).

/** <module> Debug extensions

@author Wouter Beek
@version 2017/09-2017/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(error)).
:- use_module(library(pp)).

:- thread_local
   debug_indent/1.

debug_indent(0).





%! debug_dict(+Flag:compound, +Dict:dict) is det.

debug_dict(Flag, Dict) :-
  debugging(Flag), !,
  with_output_to(string(String), print_term(Dict)),
  debug(Flag, "~s", [String]).
debug_dict(_, _).



%! format_debug(+Flag, +Out:stream, +Pattern:string) is det.
%! format_debug(+Flag, +Out:stream, +Pattern:string,
%!              +Args:list(term)) is det.

format_debug(Flag, Out, Pattern) :-
  format_debug(Flag, Out, Pattern, []).


format_debug(Flag, Out, Pattern, Args) :-
  string_concat(Pattern, "\n", PatternNewline),
  format(Out, PatternNewline, Args),
  debug(Flag, Pattern, Args).



%! indent_debug(+Mode:oneof([-1,0,1]), +Flag:compound, +Format:string) is det.
%! indent_debug(+Mode:oneof([-1,0,1]), +Flag:compound, +Format:string,
%!              +Args:list(term)) is det.

indent_debug(Mode, Flag, Format) :-
  indent_debug(Mode, Flag, Format, []).


indent_debug(Mode, Flag, Format, Args) :-
  debugging(Flag), !,
  must_be(oneof([-1,0,1]), Mode),
  (retract(debug_indent(N1)) -> N2 is max(0, N1 + Mode) ; N2 = Mode),
  assert(debug_indent(N2)),
  format(string(Msg1), Format, Args),
  (Mode =:= -1 -> N = N1 ; N = N2),
  dcg_with_output_to(string(Msg2), msg1(Mode, N, Msg1)),
  debug(Flag, Msg2, []).
indent_debug(_, _, _, _).

msg1(_, 0, Msg) --> !,
  atom(Msg).
msg1(Diff, 1, Msg) --> !,
  msg_diff1(Diff),
  "─",
  atom(Msg).
msg1(Diff, N1, Msg) -->
  ({N1 =:= 1} -> msg_diff2(Diff), "─" ; "│ "),
  {N2 is N1 - 1},
  msg1(Diff, N2, Msg).

msg_diff1(1) --> !, "┌".
msg_diff1(0) --> !, "─".
msg_diff1(-1) --> "└".

msg_diff2(1) --> !, "├".
msg_diff2(0) --> !, "└".
msg_diff2(-1) --> "└".
