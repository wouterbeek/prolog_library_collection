:- module(
  print_ext,
  [
    ansi_format/4,      % +Sink, +Attrs, +Format, +Args
    error_kind/1,       % +Kind
    formatln/1,         % +Format
    formatln/2,         % +Format, +Args
    formatln/3,         % +Out, +Format, +Args
    indent/1,           % +NumSpaces
    msg_notification/1, % +Format
    msg_notification/2, % +Format, +Args
    msg_success/1,      % +Format
    msg_success/2,      % +Format, +Args
    msg_warning/1,      % +Format
    msg_warning/2,      % +Format, +Args
    print_dict/1,       % +Dict
    print_dict/2,       % :Dcg_1, +Dict
    print_dict/3,       % :Dcg_1, +Dict, +Opts
    print_table/1,      % +Rows
    print_table/2,      % +Rows, :Opts
    print_tree/1,       % +Tree
    print_tree/2,       % +Tree, :Opts
    tab/0,
    verbose/1,          % :Goal_0
    verbose/2,          % :Goal_0, +Format
    verbose/3           % :Goal_0, +Format, +Args
  ]
).
:- reexport(library(ansi_term)).

/** <module> Print extensions

Additional predicates for printing.

@author Wouter Beek
@version 2015/08, 2015/10-2015/11, 2016/01-2016/03, 2016/05, 2016/08, 2016/10
*/

:- use_module(library(check_installation), []).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(dcg/dcg_table)).
:- use_module(library(dcg/dcg_tree)).
:- use_module(library(settings)).

is_meta(node_writer).

:- meta_predicate
    print_dict(3, +),
    print_dict(3, +, +),
    verbose(0),
    verbose(0, +),
    verbose(0, +, +).

:- setting(
     screen_width,
     integer,
     80,
     "The default width of the screen in number of characters."
   ).





%! ansi_format(+Sink, +Attrs, +Format, +Args) is det.
%
% The following attributes are supported:
%
%   - bg(+atom)
%   - blink(slow)
%   - blink(rapid)
%   - `bold`
%   - `conceal`
%   - `crossed_out`
%   - `encircled`
%   - `faint`
%   - fg(+atom)
%   - font(primary)
%   - font(+between(1,8))
%   - `fraktur`
%   - `framed`
%   - hbg(+atom)
%   - hfg(+atom)
%   - ideogram(overlined)
%   - ideogram(stress_marking)
%   - ideogram(underline)
%   - ideogram(underline(double))
%   - intensity(normal)
%   - `italic`
%   - `left_side_line`
%   - `negative`
%   - `overlined`
%   - `reset`
%     Turn all attributes off.
%   - `right_side_line`
%   - right_side_line(double)
%   - `underline`
%   - underline(double)

ansi_format(Sink, Attrs, Format, Args) :-
  with_output_to(Sink, ansi_format(Attrs, Format, Args)).



%! error_kind(+Kind) is semidet.

error_kind(Kind) :-
  check_installation:error_kind(Kind).



%! formatln(+Format) is det.
%! formatln(+Format, +Args) is det.
%! formatln(+Out, +Format, +Args) is det.
% Variants of format/[1-3] with a newline appended.

formatln(Format) :-
  format(Format), nl.

formatln(Format, Args) :-
  format(Format, Args), nl.

formatln(Out, Format, Args) :-
  format(Out, Format, Args),
  nl(Out).



%! indent(+NumSpaces) is det.

indent(N) :-
  N < 1, !.
indent(N1) :-
  write(" "),
  N2 is N1 - 1,
  indent(N2).



%! msg_notfication(+Format) is det.
%! msg_notification(+Format, +Args) is det.
% Prints a notification message, using ANSI properties.

msg_notification(Format) :-
  msg_notification(Format, []).


msg_notification(Format, Args) :-
  ansi_format(user_output, [fg(green)], Format, Args).



%! msg_success(+Format) is det.
%! msg_success(+Format, +Args) is det.
% Prints a success message, using ANSI properties.

msg_success(Format) :-
  msg_success(Format, []).


msg_success(Format, Args) :-
  ansi_format(user_output, [bold,fg(green)], Format, Args).



%! msg_warning(+Format) is det.
%! msg_warning(+Format, +Args) is det.
% Prints a warnings message, using ANSI properties.

msg_warning(Format) :-
  msg_warning(Format, []).


msg_warning(Format, Args) :-
  ansi_format(user_error, [bold,fg(red)], Format, Args).



%! print_dict(+Dict) is det.
%! print_dict(:Dcg_1, +Dict) is det.
%! print_dict(:Dcg_1, +Dict, +Opts) is det.

print_dict(Dict) :-
  print_dict(pl_term, Dict).


print_dict(Dcg_1, Dict) :-
  print_dict(Dcg_1, Dict, _{out: user_output, indent: 0}).


print_dict(Dcg_1, Dict, Opts) :-
  dcg_with_output_to(Opts.out, dcg_dict(Dcg_1, Dict, Opts.indent)),
  nl.



%! print_table(+Rows) is det.
%! print_table(+Rows, :Opts) is det.
%
% The following options are supported:
%
%   - out(+stream)
%
%   - Other options are passed to dcg_table//2.
%
% @tbd Cell writer should be a hook.

print_table(Rows) :-
  print_table(Rows, []).


print_table(Rows, Opts) :-
  option(out(Out), Opts, current_output),
  dcg_with_output_to(Out, dcg_table(Rows, Opts)).



%! print_tree(+Tree) is det.
%! print_tree(+Tree, :Opts) is det.

print_tree(Tree) :-
  print_tree(Tree, []).


print_tree(Tree, Opts1) :-
  meta_options(is_meta, Opts1, Opts2),
  option(output(Sink), Opts2, current_output),
  dcg_with_output_to(Sink, dcg_tree(Tree, Opts2)).



%! tab is det.
%
% Wrapper around tab/1 that prints a single horizontal tab character.

tab:-
  tab(1).



%! verbose(:Goal_0) is det.
%! verbose(:Goal_0, +Format) is det.
%! verbose(:Goal_0, +Format, +Args) is det.
% Verbose calling of Goal_0.

verbose(Goal_0) :-
  term_string(Goal_0, Format),
  verbose(Goal_0, Format).


verbose(Goal_0, Format) :-
  verbose(Goal_0, Format, []).


verbose(Goal_0, Format, Args) :-
  get_time(Start),
  format(Format, Args),
  (   catch(Goal_0, E, true)
  ->  (   var(E)
      ->  get_time(End),
          Delta is End - Start,
          msg_success("~`.t success (~2f sec.)~72|", [Delta])
      ;   message_to_string(E, S),
          msg_warning("~`.t ERROR: ~w~72|", [S])
      )
  ;   msg_warning("~`.t ERROR: (failed)~72|")
  ),
  nl.
