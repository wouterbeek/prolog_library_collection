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
    pp_dict/1,          % :Goal_1
    print_dict/1,       % +Dict
    print_dict/2,       % :Dcg_1, +Dict
    print_dict/3,       % :Dcg_1, +Dict, +Opts
    print_table/1,      % +Rows
    print_table/2,      % +Rows, :Opts
    print_term/1,       % +Term
   %print_term/2,       % +Term, +Opts
    print_tree/1,       % +Tree
    print_tree/2        % +Tree, :Opts
  ]
).
:- reexport(library(ansi_term)).
:- reexport(library(pprint)).

/** <module> Print extensions

Additional predicates for printing.

@author Wouter Beek
@version 2015/08-2017/03
*/

:- use_module(library(check_installation), []).
:- use_module(library(dcg)).
:- use_module(library(dcg_pl)).
:- use_module(library(dcg_table)).
:- use_module(library(dcg_tree)).
:- use_module(library(settings)).

is_meta(node_writer).

:- meta_predicate
    pp_dict(1),
    print_dict(3, +),
    print_dict(3, +, +).

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
%
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
%
% Prints a notification message, using ANSI properties.

msg_notification(Format) :-
  msg_notification(Format, []).


msg_notification(Format, Args) :-
  ansi_format(user_output, [fg(green)], Format, Args).



%! msg_success(+Format) is det.
%! msg_success(+Format, +Args) is det.
%
% Prints a success message, using ANSI properties.

msg_success(Format) :-
  msg_success(Format, []).


msg_success(Format, Args) :-
  ansi_format(user_output, [bold,fg(green)], Format, Args).



%! msg_warning(+Format) is det.
%! msg_warning(+Format, +Args) is det.
%
% Prints a warnings message, using ANSI properties.

msg_warning(Format) :-
  msg_warning(Format, []).


msg_warning(Format, Args) :-
  ansi_format(user_error, [bold,fg(red)], Format, Args).



%! pp_dict(:Goal_1) .
%
% Print the dictionary argument of the given Goal_1.

pp_dict(Goal_1) :-
  catch(call(Goal_1, Dict), E, true),
  (var(E) -> print_dict(Dict) ; print_message(warning, E)).



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



%! print_term(+Term) is det.
%! print_term(+Term, +Opts) is det.

print_term(Term) :-
  print_term(Term, []).



%! print_tree(+Tree) is det.
%! print_tree(+Tree, :Opts) is det.

print_tree(Tree) :-
  print_tree(Tree, []).


print_tree(Tree, Opts1) :-
  meta_options(is_meta, Opts1, Opts2),
  option(output(Sink), Opts2, current_output),
  dcg_with_output_to(Sink, dcg_tree(Tree, Opts2)).
