:- module(
  cli_help,
  [
    cli_help/3 % +Name, +Usages, +Specs
  ]
).

/** <module> Command-line tools: help messages

*/

:- use_module(library(apply)).
:- use_module(library(clpfd)).

:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(pair_ext)).
:- use_module(library(string_ext)).



%! cli_help(+Name:atom, +Usages:list(atom), +Specs:list(dict)) is det.

cli_help(Name, Usages, Specs) :-
  usages_message(Name, Usages, String1),
  flags_message(Specs, String2),
  format(user_output, "Usage:\n~s\nOptions:\n~s", [String1,String2]).


%! flags_message(+Specs:list(dict), -Message:string) is det.

flags_message(Specs, Message) :-
  maplist(pp_short_flags, Specs, ShortStrings),
  max_string_length(ShortStrings, ShortWidth),
  maplist(pp_long_flags, Specs, LongStringss),
  flatten(LongStringss, LongStrings0),
  max_string_length(LongStrings0, LongWidth),
  maplist(
    {ShortWidth,LongWidth}/
      [ShortString0,LongStrings0,Dict0,Line0]>>
      format_option(
        ShortWidth-ShortString0,
        LongWidth-LongStrings0,
        Dict0,
        Line0
      ),
    ShortStrings,
    LongStringss,
    Specs,
    Lines
  ),
  string_list_concat(Lines, Message).

pp_long_flag(Long, String) :-
  format(string(String), "--~a", [Long]).

pp_long_flags(Spec, Strings) :-
  dict_get(longflags, Spec, [], Longs),
  maplist(pp_long_flag, Longs, Strings).

pp_short_flag(Short, String) :-
  format(string(String), "-~a", [Short]).

pp_short_flags(Spec, String) :-
  dict_get(shortflags, Spec, [], Shorts),
  maplist(pp_short_flag, Shorts, Strings),
  string_list_concat(Strings, ',',  String).


%! format_option(+ShortFlags:pair(nonneg,string),
%!               +LongFlags:pair(nonneg,list(string)),
%!               +OptionSpec:dict,
%!               -Line:string) is det.

format_option(ShortWidth1-ShortString, LongWidth1-LongStrings1, Dict, Line) :-
  optionSpec{help: Message} :< Dict,
  words_lines(LongStrings1, LongWidth1, ", ", LongStrings2),
  % Make room for a comma and a space.
  LongWidth2 #= LongWidth1 + 2,
  ShortWidth2 #= ShortWidth1 + 2,
  string_list_concat(LongStrings2, ",\n", LongsString),
  Indent #= ShortWidth2 + LongWidth2 + 4,
  format_lines(Message, Indent, Lines),
  format(
    string(Line),
    "~s~t~*+~s~t~*+~s\n",
    [LongsString,LongWidth2,ShortString,ShortWidth2,Lines]
  ).


%! format_lines(+Message1:or([string,list(string)]),
%!              +Indent:nonneg,
%!              -Message2:string) is det.

% Line splitting determined algorithmically.
format_lines(Message1, Indent, Message2) :-
  string(Message1), !,
  MinWidth = 40,
  LineWidth = 80,
  MaxWidth #= max(MinWidth, LineWidth - Indent),
  insert_line_breaks(Message1, MaxWidth, Indent, Message2).
% Line splitting determined by the option specification.
format_lines(Message, Indent, Message) :-
  indent_lines(Message, Indent, Message).


%! insert_line_breaks(+Message:string,
%!                    +LineLength:positive_integer,
%!                    +Indent:nonneg,
%!                    -TextLines:list(string)) is det.

insert_line_breaks(Message, LineLength, Indent, TextLines) :-
  message_lines(Message, LineLength, Lines),
  indent_lines(Lines, Indent, TextLines).


%! indent_lines(+Lines:list(string), +Indent:nonneg, -Message:string) is det.

indent_lines(Lines, Indent, Message) :-
  format(string(Sep), "~n~*|", [Indent]),
  string_list_concat(Lines, Sep, Message).


%! usages_message(+Name:atom,
%!                +Usages:list(list(atom)),
%!                -Message:string) is det.

usages_message(Name, Usages, Msg) :-
  maplist(usage_line(Name), Usages, Lines),
  string_list_concat(Lines, Msg).

usage_line(Name, Usage, Line) :-
  string_phrase(usage_line(Name, Usage), Line).

usage_line(Name, PosArgs) -->
  "  ",
  atom(Name),
  pos_args(PosArgs),
  " [options]\n".

pos_args([]) --> !, "".
pos_args([H|T]) -->
  " {",
  atom(H),
  "}",
  pos_args(T).
