:- module(
  cli_help,
  [
    cli_help/3 % +Usages, +Specs, +Options
  ]
).

/** <module> Command Line Interface: help messages

*/

:- use_module(library(apply)).
:- use_module(library(clpfd)).

:- use_module(library(dict)).
:- use_module(library(pair_ext)).
:- use_module(library(string_ext)).



%! cli_help(+Usages:list(string), +Specs:dict, +Options:options) is det.

cli_help(Usages, Specs, Options) :-
  usage_lines(Usages, String1),
  opt_help(Specs, String2, Options),
  format(user_output, "Usage:\n~s\nOptions:\n~s", [String1,String2]).


%! opt_help(+Specs:dict, -Message:string, +Options:options) is det.

opt_help(Dict, Message, Options) :-
  dict_pairs(Dict, Pairs),
  pairs_values(Pairs, Dicts),
  maplist(pp_short_flags, Dicts, ShortStrings),
  max_string_length(ShortStrings, ShortWidth),
  maplist(pp_long_flags, Dicts, LongStringss),
  flatten(LongStringss, LongStrings0),
  max_string_length(LongStrings0, LongWidth),
  maplist(
    {ShortWidth,LongWidth,Options}/
      [ShortString,LongStrings,Dict,Line]>>
      format_option(
        ShortWidth-ShortString,
        LongWidth-LongStrings,
        Dict,
        Line,
        Options
      ),
    ShortStrings,
    LongStringss,
    Dicts,
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


%! format_options(+ShortFlags:pair(nonneg,string),
%!                +LongFlags:pair(nonneg,list(string)),
%!                +OptionSpec:dict,
%!                -Line:string,
%!                +Options:options) is det.

format_option(ShortWidth-ShortString, LongWidth1-LongStrings1, Dict, Line, Options) :-
  optionSpec{help: Message} :< Dict,
  group_length(LongWidth1, LongStrings1, LongStrings2),
  % Make room for a comma and a space.
  LongWidth2 #= LongWidth1 + 2,
  separator_(Separator, Options),
  maplist(
    {Separator}/[Strings,String]>>string_list_concat(Strings, Separator, String),
    LongStrings2,
    LongStrings3
  ),
  string_list_concat(LongStrings3, ",\n", LongsString),
  Indent #= ShortWidth + LongWidth2 + 4,
  format_lines(Message, Indent, Lines, Options),
  format(
    string(Line),
    "~w~t~*+~w~t~*+~w~n",
    [LongsString,LongWidth2,ShortString,ShortWidth,Lines]
  ).

% Explicitly asked to not split long flags.
separator_(", ", Options) :-
  dict_get(break_long_flags, Options, false), !.
% By default, split long flags.
separator_(",\n", _).


%! format_lines(+Message1:or([string,list(string)]),
%!              +Indent:nonneg,
%!              -Message2:string,
%!              +Options:options) is det.

% Line splitting determined algorithmically.
format_lines(Message1, Indent, Message2, Options) :-
  string(Message1), !,
  dict_get(min_help_width, Options, 40, MinWidth),
  dict_get(line_width, Options, 80, LineWidth),
  MaxWidth #= max(MinWidth, LineWidth - Indent),
  insert_line_breaks(Message1, MaxWidth, Indent, Message2).
% Line splitting determined by the option specification.
format_lines(Message, Indent, Message, _) :-
  indent_lines(Message, Indent, Message).


%! insert_line_breaks(+Message:string,
%!                    +LineLength:positive_integer,
%!                    +Indent:nonneg,
%!                    -TextLines:list(string)) is det.

insert_line_breaks(Message, LineLength, Indent, TextLines) :-
  string_list_concat(Words, ' ', Message),
  group_length(LineLength, Words, WordGroups),
  maplist(
    [Strings,String]>>string_list_concat(Strings, ' ', String),
    WordGroups,
    Lines
  ),
  indent_lines(Lines, Indent, TextLines).


%! indent_lines(+Lines:list(string), +Indent:nonneg, -Message:string) is det.

indent_lines(Lines, Indent, Message) :-
  format(string(Separator), "~n~*|", [Indent]),
  string_list_concat(Lines, Separator, Message).


%! group_length(+LineLength:positive_integer,
%!              +Words:list(string),
%!              -WordGroups:list(list(string))) is det.

group_length(LineLength, Words, Groups) :-
  group_length_(Words, LineLength, LineLength, [], [], Groups).

group_length_([], _, _, ThisLine, GroupsAcc, Groups) :-
  maplist(reverse, [ThisLine|GroupsAcc], GroupsAcc1),
  reverse(GroupsAcc1, Groups).
group_length_([Word|Words], LineLength, Remains1, ThisLine, Groups, GroupsAcc) :-
  atom_length(Word, WordLength),
  (  (Remains1 >= WordLength; ThisLine = [])
  -> % The word fits on the same line.
     Remains2 #= Remains1 - WordLength - 1,
     group_length_(Words, LineLength, Remains2, [Word|ThisLine], Groups, GroupsAcc)
  ;  % The word does not fit on the same line.
     group_length_([Word|Words], LineLength, LineLength, [], [ThisLine|Groups], GroupsAcc)
  ).



%! usage_lines(+Lines:list(string), -String:string) is det.

usage_lines(Lines1, String) :-
  maplist(usage_line_, Lines1, Lines2),
  string_list_concat(Lines2, String).

usage_line_(String1, String2) :-
  format(string(String2), "  ~s\n", [String1]).
