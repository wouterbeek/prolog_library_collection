:- module(
  cowspeak,
  [
    cowsay/2, % +Text:atom
              % -Cow:atom
    cowsay_web/2, % +Text:atom
                  % -Markup:list
    speech/1 % +Text:atom
  ]
).

/** <module> Cowspeak

A funncy cow for communicating with users.

@author Wouter Beek
@version 2012/09-2012/10
*/

:- use_module(generics(atom_ext)).
:- use_module(library(debug)).
:- use_module(library(process)).

:- debug(cowspeak).



cowsay(Text):-
  cowsay(Text, Cow),
  format(user_output, '~w', [Cow]),
  speech(Text).

%% cowsay(+Text:oneof([atom,list(atom)]), -Cow:atom) is det.
% Turns the given text into a cowified message, displaying the given
% text in the cow's speech bubble.
%
% Meet the cow:
% ==
%    ^__^
%    (oo)|_____
%    (__)|     )/|/
%      ||----w |
%     ||       ||
% ==
%
% @param Text Either an atomic text message or a list of atomic lines
%        constituting a message.
% @param Cow An atomic representing of both the cow and the text message.
% @tbd Split lines by words (in whitespace). Add this to module ATOM_EXT.

cowsay(Atom, Cow):-
  atomic(Atom),
  !,
  cowsay([Atom], Cow).
cowsay(Atoms, Cow):-
  % Split the given atoms to fit nicely into the speech bubble.
  max_line(MaxLength),
  findall(
    Line,
    (
      member(Atom, Atoms),
      split_atom_exclusive(Atom, '\n', Lines1),
      member(Line1, Lines1),
      split_atom_length(Line1, MaxLength, Lines2),
      member(Line, Lines2)
    ),
    Lines
  ),

  % Establish the width of the speech bubble.
  maplist(atom_length, Lines, Lengths),
  max_list(Lengths, LongestLength),

  % Draw the speech bubble.
  repeating_atom('-', LongestLength, Dashes),
  findall(
    NewLine,
    (
      member(Line, Lines),
      atom_length(Line, Length),
      NumberOfSpaces is MaxLength - Length,
      repeating_atom(' ', NumberOfSpaces, Spaces),
      atomic_list_concat(['| ', Line, Spaces, ' |'], NewLine)
    ),
    NewLines
  ),
  atomic_list_concat(NewLines, '\n', NewText),
  format(atom(Bubble), '/-~w-\\\n~w\n\\-~w-/\n', [Dashes, NewText, Dashes]),

  % Draw the cow!
  atomic_list_concat(
    [
      Bubble,
      '  |\n',
      '  |\n',
      '  |\n',
      '  |  ^__^\n',
      '   - (oo)______\n',
      '     (__)      )/|/\n',
      '       ||----w||\n',
      '      ||       ||'
    ],
    Cow
  ).

%% cowsay_web(+Text:atom, -Markup:list) is det.

cowsay_web(
  Text,
  [element(title, [], ['Cow says'])]/[element(pre, [], [CowText])]
):-
  speech(Text),
  cowsay(Text, CowText).

max_line(76).

%% speech(+Text:atom) is det.
% Turns the given text into speech and plays this speech shound.
%
% @param Text An atomic text message.
% @tbd Add speech for Windows, e.g. using Mary TTS.

speech(Text1):-
  is_list(Text1),
  !,
  atomic_list_concat(Text1, Text2),
  speech(Text2).
speech(Text):-
  atomic(Text),
  process_create(path(espeak), [Text], []),
  !.
speech(_Text):-
  debug(
    cowspeak,
    'The cow\'s speech cannot be played on the current OS.',
    []
  ).

