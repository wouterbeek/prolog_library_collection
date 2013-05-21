:- module(
  atom_ext,
  [
    atom_to_term/2, % +Atom:atom
                    % -Term:term
    atom_replace/3, % +Atom:atom
                    % +Replacements:list(char-char)
                    % -NewAtom:atom
    decapitalize/2, % +Old:atom
                    % -New:atom
    escape_underscores/2, % +Old:atom
                          % -New:atom
    first_char/2, % +Atom:atom
                  % ?First:char
    format_integer/3, % +Integer:integer
                      % +Length:integer
                      % -Atom:atom
    last_char/2, % +Atom:atom
                 % ?Last:char
    progress_bar/3, % +Current:number
                    % +End:number
                    % -ProgressBar:atom
    punctuate/2, % +Old:atom
                 % -New:atom
    repeating_atom/3, % +SubAtom:atom
                      % +Repeats:integer
                      % -Atom:atom
    slashes_to_underscores/2, % +Atom:atom
                              % -UnderscoreAtom:atom
    spaces_to_underscores/2, % +Atom:atom
                             % -UnderscoreAtom:atom
    split_atom_exclusive/3, % +Split:oneof([atom,list(atom)])
                            % +Atom:atom
                            % -Splits:list(atom)
    split_atom_inclusive/3, % +Split:oneof([atom,list(atom)])
                            % +Atom:atom
                            % -Splits:list(atom)
    split_atom_length/3, % +Atom:atom
                         % +Length:integer
                         % -Splits:list(atom)
    split_codes/3, % +Codes:list(integer)
                   % +Split:list(integer)
                   % -Results:list(list(integer))
    split_length/3, % +Codes:list(integer)
                    % +Length:integer
                    % -Results:list(list(integer))
    strip/3, % +RemovableChars:oneof([char,list(char)])
             % +Unstripped:oneof([atom,list(char)])
             % -Stripped:oneof([atom,list(char)])
    strip_begin/3, % +RemovableChars:oneof([char,list(char)])
                   % +Unstripped:oneof([atom,list(char)])
                   % -Stripped:oneof([atom,list(char)])
    strip_end/3, % +RemovableChars:oneof([char,list(char)])
                 % +Unstripped:oneof([atom,list(char)])
                 % -Stripped:oneof([atom,list(char)])
    titlecase/2, % +Atom:atom
                 % -TitlecaseAtom:atom
    truncate/3, % +Atom:atom
                % +MaximumLength:integer
                % -Truncated:atom
    underscores_to_spaces/2 % +Atom:atom
                            % -SpacesAtom:atom
  ]
).

/** <module> Atom extensions for SWI-Prolog

Extra predicates that manipulate atoms for use in SWI-Prolog.

We assume atoms to be encoded using ASCII (or an ASCII-compatible) encoding
scheme.

@author Wouter Beek
@version 2011/08-2013/05
*/

:- use_module(generics(list_ext)).
:- use_module(math(math_ext)).



%! atom_to_term(+Atom:atom, -Term:term) is det.
% Returns the term described by the atom.
%
% @arg Atom An atom.
% @arg Term A term.
% @see Dumbed down version of atom_to_term/3.

atom_to_term(Atom, Term):-
  atom_to_term(Atom, Term, _Bindings).

%! atom_replace(
%!   +Atom:atom,
%!   +Replacement:list(char-char),
%!   -NewAtom:atom
%! ) is det.
% Returns a new atom that is like the given atom, but with the given
% character replacement.
%
% @arg Atom An atom.
% @arg Replacements A list of elements of the form =|char-char|=.
% @arg NewAtom An atom.

atom_replace(Atom, Replacements, NewAtom):-
  atom_codes(Atom, Codes),
  maplist(char_code_, Replacements, CodesReplacements),
  list_replace(Codes, CodesReplacements, NewCodes),
  atom_codes(NewAtom, NewCodes).

char_code_(Atom1-Atom2, Code1-Code2):-
  char_code(Atom1, Code1),
  char_code(Atom2, Code2).

codes_replace([], _From, _To, []):-
  !.
codes_replace(Codes, From, To, NewCodes):-
  append(From, Rest, Codes),
  !,
  append(To, Rest, Codes1),
  codes_replace(Codes1, From, To, NewCodes).
codes_replace([Code | Codes], From, To, [Code | NewCodes]):-
  codes_replace(Codes, From, To, NewCodes).

%! decapitalize(+Old:atom, -New:atom) is det.
% Ensure that the first letter of the atom is lower case.
% This can be used to concatenate natural language sentences.

decapitalize(Old, New):-
  atom_chars(Old, [Char | Chars]),
  char_type(Char, to_lower(Lower)),
  atom_chars(New, [Lower | Chars]).

%! escape_underscores(+Old:atom, -New:atom) is det.
% Use backslash-encoding for underscores.

escape_underscores(Atom, NewAtom):-
  atom_replace(Atom, ['_'-['\\', '_']], NewAtom).

%! first_char(+Atom:atom, ?First:char) is semidet.
% The first character in the given atom.

first_char(Atom, Char):-
  atom_chars(Atom, [Char | _Chars]).

%! format_integer(+Integer:integer, +Length:integer, -Atom:atom) is det.
% Returns a formatted representation of the given integer that is
% the given number of characters long.
%
% If the length of the formatted integer exceeds the given length, then
% the integer is simply converted to an atom.
%
% @arg Integer An integer, the formatted value.
% @arg Length An integer, indicating the character lenght of the
%        formatted integer atom.
% @arg Atom An atom, the formatted version of the integer value.

format_integer(Integer, Length, Atom):-
  atom_length(Integer, IntegerLength),
  format_integer0(Integer, IntegerLength, Length, Atom).

format_integer0(Integer, IntegerLength, Length, Atom):-
  Length < IntegerLength,
  !,
  atom_number(Atom, Integer).
format_integer0(Integer, IntegerLength, Length, Atom):-
  ZeroLength is Length - IntegerLength,
  repeating_atom('0', ZeroLength, Zeros),
  atomic_concat(Zeros, Integer, Atom).

%! last_char(+Atom:atom, ?Last:char) is semidet.
% The first character in the given atom.

last_char(Atom, Char):-
  atom_chars(Atom, Chars),
  last(Chars, Char).

%! progress_bar(+Current:integer, End:integer, ProgressBar:atom) is det.
% Returns an atomic progress bar that displays the current value onto
% the scale that runs from one to the given end value.
%
% @arg Current An integer, representing the current processed value.
% @arg End An integer, representing the last value to be processed.
% @arg ProgressBar The atomic representation of a progress bar.

progress_bar(End, End, ProgressBar2):-
  !,
  progress_bar0(End, End, ProgressBar1),
  format(atom(ProgressBar2), '~w [done]', [ProgressBar1]).
progress_bar(Current, End, ProgressBar):-
  progress_bar0(Current, End, ProgressBar).

progress_bar0(Current1, End, ProgressBar):-
  Percentage is round(Current1 / End * 100),
  format_integer(Percentage, 3, Percentage1),
  Progress is round(Current1 / (End / 10)),
  atom_number(EndAtom, End),
  atom_length(EndAtom, EndLength),
  format_integer(Current1, EndLength, Current2),
  repeating_atom('=', Progress, Bar),
  Fill is 10 - Progress,
  repeating_atom('-', Fill, NonBar),
  format(
    atom(ProgressBar),
    '~w% ~w~w (~w/~w)',
    [Percentage1, Bar, NonBar, Current2, End]
  ).

%! punctuate(+Old:atom, -New:atom) is det.
% Ensures that the atom will have a dot character at the end.

punctuate(Atom, Atom):-
  last_char(Atom, '.'),
  !.
punctuate(Old, New):-
  atomic_concat(Old, '.', New).

%! repeating_atom(+SubAtom:atom, +Repeats:integer, -Atom:atom) is det.
% Returns the atom that is the repetition of the given subatom the given
% number of times.
%
% @arg SubAtom An atom, the element that gets repeated.
% @arg Repeats A integer, the number of repeats of the subatom.
% @arg Atom An atom, the result of repeating the given atom.

repeating_atom(_SubAtom, 0, ''):-
  !.
repeating_atom(SubAtom, 1, SubAtom):-
  !.
repeating_atom(SubAtom, Repeats, Atom):-
  Repeats > 1,
  NewRepeats is Repeats - 1,
  repeating_atom(SubAtom, NewRepeats, Atom1),
  atomic_concat(Atom1, SubAtom, Atom).

slashes_to_underscores(Atom, NewAtom):-
  atom_replace(Atom, ['/'-'_'], NewAtom).

%! spaces_to_underscores(+Old:atom, -New:atom) is det.
% Returns the atom that is like the give atom, but with all spaces replaced
% by underscores.
%
% @arg Old Any atom.
% @arg New An atom with spaces replaced by underscores.

spaces_to_underscores(Atom, NewAtom):-
  atom_replace(Atom, [' '-'_'], NewAtom).

%! split_atom_exclusive(
%!   +Split:oneof([atom,list(atom)]),
%!   +Atom:atom,
%!   -Splits:list(atom)
%! ) is det.
% Returns the given atom split up in two, according to the given split.
% The first split does not include the split atom, making this method
% exclusive.
%
% @arg Split The occurrence atoms where the splittable atom will be split.
% @arg Atom The original, unsplit atom.
% @arg Splits The results of splitting.
%
% @see split_atom_inclusive/3 includes the split atom in the split results.

split_atom_exclusive(Split, Atom, Splits):-
  atom(Split),
  !,
  split_atom_exclusive([Split], Atom, Splits).
split_atom_exclusive(SplitList, Atom, [Split1 | Splits]):-
  member(SplitMember, SplitList),
  sub_atom(Atom, Before, _Length, After, SplitMember),
  sub_atom(Atom, 0, Before, _After, Split1),
  atom_length(Atom, Total),
  Rest is Total - After,
  sub_atom(Atom, Rest, After, 0, NewAtom),
  !,
  split_atom_exclusive(SplitList, NewAtom, Splits).
split_atom_exclusive(_SplitList, Atom, [Atom]).

%! split_atom_inclusive(
%!   +Split:oneof([atom,list(atom)]),
%!   +Atom:atom,
%!   -Splits:list(atom)
%! ) is det.
% Returns the given atom split up in two, according to the given split.
% Earlier splits includes the split atom, making this method inclusive.
%
% @arg Split The occurrence in atom where atom will be split.
% @arg Atom The original, unsplit atom.
% @arg Splits The results of splitting.
%
% @see split_atom_exclusive/3 does not include the split atom in the split
%      results.

split_atom_inclusive(Split, Atom, Splits):-
  atom(Split),
  !,
  split_atom_inclusive([Split], Atom, Splits).
split_atom_inclusive(SplitList, Atom, [Split1 | Splits]):-
  member(SplitMember, SplitList),
  atom_length(SplitMember, SplitLength),
  sub_atom(Atom, Before, _Length, After, SplitMember),
  Split1Length is SplitLength + Before,
  sub_atom(Atom, 0, Split1Length, _After, Split1),
  atom_length(Atom, Total),
  Rest is Total - After,
  sub_atom(Atom, Rest, After, 0, NewAtom),
  split_atom_inclusive(SplitList, NewAtom, Splits),
  !.
split_atom_inclusive(_SplitList, Atom, [Atom]).

split_atom_length(Atom, Length, Splits):-
  atom_codes(Atom, Codes),
  split_length(Codes, Length, CodeSplits),
  findall(
    Split,
    (
      member(CodeSplit, CodeSplits),
      atom_codes(Split, CodeSplit)
    ),
    Splits
  ).

split_codes(Codes, Split, Results):-
  \+ is_list(Split),
  !,
  split_codes(Codes, [Split], Results).
split_codes(Codes, Split, [Result | Results]):-
  append(Result, Temp, Codes),
  append(Split, NewCodes, Temp),
  !,
  split_codes(NewCodes, Split, Results).
split_codes(Result, _Split, [Result]).

%! split_length(
%!   +Codes:list(integer),
%!   +Length:integer,
%!   -Splits:list(list(integer))
%! ) is det.

split_length(Codes, Length, [Codes]):-
  length(Codes, CodesLength),
  CodesLength < Length,
  !.
split_length(Codes, Length, [SubCodes | Results]):-
  length(SubCodes, Length),
  append(SubCodes, Rest, Codes),
  split_length(Rest, Length, Results).

%! strip(+RemovableChar:char, +Unstripped:atom, -Stripped:atom) is det.
% Strips the given atom's front and back for the given character.

strip(RemovableChar, Unstripped, Stripped):-
  atom(RemovableChar),
  !,
  strip([RemovableChar], Unstripped, Stripped).
strip(RemovableChars, Unstripped, Stripped):-
  is_list(RemovableChars),
  atom(Unstripped),
  !,
  atom_chars(Unstripped, UnstrippedChars),
  strip(RemovableChars, UnstrippedChars, StrippedChars),
  atom_chars(Stripped, StrippedChars).
strip(RemovableChars, UnstrippedChars1, StrippedChars):-
  is_list(RemovableChars),
  is_list(UnstrippedChars1),
  !,
  strip_begin(RemovableChars, UnstrippedChars1, UnstrippedChars2),
  strip_end(RemovableChars, UnstrippedChars2, StrippedChars).

strip_begin(RemovableChar, Unstripped, Stripped):-
  atom(RemovableChar),
  !,
  strip_begin([RemovableChar], Unstripped, Stripped).
strip_begin(RemovableChars, Unstripped, Stripped):-
  is_list(RemovableChars),
  atom(Unstripped),
  !,
  atom_chars(Unstripped, UnstrippedChars),
  strip_begin(RemovableChars, UnstrippedChars, StrippedChars),
  atom_chars(Stripped, StrippedChars).
strip_begin(_RemovableChars, [], []):-
  !.
strip_begin(RemovableChars, [Strip | UnstrippedChars], StrippedChars):-
  member(Strip, RemovableChars),
  !,
  strip_begin(RemovableChars, UnstrippedChars, StrippedChars).
strip_begin(_Strips, Chars, Chars).

strip_end(RemovableChars, Unstripped, Stripped):-
  atom(Unstripped),
  !,
  atom_chars(Unstripped, UnstrippedChars),
  strip_end(RemovableChars, UnstrippedChars, StrippedChars),
  atom_chars(Stripped, StrippedChars).
strip_end(RemovableChars, UnstrippedChars, StrippedChars):-
  once(reverse(UnstrippedChars, ReverseUnstrippedChars)),
  strip_begin(RemovableChars, ReverseUnstrippedChars, ReverseStrippedChars),
  once(reverse(StrippedChars, ReverseStrippedChars)).

%! titlecase(+Old:atom, -New:atom) is det.
% Returns an atom that is like the given atom, except for the first character
% which must be either no letter or a capitalized letter.
%
% @arg Old Any atom.
% @arg New A new atom that starts with a capital letter or with
%        no letter at all.

titlecase(Atom, TitlecaseAtom):-
  downcase_atom(Atom, Atom0),
  atom_chars(Atom0, [Char | Chars]),
  char_type(UpperChar, to_upper(Char)),
  atom_chars(TitlecaseAtom, [UpperChar | Chars]).

%! truncate(+Atom:atom, +MaxLen:integer, -Truncated:atom) is det.
% Returns the truncated version of the given atom.
% If =Atom='s length exceeds the given maximum length, then its truncated
% name will include '...' to indicate that is has been truncated.
%
% @arg Atom An atom.
% @arg MaxLen An integer.
% @arg Truncated An atom.
%
% @author Jan Wielemaker, taken from Cliopatria.
% @author Wouter Beek, some alterations.

truncate(Atom, MaxLen, Truncated):-
  atom_length(Atom, AtomLen),
  truncate(Atom, AtomLen, MaxLen, Truncated).

truncate(Atom, AtomLen, MaxLen, Atom):-
  AtomLen =< MaxLen,
  !.
truncate(Atom, _AtomLen, MaxLen, Truncated):-
  TruncatedLen is max(3, MaxLen - 4),
  sub_atom(Atom, 0, TruncatedLen, _, Truncated1),
  atom_concat(Truncated1, ' ...', Truncated).

%! underscores_to_spaces(+Old:atom, -New:atom) is det.
% Retruns an atom that is like the given atom, but with any underscore
% characters replaced by spaces.
%
% @arg Old An atom.
% @arg New An atom with underscores replaced by spaces.

underscores_to_spaces(Old, New):-
  atom_replace(Old, ['_'-' '], New).

