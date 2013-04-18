:- module(
  atom_ext,
  [
    atom_to_term/2, % +Atom:atom
                    % -Term:term
    atom_or_number_number/2, % ?AtomOrNumber:oneof([atom,number])
                             % ?Number:number
    atom_replace/3, % +Atom:atom
                    % +Replacements:list(char-char)
                    % -NewAtom:atom
    codes_replace/4, % +Atom:atom
                     % +From:atom
                     % +To:atom
                     % -NewAtom:atom
    decapitalize/2, % +Atom:atom
                    % -Decapitalized:atom
    escape_underscores/2, % +Atom:atom
                          % -NewAtom:atom
    first_char/2, % +Atom:atom
                  % ?Char:char
    format_integer/3, % +Integer:integer
                      % +Length:integer
                      % -Atom:atom
    last_char/2, % +Atom:atom
                 % ?Char:char
    progress_bar/3, % +Current:number
                    % +End:number
                    % -ProgressBar:atom
    punctuate/2, % +Atom:atom
                 % -Punctuated:atom
    repeating_atom/3, % +SubAtom:atom
                      % +Repeats:number
                      % -Atom:atom
    slashes_to_underscores/2, % +Atom:atom
                              % -UnderscoreAtom:atom
    spaces_to_underscores/2, % +Atom:atom
                             % -UnderscoreAtom:atom
    split_atom_exclusive/3, % +Atom:atom
                            % +Split:atom
                            % -Splits:list(atom)
    split_atom_inclusive/3, % +Atom:atom
                            % +Split:atom
                            % -Splits:list(atom)
    split_codes/3, % +Codes:list(integer)
                   % +Split:list(integer)
                   % -Results:list(list(integer))
    strip/3, % +Strips:list(char)
             % +Unstripped:atom
             % -Stripped:atom
    strip_begin/3, % +Strips:list(char)
                   % +Unstripped:atom
                   % -Stripped:atom
    strip_end/3, % +Strips:list(char)
                 % +Unstripped:atom
                 % -Stripped:atom
    term_atom/2, % +Term:term
                 % -Atom:atom
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
@version 2011/08-2012/12, 2013/02-2013/04
*/

:- use_module(generics(list_ext)).
:- use_module(math(math_ext)).



%% atom_to_term(+Atom:atom, -Term:term) is det.
% Returns the term described by the atom.
%
% @param Atom An atom.
% @param Term A term.
% @see Dumbed down version of atom_to_term/3.

atom_to_term(Atom, Term):-
  atom_to_term(Atom, Term, _Bindings).

atom_or_number_number(Number, Number1):-
  nonvar(Number),
  number(Number),
  !,
  Number1 = Number.
atom_or_number_number(Atom, Number1):-
  atom_number(Atom, Number1).

%% atom_replace(
%%   +Atom:atom,
%%   +Replacement:list(char-char),
%%   -NewAtom:atom
%% ) is det.
% Returns a new atom that is like the given atom, but with the given
% character replacement.
%
% @param Atom An atom.
% @param Replacements A list of elements of the form =|char-char|=.
% @param NewAtom An atom.

atom_replace(Atom, Replacements, NewAtom):-
  atom_codes(Atom, Codes),
  maplist(char_code_, Replacements, CodesReplacements),
  list_replace(Codes, CodesReplacements, NewCodes),
  atom_codes(NewAtom, NewCodes).

codes_replace([], _From, _To, []):-
  !.
codes_replace(Codes, From, To, NewCodes):-
  append(From, Rest, Codes),
  !,
  append(To, Rest, Codes1),
  codes_replace(Codes1, From, To, NewCodes).
codes_replace([Code | Codes], From, To, [Code | NewCodes]):-
  codes_replace(Codes, From, To, NewCodes).

decapitalize(Atom, Decapitalized):-
  atom_chars(Atom, [Char | Chars]),
  char_type(Char, to_lower(Lower)),
  atom_chars(Decapitalized, [Lower | Chars]).

escape_underscores(Atom, NewAtom):-
  atom_replace(Atom, ['_'-['\\', '_']], NewAtom).

%% first_char(+Atom:atom, -Char:char) is det.
%% first_char(+Atom:atom, +Char:char) is semidet.
% The first character in an atom.

first_char(Atom, Char):-
  atom_chars(Atom, [Char | _Chars]).

%% format_integer(+Integer:integer, +Length:integer, -Atom:atom) is semidet.
% Returns a formatted representation of the given integer that is
% the given number of characters long.
%
% @param Integer An integer.
% @param Length An integer, indicating the character lenght of the
%        formatted integer atom.
% @param Atom An atom.

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

%% first_char(+Atom:atom, -Char:char) is det.
%% first_char(+Atom:atom, +Char:char) is semidet.
% The first character in an atom.

last_char(Atom, Char):-
  atom_chars(Atom, Chars),
  last(Chars, Char).

progress_bar(End, End, ProgressBar2):-
  !,
  progress_bar0(End, End, ProgressBar1),
  format(atom(ProgressBar2), '~w [done]', [ProgressBar1]).
progress_bar(Current, End, ProgressBar):-
  progress_bar0(Current, End, ProgressBar).

progress_bar0(Current, End, ProgressBar):-
  Percentage is round(Current / End * 100),
  format_integer(Percentage, 2, Percentage1),
  Progress is round(Current / (End / 10)),
  repeating_atom('=', Progress, Bar),
  Fill is 10 - Progress,
  repeating_atom('-', Fill, NonBar),
  format(
    atom(ProgressBar),
    '~w% ~w~w (~w/~w)',
    [Percentage1, Bar, NonBar, Current, End]
  ).

%% punctuate(+Atom:atom, -Punctuated:atom) is det.
% Ensures that the atom will have a dot character at the end.

punctuate(Atom, Atom):-
  last_char(Atom, '.'),
  !.
punctuate(Atom, Punctuated):-
  atomic_concat(Atom, '.', Punctuated).

%% repeating_atom(+SubAtom:atom, +Repeats:number, -Atom:atom) is det.
% Returns the atom that is the repetition of the given subatom the given
% number of times.
%
% @param SubAtom An atom
% @param Repeats A natural number.
% @param Atom An atom.

repeating_atom(_SubAtom, 0, ''):-
  !.
repeating_atom(SubAtom, 1, SubAtom):-
  !.
repeating_atom(SubAtom, Repeats, Atom):-
  Repeats > 1,
  NewRepeats is Repeats - 1,
  repeating_atom(SubAtom, NewRepeats, Atom1),
  atomic_concat(Atom1, SubAtom, Atom).

char_code_(Atom1-Atom2, Code1-Code2):-
  char_code(Atom1, Code1),
  char_code(Atom2, Code2).

slashes_to_underscores(Atom, NewAtom):-
  atom_replace(Atom, ['/'-'_'], NewAtom).

%% spaces_to_underscores(+Atom:atom, -UnderscoreAtom:atom) is det.
% Returns the atom that is like the give atom, but with all spaces replaced
% by underscores.
%
% @param Atom Any atom.
% @param UnderscoreAtom An atom without spaces.

spaces_to_underscores(Atom, NewAtom):-
  atom_replace(Atom, [' '-'_'], NewAtom).

%% split_atom_exclusive(
%%   +Atom:atom,
%%   +Split:list(atom),
%%   -Splits:list(atom)
%% ) is det.
% Returns the given atom split up in two, according to the given split.
% The first split does not include the split atom, making this method
% exclusive.
%
% @param Atom The original, unsplit atom.
% @param Split The occurrence atoms where the splittable atom will be split.
% @param Splits The results of splitting.
% @see split_atom_inclusive/3 includes the split atom in the split results.

split_atom_exclusive(Atom, Split, [Split1 | Splits]):-
  member(SplitMember, Split),
  sub_atom(Atom, Before, _Length, After, SplitMember),
  sub_atom(Atom, 0, Before, _After, Split1),
  atom_length(Atom, Total),
  Rest is Total - After,
  sub_atom(Atom, Rest, After, 0, NewAtom),
  !,
  split_atom_exclusive(NewAtom, Split, Splits).
split_atom_exclusive(Atom, _Split, [Atom]).

%% split_atom_inclusive(+Atom:atom, +Split:atom, -Splits:list(atom)) is det.
% Returns the given atom split up in two, according to the given split.
% Earlier splits includes the split atom, making this method inclusive.
%
% @param Atom The original, unsplit atom.
% @param Split The occurrence in atom where atom will be split.
% @param Splits The results of splitting.
% @see split_atom_exclusive/3 does not include the split atom in the split
%      results.

split_atom_inclusive(Atom, Split, Splits):-
  atom_length(Split, SplitLength),
  split_atom_inclusive(Atom, Split, SplitLength, Splits).

split_atom_inclusive(Atom, Split, SplitLength, [Split1 | Splits]):-
  sub_atom(Atom, Before, _Length, After, Split),
  Split1Length is SplitLength + Before,
  sub_atom(Atom, 0, Split1Length, _After, Split1),
  atom_length(Atom, Total),
  Rest is Total - After,
  sub_atom(Atom, Rest, After, 0, NewAtom),
  split_atom_inclusive(NewAtom, Split, SplitLength, Splits),
  !.
split_atom_inclusive(Atom, _Split, _SplitLength, [Atom]).

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

strip(Strips, Unstripped, Stripped):-
  atom_chars(Unstripped, UnstrippedChars1),
  strip_begin(Strips, UnstrippedChars1, UnstrippedChars2),
  strip_end(Strips, UnstrippedChars2, StrippedChars),
  atom_chars(Stripped, StrippedChars).

strip_begin(_Strips, [], []):-
  !.
strip_begin(Strips, [Strip | UnstrippedChars], StrippedChars):-
  member(Strip, Strips),
  !,
  strip_begin(Strips, UnstrippedChars, StrippedChars).
strip_begin(_Strips, Chars, Chars).

strip_end(Strips, UnstrippedChars, StrippedChars):-
  once(reverse(UnstrippedChars, ReverseUnstrippedChars)),
  strip_begin(Strips, ReverseUnstrippedChars, ReverseStrippedChars),
  once(reverse(StrippedChars, ReverseStrippedChars)).

%% term_atom(+Term:term, -Atom:atom) is det.
% Returns the atom that is closest to the given term.
% Number are turned into atoms.
%
% @param Term A term.
% @param Atom An atom.

term_atom(Atom, Atom):-
  atom(Atom),
  !.
term_atom(Term, Atom):-
  number(Term),
  !,
  atom_number(Atom, Term).
term_atom(Term, Atom):-
  term_to_atom(Term, Atom).

%% titlecase(+Atom, -TitlecaseAtom:atom) is det.
% Returns an atom that is like the given atom, except for the first character
% which must be either no letter or a capitalized letter.
%
% @param Atom Any atom.
% @param TitlecaseAtom A new atom that starts with a capital letter or with
%        no letter at all.

titlecase(Atom, TitlecaseAtom):-
  downcase_atom(Atom, Atom0),
  atom_chars(Atom0, [Char | Chars]),
  char_type(UpperChar, to_upper(Char)),
  atom_chars(TitlecaseAtom, [UpperChar | Chars]).

%% truncate(+Atom:atom, +MaxLen:integer, -Truncated:atom) is det.
% Returns the truncated version of the given atom.
% If =Atom='s length exceeds the given maximum length, then its truncated
% name will include '...' to indicate that is has been truncated.
%
% @param Atom An atom.
% @param MaxLen An integer.
% @param Truncated An atom.
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

%% underscores_to_spaces(+Atom, -SpacesAtom)
% Retruns an atom that is like the given atom, but with any underscore
% characters replaced by spaces.
%
% @param Atom Any atom.
% @param SpacesAtom An atom without underscores.

underscores_to_spaces(Atom, NewAtom):-
  atom_replace(Atom, ['_'-' '], NewAtom).

