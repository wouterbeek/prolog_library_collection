:- module(
  c,
  [
    c_convert/2 % +Atom:atom
                % -C_Atom:atom
  ]
).

/** <module> C

Support for the C programming language.

@author Wouter Beek
@version 2013/02
*/

:- use_module(pgc(parse_ext)).



/*
%%%%% This would be used instead of module ASCII.
% Replace the bell character with '\b'.
c_char(_O1, [92, 98 | R0]-R0, [7 | C0]-C0).
% Replace the line feed character with '\n'.
c_char(_O1, [92, 110 | R0]-R0, [10 | C0]-C0).
% Replace the horizontal tab character with '\t'.
c_char(_O1, [92, 116 | R0]-R0, [9 | C0]-C0).
% No replacement.
c_char(_O1, [X | R0]-R0, [X | C0]-C0).
*/

%% c_convert(+Atom:atom, -CAtom:atom) is det.
% Uses an internal DCG to convert atoms to C-strings, i.e. strings
% according to the specification for the C programming language.
%
% GraphViz also uses C-strings for the names of nodes in the DOT format.
%
% @param Atom An atom.
% @param CAtom An atom.
%
% @author Wouter Beek

c_convert(Atom, C_Atom):-
  atom_codes(Atom, Codes),
  % Read the codes that constitute the given atom. This may include codes for
  % escape characters that are not legitimate C-characters.
  %%%%% This would be used instead of module ASCII.
  %%%%re([case(sensitive), out(atom), q('*')], c:c_char, C_Atom, Codes-[]),
  re([case(sensitive), lang(c), out(atom), q('*')], any, C_Atom, Codes-[]),
  !.

/*
%%%%% This would be used instead of re/4 with option lang(c).
%% c_string(+Options:list(nvpair), +Results:diff_list)//
% Create a C-string. Normally =dot=  appears to be using UTF-8
% encoding. Would there be a safer way to transport non-ASCII
% characters, such as \uXXXX?
%
% @author Wouter Beek

c_string(O1, R1-R0, C1-C0):-
  c_char(O1, R1-R2, C1-C2),
  c_string(O1, R2-R0, C2-C0).
c_string(O1, R1-R0, C1-C0):-
  c_char(O1, R1-R0, C1-C0).
*/

