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

:- use_module(generics(parse_ext)).



% Replace the bell character with '\b'.
c_char([92, 98 | R0]-R0, [7 | C0]-C0).
% Replace the line feed character with '\n'.
c_char([92, 110 | R0]-R0, [10 | C0]-C0).
% Replace the horizontal tab character with '\t'.
c_char([92, 116 | R0]-R0, [9 | C0]-C0).
% No replacement.
c_char([X | R0]-R0, [X | C0]-C0).

%! c_convert(+Atom:atom, -CAtom:atom) is det.
% Uses an internal DCG to convert atoms to C-strings, i.e. strings
% according to the specification for the C programming language.
%
% GraphViz also uses C-strings for the names of nodes in the DOT format.
%
% @arg Atom An atom.
% @arg CAtom An atom.

c_convert2(Atom, C_Atom):-
  atom_codes(Atom, Codes),
  % Read the codes that constitute the given atom. This may include codes for
  % escape characters that are not legitimate C-characters.
/* This would be used instead of module ASCII.
 * parse_re([case(sensitive), out(atom), q('*')], c:c_char, C_Atom, Codes-[]),
 */
  parse_re(
    [case(sensitive), lang(c), out(atom), q('*')],
    any,
    C_Atom,
    Codes-[]
  ),
  !.

c_convert(Atom, C_Atom):-
  atom_codes(Atom, Codes),
  c_string(C_Codes-[], Codes-[]),
  atom_codes(C_Atom, C_Codes).

%! c_string(+Options:list(nvpair), +Results:diff_list)
% Create a C-string. Normally =dot=  appears to be using UTF-8
% encoding. Would there be a safer way to transport non-ASCII
% characters, such as \uXXXX?

c_string(R1-R0, C1-C0):-
  c_char(R1-R2, C1-C2),
  c_string(R2-R0, C2-C0).
c_string(R1-R0, C1-C0):-
  c_char(R1-R0, C1-C0).

