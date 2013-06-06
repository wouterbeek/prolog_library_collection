:- module(
  rdf_3066,
  [
    language_tag/4 % +Options:list(nvpair)
                   % -Primary:atom
                   % -Secondary:list(atom)
                   % +C:difflist
  ]
).

/** <module> RFC 3066

@author Wouter Beek
@version 2013/02
*/

:- use_module(generics(parse_ext)).
:- use_module(iso(iso_639_1)).
:- use_module(iso(iso_639_2)).



language_tag(O1, Primary, Secondary, C1-C0):-
  merge_options([case(lower), out(atom)], O1, O2),
  language_tag0(O2, Primary, C1-C2),
  language_tags(O2, Secondary, C2-C0).

language_tag0(O1, Tag, C1-C0):-
  parse_re(O1, [letter], Tag, C1-C0),
  atom_length(Tag, Length),
  (
    % Length-2 codes must be ISO 639-1.
    Length == 2
  ->
    iso_639_1(Tag, _URI1)
  ;
    % Length-3 codes must be ISO 639-2.
    Length == 3
  ->
    iso_639_2(Tag, _URI2)
  ;
    true
  ).

language_tags(_O1, [], C0-C0).
language_tags(O1, [Tag | Tags], C1-C0):-
  parse_char(hyphen, C1-C2),
  language_tag0(O1, Tag, C2-C3),
  language_tags(O1, Tags, C3-C0).

