:- module(
  svg,
  [
% FETCHING
    file_to_svg/2, % +File:atom
                   % -SVG:dom
    stream_to_svg/2, % +Stream:stream
                     % -SVG:dom

% GENERATING
    svg_head/2, % +Size:size
                % -Head:list
    svg_head/3, % +Width:number
                % +Height:number
                % -Head:list

% PARSING
    parse_attributes_svg/3, % +Context:oneof([circle,line])
                            % +Attributes:list(nvpair)
                            % -ParsedAttributes:list(nvassignment)

% SVG COLOR SPACE
    svg_color/2, % ?Color:atom
                 % ?RGB:rgb
    svg_colors/1 % -Colors:list(atom)
  ]
).

/** <module> SVG

Predictaes that allow vector graphics to be drawn according to
the SVG standards.

---+ Prolog datatypes

---++ rgb

A compound term =rgb(Red, Green, Blue)=, where the three color parts are
represent by an integer between 0 and 255 (inclusive).

@author Wouter Beek
@version 2012/10, 2013/01-2013/04
*/

:- use_module(generics(db_ext)).
:- use_module(generics(parse_ext)).
:- use_module(generics(type_checking), [type_check/2 as type_check_generic]).
:- use_module(library(semweb/rdf_db)).
:- use_module(standards(markup)).

:- discontiguous(attribute0(_Name, _Type, _Contexts)).
:- discontiguous(attribute0(_Name, _Type, _Contexts, _Default)).

:- meta_predicate(type_check(2,+)).

% Assert DTD file locations.
:- assert_novel(user:file_search_path(dtd, svg(.))).

:- rdf_register_prefix(svg, 'http://www.w3.org/2000/svg').



% FETCHING %

file_to_svg(File, SVG):-
  open(File, read, Stream),
  stream_to_svg(Stream, SVG).

stream_to_svg(Stream, SVG):-
  load_structure(
    stream(Stream),
    SVG,
    [
      dialect(xmlns),
      max_errors(-1),
      shorttag(false),
      space(default),
      syntax_errors(quiet)
    ]
  ).



attribute(Name,  Type, Scopes):-
  attribute0(Name,  Type, Scopes).

attribute(Name,  Type, Scopes):-
  attribute0(Name,  Type, Scopes, _Default).

attribute0(cx, coordinate, [circle], '0').

attribute0(cy, coordinate, [circle], '0').

attribute0(fill, paint, [circle], black).

attribute0(r, length_svg, [circle]).

attribute0(stroke, paint, [circle, line], none).

attribute0('stroke-width', length_svg, [circle, line]).

attribute0(x1, coordinate, [line], '0').
attribute0(x2, coordinate, [line], '0').
attribute0(y1, coordinate, [line], '0').
attribute0(y2, coordinate, [line], '0').



% GENERICS %

%% svg_head(+Size:size, -Head:list) is det.
% Returns the markup for the SVG head for graphics with the given 2D size.
%
% @see svg_head/3

svg_head(size(2, [Width, Height]), Head):-
  svg_head(Width, Height, Head).

%% svg_head(+Width:integer, +Height:integer, -Head:list) is det.
% Returns the markup for the SVG head for graphics with the given
% height and width.

svg_head(Width, Height, [height=Height_cm, width=Width_cm]):-
  format_number(Width, cm, Width_cm),
  format_number(Height, cm, Height_cm).



% PARSING %

char0([X | C0]-C0):-
  atom_codes(X, [Y]),
  % [comma, round_bracket]
  \+ memberchk(Y, [40, 41, 44]).

% SVG color.
color_svg(C1-C0):-
  char(number_sign, C1-C2),
  hexdigit_mod_3(C2-C0).
color_svg(C1-C0):-
  atom(rgb, C1-C2),
  char(opening_round_bracket, C2-C3),
  re([q('?')], wsp, C3-C4),
  integer_svg(C4-C5),
  comma0(C5-C6),
  integer_svg(C6-C7),
  comma0(C7-C8),
  integer_svg(C8-C9),
  re([q('?')], wsp, C9-C10),
  char(closing_round_bracket, C10-C0).
color_svg(C1-C0):-
  atom(rgb, C1-C2),
  char(opening_round_bracket, C2-C3),
  re([q('?')], wsp, C3-C4),
  integer_svg(C4-C5),
  char(percent_sign, C5-C6),
  comma0(C6-C7),
  integer_svg(C7-C8),
  char(percent_sign, C8-C9),
  comma0(C9-C10),
  integer_svg(C10-C11),
  char(percent_sign, C11-C12),
  re([q('?')], wsp, C12-C13),
  char(closing_round_bracket, C13-C0).
color_svg(C1-C0):-
  color-keyword(C1-C0).

% SVG color names defined in the specification.
color-keyword(C1-C0):-
  svg_color(Color),
  atom_codes(Color, C2),
  append(C2, C0, C1).

% Comma, respecting whitespace.
comma0(C1-C0):-
  re([q('?')], wsp, C1-C2),
  char(comma, C2-C3),
  re([q('?')], wsp, C3-C0).

comma_wsp(C1-C0):-
  wsp(C1-C0).
comma_wsp(C1-C0):-
  wsp(C1-C0),
  char(comma, C1-C0),
  re([q('?')], wsp, C1-C0).
comma_wsp(C1-C0):-
  char(comma, C1-C0),
  re([q('?')], wsp, C1-C0).

comma_wsp_numberZ(C1-C0):-
  comma_wsp(C1-C2),
  number0(C2-C0).
comma_wsp_numberZ(C1-C0):-
  comma_wsp_numberZ(C1-C2),
  comma_wsp(C2-C3),
  number0(C3-C0).

% Coordiantes in SVG.
coordinate(C1-C0):-
  length_svg(C1-C0).

% Functional notation for an IRI.
funciri(C1-C0):-
  atom(url, C1-C2),
  char(opening_round_bracket, C2-C3),
  iri(C3-C4),
  char(closing_round_bracket, C4-C0).

hexdigit_mod_3(C1-C0):-
  re([q(3)], hexadecimal_digit, C1-C0).
hexdigit_mod_3(C1-C0):-
  re([q(3)], hexadecimal_digit, C1-C2),
  hexdigit_mod_3(C2-C0).

% ICC color specification. References a =|color-profile|= element, and one
% or more color component values.

icccolor(C1-C0):-
  atom(icc, C1-C2),
  char(hyphen, C2-C3),
  atom(color, C3-C4),
  char(opening_round_bracket, C4-C5),
  name_svg(C5-C6),
  comma_wsp_numberZ(C6-C7),
  char(closing_round_bracket, C7-C0).

% Signed integers in SVG.
integer_svg(C1-C0):-
  re([q('?')], sign, C1-C2),
  re([q('+')], decimal_digit, C2-C0).

% @tbd Check whether this is a reliable check for IRIs.
iri(C1-C0):-
  append(_, C0, C1).

length_svg(C1-C0):-
  number0(C1-C0).
length_svg(C1-C0):-
  number0(C1-C2),
  atom(cm, C2-C0).
length_svg(C1-C0):-
  number0(C1-C2),
  atom(em, C2-C0).
length_svg(C1-C0):-
  number0(C1-C2),
  atom(ex, C2-C0).
length_svg(C1-C0):-
  number0(C1-C2),
  atom(in, C2-C0).
length_svg(C1-C0):-
  number0(C1-C2),
  atom(mm, C2-C0).
length_svg(C1-C0):-
  number0(C1-C2),
  atom(pc, C2-C0).
length_svg(C1-C0):-
  number0(C1-C2),
  atom(pt, C2-C0).
length_svg(C1-C0):-
  number0(C1-C2),
  atom(px, C2-C0).
length_svg(C1-C0):-
  number0(C1-C2),
  char(percent_sign, C2-C0).

% SVG names are sequences of characters not containing ',', '(', ')'.
name_svg(C1-C0):-
  char0(C1-C0).
name_svg(C1-C0):-
  char0(C1-C2),
  name_svg(C2-C0).

% Real numbers in SVG.
number0(C1-C0):-
  integer_svg(C1-C2),
  re([q(1)], exponent, C2-C0).
number0(C1-C0):-
  re([q('?')], sign, C1-C2),
  re([q('?')], decimal_digit, C2-C3),
  char(dot, C3-C4),
  re([q('+')], decimal_digit, C4-C5),
  re([q('?')], exponent, C5-C0).

paint(C1-C0):-
  paint0(C1-C0).
paint(C1-C0):-
  funciri(C1-C2),
  re([q('?')], paint0, C2-C0).
paint(C1-C0):-
  atom(inherit, C1-C0).

paint0(C1-C0):-
  atom(none, C1-C0).
paint0(C1-C0):-
  atom(currentColor, C1-C0).
paint0(C1-C0):-
  color_svg(C1-C0).
paint0(C1-C0):-
  color_svg(C1-C2),
  icccolor(C2-C0).

% Whitespace in SVG:
wsp(C1-C0):-
  char(carriage_return, C1-C0).
wsp(C1-C0):-
  char(horizontal_tab, C1-C0).
wsp(C1-C0):-
  char(line_feed, C1-C0).
wsp(C1-C0):-
  char(space, C1-C0).

%% parse_attribute(
%%   +Context:oneof([circle,line]),
%%   +Attribute:nvpair,
%%   -ParsedAttribute:nvassignment
%% ) is semidet.
% Succeeds if the given attribute can be parsed within the given context.

parse_attribute(Context, Name=Value, Name=Value):-
  parse_attribute0(Context, Name, Value),
  !.
parse_attribute(Context, Attribute, Name=Value):-
  Attribute =.. [Name, Value],
  parse_attribute0(Context, Name, Value).

parse_attribute0(Context, Name, Value):-
  attribute(Name, Type, Contexts),
  memberchk(Context, Contexts),
  !,
  type_check(Type, Value).

parse_attributes_svg(Context, Attributes, ParsedAttributes):-
  maplist(parse_attribute(Context), Attributes, ParsedAttributes).

% DCG defined types
type_check(Type, Value):-
  atom_codes(Value, ValueCodes),
  call(Type, ValueCodes-[]),
  !.



% SVG COLOR SPACE %

% svg_color(?Color:name) is nondet.
% @see color/2

svg_color(Color):-
  svg_color(Color, _RGB).

%% svg_color(?Name:atom, ?RGB:rgb) is nondet.

svg_color(aliceblue, rgb(240, 248, 255)).
svg_color(antiquewhite, rgb(250, 235, 215)).
svg_color(aqua, rgb(0, 255, 255)).
svg_color(aquamarine, rgb(127, 255, 212)).
svg_color(azure, rgb(240, 255, 255)).
svg_color(beige, rgb(245, 245, 220)).
svg_color(bisque, rgb(255, 228, 196)).
svg_color(black, rgb(0, 0, 0)).
svg_color(blanchedalmond, rgb(255, 235, 205)).
svg_color(blue, rgb(0, 0, 255)).
svg_color(blueviolet, rgb(138, 43, 226)).
svg_color(brown, rgb(165, 42, 42)).
svg_color(burlywood, rgb(222, 184, 135)).
svg_color(cadetblue, rgb(95, 158, 160)).
svg_color(chartreuse, rgb(127, 255, 0)).
svg_color(chocolate, rgb(210, 105, 30)).
svg_color(coral, rgb(255, 127, 80)).
svg_color(cornflowerblue, rgb(100, 149, 237)).
svg_color(cornsilk, rgb(255, 248, 220)).
svg_color(crimson, rgb(220, 20, 60)).
svg_color(cyan, rgb(0, 255, 255)).
svg_color(darkblue, rgb(0, 0, 139)).
svg_color(darkcyan, rgb(0, 139, 139)).
svg_color(darkgoldenrod, rgb(184, 134, 11)).
svg_color(darkgray, rgb(169, 169, 169)).
svg_color(darkgreen, rgb(0, 100, 0)).
svg_color(darkgrey, rgb(169, 169, 169)).
svg_color(darkkhaki, rgb(189, 183, 107)).
svg_color(darkmagenta, rgb(139, 0, 139)).
svg_color(darkolivegreen, rgb(85, 107, 47)).
svg_color(darkorange, rgb(255, 140, 0)).
svg_color(darkorchid, rgb(153, 50, 204)).
svg_color(darkred, rgb(139, 0, 0)).
svg_color(darksalmon, rgb(233, 150, 122)).
svg_color(darkseagreen, rgb(143, 188, 143)).
svg_color(darkslateblue, rgb(72, 61, 139)).
svg_color(darkslategray, rgb(47, 79, 79)).
svg_color(darkslategrey, rgb(47, 79, 79)).
svg_color(darkturquoise, rgb(0, 206, 209)).
svg_color(darkviolet, rgb(148, 0, 211)).
svg_color(deeppink, rgb(255, 20, 147)).
svg_color(deepskyblue, rgb(0, 191, 255)).
svg_color(dimgray, rgb(105, 105, 105)).
svg_color(dimgrey, rgb(105, 105, 105)).
svg_color(dodgerblue, rgb(30, 144, 255)).
svg_color(firebrick, rgb(178, 34, 34)).
svg_color(floralwhite, rgb(255, 250, 240)).
svg_color(forestgreen, rgb(34, 139, 34)).
svg_color(fuchsia, rgb(255, 0, 255)).
svg_color(gainsboro, rgb(220, 220, 220)).
svg_color(ghostwhite, rgb(248, 248, 255)).
svg_color(gold, rgb(255, 215, 0)).
svg_color(goldenrod, rgb(218, 165, 32)).
svg_color(gray, rgb(128, 128, 128)).
svg_color(grey, rgb(128, 128, 128)).
svg_color(green, rgb(0, 128, 0)).
svg_color(greenyellow, rgb(173, 255, 47)).
svg_color(honeydew, rgb(240, 255, 240)).
svg_color(hotpink, rgb(255, 105, 180)).
svg_color(indianred, rgb(205, 92, 92)).
svg_color(indigo, rgb(75, 0, 130)).
svg_color(ivory, rgb(255, 255, 240)).
svg_color(khaki, rgb(240, 230, 140)).
svg_color(lavender, rgb(230, 230, 250)).
svg_color(lavenderblush, rgb(255, 240, 245)).
svg_color(lawngreen, rgb(124, 252, 0)).
svg_color(lemonchiffon, rgb(255, 250, 205)).
svg_color(lightblue, rgb(173, 216, 230)).
svg_color(lightcoral, rgb(240, 128, 128)).
svg_color(lightcyan, rgb(224, 255, 255)).
svg_color(lightgoldenrodyellow, rgb(250, 250, 210)).
svg_color(lightgray, rgb(211, 211, 211)).
svg_color(lightgreen, rgb(144, 238, 144)).
svg_color(lightgrey, rgb(211, 211, 211)).
svg_color(lightpink, rgb(255, 182, 193)).
svg_color(lightsalmon, rgb(255, 160, 122)).
svg_color(lightseagreen, rgb(32, 178, 170)).
svg_color(lightskyblue, rgb(135, 206, 250)).
svg_color(lightslategray, rgb(119, 136, 153)).
svg_color(lightslategrey, rgb(119, 136, 153)).
svg_color(lightsteelblue, rgb(176, 196, 222)).
svg_color(lightyellow, rgb(255, 255, 224)).
svg_color(lime, rgb(0, 255, 0)).
svg_color(limegreen, rgb(50, 205, 50)).
svg_color(linen, rgb(250, 240, 230)).
svg_color(magenta, rgb(255, 0, 255)).
svg_color(maroon, rgb(128, 0, 0)).
svg_color(mediumaquamarine, rgb(102, 205, 170)).
svg_color(mediumblue, rgb(0, 0, 205)).
svg_color(mediumorchid, rgb(186, 85, 211)).
svg_color(mediumpurple, rgb(147, 112, 219)).
svg_color(mediumseagreen, rgb(60, 179, 113)).
svg_color(mediumslateblue, rgb(123, 104, 238)).
svg_color(mediumspringgreen, rgb(0, 250, 154)).
svg_color(mediumturquoise, rgb(72, 209, 204)).
svg_color(mediumvioletred, rgb(199, 21, 133)).
svg_color(midnightblue, rgb(25, 25, 112)).
svg_color(mintcream, rgb(245, 255, 250)).
svg_color(mistyrose, rgb(255, 228, 225)).
svg_color(moccasin, rgb(255, 228, 181)).
svg_color(navajowhite, rgb(255, 222, 173)).
svg_color(navy, rgb(0, 0, 128)).
svg_color(oldlace, rgb(253, 245, 230)).
svg_color(olive, rgb(128, 128, 0)).
svg_color(olivedrab, rgb(107, 142, 35)).
svg_color(orange, rgb(255, 165, 0)).
svg_color(orangered, rgb(255, 69, 0)).
svg_color(orchid, rgb(218, 112, 214)).
svg_color(palegoldenrod, rgb(238, 232, 170)).
svg_color(palegreen, rgb(152, 251, 152)).
svg_color(paleturquoise, rgb(175, 238, 238)).
svg_color(palevioletred, rgb(219, 112, 147)).
svg_color(papayawhip, rgb(255, 239, 213)).
svg_color(peachpuff, rgb(255, 218, 185)).
svg_color(peru, rgb(205, 133, 63)).
svg_color(pink, rgb(255, 192, 203)).
svg_color(plum, rgb(221, 160, 221)).
svg_color(powderblue, rgb(176, 224, 230)).
svg_color(purple, rgb(128, 0, 128)).
svg_color(red, rgb(255, 0, 0)).
svg_color(rosybrown, rgb(188, 143, 143)).
svg_color(royalblue, rgb(65, 105, 225)).
svg_color(saddlebrown, rgb(139, 69, 19)).
svg_color(salmon, rgb(250, 128, 114)).
svg_color(sandybrown, rgb(244, 164, 96)).
svg_color(seagreen, rgb(46, 139, 87)).
svg_color(seashell, rgb(255, 245, 238)).
svg_color(sienna, rgb(160, 82, 45)).
svg_color(silver, rgb(192, 192, 192)).
svg_color(skyblue, rgb(135, 206, 235)).
svg_color(slateblue, rgb(106, 90, 205)).
svg_color(slategray, rgb(112, 128, 144)).
svg_color(slategrey, rgb(112, 128, 144)).
svg_color(snow, rgb(255, 250, 250)).
svg_color(springgreen, rgb(0, 255, 127)).
svg_color(steelblue, rgb(70, 130, 180)).
svg_color(tan, rgb(210, 180, 140)).
svg_color(teal, rgb(0, 128, 128)).
svg_color(thistle, rgb(216, 191, 216)).
svg_color(tomato, rgb(255, 99, 71)).
svg_color(turquoise, rgb(64, 224, 208)).
svg_color(violet, rgb(238, 130, 238)).
svg_color(wheat, rgb(245, 222, 179)).
svg_color(white, rgb(255, 255, 255)).
svg_color(whitesmoke, rgb(245, 245, 245)).
svg_color(yellow, rgb(255, 255, 0)).
svg_color(yellowgreen, rgb(154, 205, 50)).

%% svg_colors(-Colors:list(atom)) is det.
% Returns the list with supported color names.
%
% @param Colors A list with atomic color names.

svg_colors(Colors):-
  findall(
    Color,
    svg_color(Color),
    Colors
  ).

