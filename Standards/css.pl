:- module(
  css,
  [
     attribute_value/3 % +Attribute:atom
                       % +Value:atom
                       % -Markup:atom
  ]
).

/** <module> CSS

Support for Cascading Style Sheets.

@author Wouter Beek
@version 2012/10, 2013/02
*/

:- use_module(pgc(parse_ext)).



% PARSER %

escape(C1-C0):-
  char(backslash, C1-C2),
  char(backslash, C2-C3),
  re([q(1/6)], hexadecimal_digit, C3-C4),
  re(
    [q('?')],
    [carriage_return, form_feed, horizontal_tab, line_feed, space],
    C4-C0
  ).

ident(C1-C0):-
  re([q('?')], dash, C1-C2),
  char(nmstart, C2-C3),
  re([q('*')], nmchar, C3-C0).

nmstart(C1-C0):-
  char(underscore, C1-C0).
nmstart(C1-C0):-
  char(letter_lowercase, C1-C0).
%nmstart(C1-C0):-
%  nonascii(C1-C0).
nmstart(C1-C0):-
  escape(C1-C0).




%% attribute_value(+Attribute:atom, +Value:atom, -Markup:atom) is det.

attribute_value(Attribute, Value, Markup):-
  format(atom(Markup), '~w:~w;', [Attribute, Value]).

color_keyword(aqua,    '#00ffff').
color_keyword(black,   '#000000').
color_keyword(blue,    '#0000ff').
color_keyword(fuchsia, '#ff00ff').
color_keyword(gray,    '#808080').
color_keyword(green,   '#008000').
color_keyword(lime,    '#00ff00').
color_keyword(maroon,  '#800000').
color_keyword(navy,    '#000080').
color_keyword(olive,   '#808000').
color_keyword(orange,  '#ffA500').
color_keyword(purple,  '#800080').
color_keyword(red,     '#ff0000').
color_keyword(silver,  '#c0c0c0').
color_keyword(teal,    '#008080').
color_keyword(yellow,  '#ffff00').
color_keyword(white,   '#ffffff').

system_color('ActiveBorder', 'Active window border.').
system_color('ActiveCaption', 'Active window caption.').
system_color('AppWorkspace', 'Background color of multiple document interface.').
system_color('Background', 'Desktop background.').
system_color('ButtonFace', 'Face color for three-dimensional display elements.').
system_color('ButtonHighlight', 'Highlight color for three-dimensional display elements (for edges facing away from the light source).').
system_color('ButtonShadow', 'Shadow color for three-dimensional display elements.').
system_color('ButtonText', 'Text on push buttons.').
system_color('CaptionText', 'Text in caption, size box, and scrollbar arrow box.').
system_color('GrayText', 'Grayed (disabled) text. This color is set to #000 if the current display driver does not support a solid gray color.').
system_color('Highlight', 'Item(s) selected in a control.').
system_color('HighlightText', 'Text of item(s) selected in a control.').
system_color('InactiveBorder', 'Inactive window border.').
system_color('InactiveCaption', 'Inactive window caption.').
system_color('InactiveCaptionText', 'Color of text in an inactive caption.').
system_color('InfoBackground', 'Background color for tooltip controls.').
system_color('InfoText', 'Text color for tooltip controls.').
system_color('Menu', 'Menu background.').
system_color('MenuText', 'Text in menus.').
system_color('Scrollbar', 'Scroll bar gray area.').
system_color('ThreeDDarkShadow', 'Dark shadow for three-dimensional display elements.').
system_color('ThreeDFace', 'Face color for three-dimensional display elements.').
system_color('ThreeDHighlight', 'Highlight color for three-dimensional display elements.').
system_color('ThreeDLightShadow', 'Light color for three-dimensional display elements (for edges facing the light source).').
system_color('ThreeDShadow', 'Dark shadow for three-dimensional display elements.').
system_color('Window', 'Window background.').
system_color('WindowFrame', 'Window frame.').
system_color('WindowText', 'Text in windows.').

