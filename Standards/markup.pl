:- module(
  markup,
  [
    file_to_uri/2, % +File
                   % -URI
    format_number/3, % +Number:float
                     % +Unit:unit
                     % -Atom:atom
    integer_sequence/2, % +Sequence:list(integer)
                        % -Markup:compound
    sonnet/2 % +Sentences:list(atom)
             % -Markup:compound
  ]
).

/** <module> Markup

@author Wouter Beek
@version 2012/10
*/

:- use_module(standards(css), [attribute_value/3 as css_attribute_value]).
:- use_module(standards(standards)).



%! file_to_uri(+File, -URI) is det.
% Returns the URI representation of the given file.

file_to_uri(File, URI):-
  format(atom(URI), 'file://~w', [File]).

%! format_number(+Number:float, +Unit:unit, -Atom:atom) is det.
% Formats a number according to a certain unit scale.
%
% @arg Number Any number (e.g., integer, float).
% @arg Unit An atomic unit descriptor. Units must be registered as unit/3.
% @arg Atom The atomic result of formatting.

format_number(Number, Unit, Atom):-
  format(atom(Atom), '~w~w', [Number, Unit]).

%! integer_sequence(+Sequence:list(integer), -Markup:compound) is det.

integer_sequence([], []).
integer_sequence([H | T], [element(span, [style=Style], [H1]) | Markup]):-
  FontSize is 100 + 10 * H,
  atom_number(H1, H),
  format_number(FontSize, '%', FontSize_pct),
  css_attribute_value('font-size', FontSize_pct, Style),
  integer_sequence(T, Markup).

%! sonnet(+Sonnet:list(atom), -Markup:compound) is det.

sonnet(Sonnet, element(figure, [], Ts)):-
  sonnet0(Sonnet, Ts).

sonnet0(
  [Sentence1, Sentence2],
  [element(p, [], [Sentence1, element(br, [], []), Sentence2])]
).
sonnet0(
  [Sentence1, Sentence2, Sentence3, Sentence4 | Sentences],
  [
    element(
      p,
      [],
      [
        Sentence1,
        element(br, [], []),
        Sentence2,
        element(br, [], []),
        Sentence3,
        element(br, [], []),
        Sentence4
      ]
    )
  |
    Markup
  ]
):-
  sonnet0(Sentences, Markup).

