:- module(
  dcg_html,
  [
    dcg_html//1,       % +Content
    html_attribute//1, % +Attribute
    html_element//1,   % +Name
    html_element//2,   % +Name, +Attributes
    html_element//3,   % +Name, +Attributes, :Content_0
    html_entity//1,    % +Name
    html_graphic//1,   % +Code
    html_string//1,    % +String
    html_style//1      % +Pair
  ]
).

/** <module> HTML DCG

DCG grammar for generating HTML snippets.

@author Wouter Beek
@version 2015-2018
*/

:- use_module(library(dcg)).
:- use_module(library(dcg/dcg_abnf)).

:- meta_predicate
    html_element(+, +, //, ?, ?).





%! html_attribute(+Attribute:compound)// is det.

html_attribute(Attr) -->
  {
    compound_name_arguments(Attr, Name0, [Arg]),
    upcase_atom(Name0, Name)
  },
  " ",
  atom(Name),
  "=\"",
  html_string(Arg),
  "\"".



%! dcg_html(+Content:list(compound))// is det.

% Tag with no content.
dcg_html([tag(Name,Attrs)|T]) --> !,
  html_element(Name, Attrs),
  dcg_html(T).
% Tag with content.
dcg_html([tag(Name,Attrs,Contents)|T]) --> !,
  html_element(Name, Attrs, dcg_html(Contents)),
  dcg_html(T).
% Atom.
dcg_html([H|T]) -->
  {atom(H)}, !,
  atom(H),
  dcg_html(T).
% Codes list.
dcg_html([H|T]) -->
  html_string(H), !,
  dcg_html(T).
% Done.
dcg_html([]) --> !, "".



%! html_entity(+Name:atom)// is det.

html_entity(Name) -->
  "&",
  atom(Name),
  ";".



%! html_element(+Name:atom)// is det.
%! html_element(+Name:atom, ?Attributes:list(pair))// is det.

html_element(Name) -->
  html_element(Name, []).


html_element(Name, Attrs) -->
  "<",
  atom(Name),
  *(html_attribute, Attrs),
  "/>".


%! html_element(+Name:atom, ?Attributes:list(pair), :Content_0)// is det.

html_element(Name0, Attrs, Content_0) -->
  "<",
  {upcase_atom(Name0, Name)},
  atom(Name),
  *(html_attribute, Attrs),
  ">",
  Content_0,
  "</",
  atom(Name),
  ">".



%! html_graphic(+Code:code)// is det.
%
% HTML reserves the following ASCII characters:
%
%   - Ampersand
%   - Apostrophe
%   - Greater-than
%   - Less-than
%   - Quotation mark

html_graphic(0'&) --> !, "&amp;".
html_graphic(0'') --> !, "&#39".
html_graphic(0'") --> !, "&quot;". %"
html_graphic(0'<) --> !, "&lt;".
html_graphic(0'>) --> !, "&gt;".
html_graphic(C)   --> [C].



%! html_string(+String:atom)// is det.
%
% An **HTML string** is a sequence of printable or graphic HTML
% characters.  This includes spaces.

html_string(S) -->
  {atom_codes(S, Cs)},
  *(html_graphic, Cs).



%! html_style(+Pair:pair)// is det.

html_style(Name-Value) -->
  atom(Name),
  ":",
  (" ", ! ; ""),
  atom(Value),
  ";".
