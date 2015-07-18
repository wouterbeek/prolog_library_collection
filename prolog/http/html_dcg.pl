:- module(
  html_dcg,
  [
    html_attribute//1, % ?Attribute:pair(atom)
    html_dcg//1, % +Content:list(or([atom,compound,list(code)]))
    html_element//1, % +Name:atom
    html_element//2, % +Name:atom
                     % +Attributes:list(pair)
    html_element//3, % +Name:atom
                     % +Attributes:list(pair)
                     % :Content
    html_entity//1, % +Name:atom
    html_graphic//1, % ?Code:nonneg
    html_string//1, % ?String:atom
    html_style//1 % ?NVPair:pair(atom)
  ]
).

/** <module> HTML: DCG

DCG grammar for HTML snippets.

@author Wouter Beek
@version 2013/09, 2014/11
*/

:- use_module(library(dcg/basics)).

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_quoted)).
:- use_module(library(dcg/dcg_unicode),
  [alpha_numeric//1,punctuation//1,white//1]
).

:- meta_predicate(html_element(+,+,//,?,?)).



%! html_attribute(?Attribute:pair)// .
% Used for *checking* GraphViz HTML-like labels.

html_attribute(Name-Value) -->
  atom(Name),
  "=",
  quoted(html_string(Value)).



%! html_dcg(+Content)// .

% Done.
html_dcg([]) --> !, [].
% Tag with no content.
html_dcg([tag(Name,Attrs)|T]) --> !,
  html_element(Name, Attrs),
  html_dcg(T).
% Tag with content.
html_dcg([tag(Name,Attrs,Contents)|T]) --> !,
  html_element(Name, Attrs, html_dcg(Contents)),
  html_dcg(T).
% Atom.
html_dcg([H|T]) -->
  {atom(H)}, !,
  atom(H),
  html_dcg(T).
% Codes list.
html_dcg([H|T]) -->
  html_string(H), !,
  html_dcg(T).



%! html_entity(?Name:atom)// .

html_entity(Name) -->
  "&",
  atom(Name),
  ";".



%! html_element(?Name:atom)// .
%! html_element(?Name:atom, ?Attributes:list(nvpair))// .
%! html_element(?Name:atom, ?Attributes:list(nvpair), :Content)// .

html_element(Name) -->
  html_element(Name, []).

html_element(Name, Attrs) -->
  "<",
  atom(Name),
  " ",
  '*'(html_attribute, Attrs, [separator(space)]),
  "/>".

html_element(Name, Attrs, Content) -->
  "<",
  atom(Name),
  " ",
  '*'(html_attribute, Attrs, [separator(space)]),
  ">",
  phrase(Content),
  "</",
  atom(Name),
  ">".



%! html_graphic(?Code:nonneg)// .
% HTML reserves the following ASCII characters:
%   - Ampersand
%   - Apostrophe
%   - Greater-than
%   - Less-than
%   - Quotation mark

html_graphic(Code) --> white(Code).
html_graphic(Code) --> alpha_numeric(Code).
html_graphic(Code) --> html_punctuation(Code).


%! html_punctuation(?Code:nonneg)// .

% First come the translations for escaped punctuation characters.
html_punctuation(34) --> "&quot;". % Double quotes (").
html_punctuation(60) --> "&lt;".   % Smaller than (<).
html_punctuation(62) --> "&gt;".   % Greater than (>).
html_punctuation(68) --> "&amp;".  % Ampersand (&).
html_punctuation(Code) -->
  punctuation(Code),
  {\+ member(Code, [34,60,62,68])}.



%! html_string(?String:atom)// .
% An **HTML string** is a sequence of printable or graphic HTML characters.
% This includes spaces.

html_string(String) -->
  '*'(html_graphic, String, [convert1(codes_atom)]).



%! html_style(NVPair:pair(atom))// .

html_style(Name-Value) -->
  atom(Name),
  ":",
  '?'(space, []),
  atom(Value),
  ";".
