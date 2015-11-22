:- module(
  html_dcg,
  [
    html_attr//1, % ?Attribute:pair(atom)
    html_dcg//1, % +Content:list(or([atom,compound,list(code)]))
    html_element//1, % +Name:atom
    html_element//2, % +Name:atom
                     % +Attributes:list(pair)
    html_element//3, % +Name:atom
                     % +Attributes:list(pair)
                     % :Content
    html_entity//1, % +Name:atom
    html_graphic//1, % ?Code:code
    html_string//1, % ?String:atom
    html_style//1 % ?NVPair:pair(atom)
  ]
).

/** <module> HTML DCG

DCG grammar for HTML snippets.

@author Wouter Beek
@version 2015/07-2015/08
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_re)).
:- use_module(library(dcg/dcg_word)).

:- meta_predicate(html_element(+,+,//,?,?)).



%! html_attr(?Attribute:pair)// .
% Used for *checking* GraphViz HTML-like labels.

html_attr(Name-Value) --> atom(Name), "=\"", html_string(Value), "\"".

html_attrs([H|T]) --> html_attr(H), !, " ", html_attrs(T).
html_attrs([]) --> "".



%! html_dcg(+Content)// .

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
% Done.
html_dcg([]) --> !, "".



%! html_entity(?Name:atom)// .

html_entity(Name) --> "&", atom(Name), ";".



%! html_element(?Name:atom)// .
%! html_element(?Name:atom, ?Attributes:list(nvpair))// .
%! html_element(?Name:atom, ?Attributes:list(nvpair), :Content)// .

html_element(Name) --> html_element(Name, []).

html_element(Name, Attrs) --> "<", atom(Name), " ", html_attrs(Attrs), "/>".

html_element(Name, Attrs, Content) -->
  "<",
  atom(Name),
  " ",
  html_attrs(Attrs),
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

html_graphic(C) --> u_white(C).
html_graphic(C) --> alpha_numeric(C).
html_graphic(C) --> html_punctuation(C).


%! html_punctuation(?Code:nonneg)// .

% First come the translations for escaped punctuation characters.
html_punctuation(0'") --> "&quot;". % Double quotes (").
html_punctuation(0'<) --> "&lt;".   % Smaller than (<).
html_punctuation(0'>) --> "&gt;".   % Greater than (>).
html_punctuation(0'&) --> "&amp;".  % Ampersand (&).
html_punctuation(C)   --> punctuation(C), {\+ member(C, [34,60,62,68])}.



%! html_string(?String:atom)// .
% An **HTML string** is a sequence of printable or graphic HTML characters.
% This includes spaces.

html_string(S) --> dcg_atom(html_string_codes, S).
html_string_codes([H|T]) --> html_graphic(H), !, html_string_codes(T).
html_string_codes([]) --> "".



%! html_style(NVPair:pair(atom))// .

html_style(Name-Value) --> atom(Name), ":", (" ", ! ; ""), atom(Value), ";".
