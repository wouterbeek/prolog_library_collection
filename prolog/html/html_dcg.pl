:- module(
  html_dcg,
  [
    html_attr//1, % +Attribute:pair
    html_dcg//1, % +Content:list(compound)
    html_element//1, % +Name:atom
    html_element//2, % +Name:atom
                     % +Attributes:list(pair)
    html_element//3, % +Name:atom
                     % +Attributes:list(pair)
                     % :Content_0
    html_entity//1, % +Name:atom
    html_graphic//1, % +Code:code
    html_string//1, % +String:atom
    html_style//1, % +Pair:pair
    'MediaDesc'//1 % -MediaDescriptions:list(string)
  ]
).

/** <module> HTML DCG

DCG grammar for generating HTML snippets.

@author Wouter Beek
@compat HTML 4.01
@version 2015/07-2015/08, 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_word)).

:- meta_predicate(html_element(+,+,//,?,?)).





%! html_attr(+Attribute:pair)// is det.

html_attr(Name-Value) --> " ", atom(Name), "=\"", html_string(Value), "\"".



%! html_dcg(+Content:list(compound))// is det.

% Tag with no content.
html_dcg([tag(Name,Attrs)|T]) --> !,
  html_element(Name, Attrs), html_dcg(T).
% Tag with content.
html_dcg([tag(Name,Attrs,Contents)|T]) --> !,
  html_element(Name, Attrs, html_dcg(Contents)), html_dcg(T).
% Atom.
html_dcg([H|T]) --> {atom(H)}, !, atom(H), html_dcg(T).
% Codes list.
html_dcg([H|T]) --> html_string(H), !, html_dcg(T).
% Done.
html_dcg([]) --> !, "".



%! html_entity(+Name:atom)// is det..

html_entity(Name) --> "&", atom(Name), ";".



%! html_element(+Name:atom)// is det.

html_element(Name) --> html_element(Name, []).


%! html_element(+Name:atom, ?Attributes:list(pair))// is det.

html_element(Name, Attrs) --> "<", atom(Name), *(html_attr, Attrs), "/>".


%! html_element(+Name:atom, ?Attributes:list(pair), :Content_0)// is det.

html_element(Name, Attrs, Content_0) -->
  "<", atom(Name), *(html_attr, Attrs), ">",
  Content_0,
  "</", atom(Name), ">".



%! html_graphic(+Code:code)// is det.
% HTML reserves the following ASCII characters:
%   - Ampersand
%   - Apostrophe
%   - Greater-than
%   - Less-than
%   - Quotation mark

html_graphic(0'&) --> !, "&amp;".
html_graphic(0'') --> !, "&#39".
html_graphic(0'") --> !, "&quot;".   %"
html_graphic(0'<) --> !, "&lt;".
html_graphic(0'>) --> !, "&gt;".
html_graphic(C)   --> [C].



%! html_string(+String:atom)// is det.
% An **HTML string** is a sequence of printable or graphic HTML characters.
% This includes spaces.

html_string(S) --> {atom_codes(S, Cs)}, *(html_graphic, Cs).



%! html_style(+Pair:pair)// is det.

html_style(Name-Value) --> atom(Name), ":", (" ", ! ; ""), atom(Value), ";".



%! 'MediaDesc'(-MediaDescriptions:list(string))// is det.
% The following media descriptions are supported by HTML 4.01:
%   * all
%     Suitable for all devices.
%   * aural
%     Intended for speech synthesizers.
%   * braille
%     Intended for braille tactile feedback devices.
%   * handheld
%     Intended for handheld devices (small screen, monochrome,
%     bitmapped graphics, limited bandwidth).
%   * print
%     Intended for paged, opaque material and for documents viewed on
%     screen in print preview mode.
%   * projection
%     Intended for projectors.
%   * screen
%     Intended for non-paged computer screens.
%   * tty
%     Intended for media using a fixed-pitch character grid, such as
%     teletypes, terminals, or portable devices with limited display
%     capabilities.
%   * tv
%     Intended for television-type devices (low resolution, color,
%     limited scrollability).
%
% To facilitate the introduction of media descriptions in the future,
% conforming UAs must parse as follows:
%   1. The value is a comma-separated list of entries [stripping spaces?].
%   2. Each entry is truncated just before the first character that is not
%      ALPHA, DIGIT or hyphen.
%   3. Case-sensitive match with the above enumerated media descriptions.
%      UAs may ignore entries that do not match.

'MediaDesc'(L) -->
  *(media_desc_sep),
  ...(Cs),
  media_desc_post, !,
  {
    string_codes(H, Cs),
    (known_media_desc(H) -> L = [H|T] ; L = T)
  },
  'MediaDesc'(T).
'MediaDesc'([]) --> "".

media_desc_sep --> " ".
media_desc_sep --> ",".

media_desc_post --> ",", !.
media_desc_post --> " ", ..., ",".

known_media_desc("all").
known_media_desc("aural").
known_media_desc("braille").
known_media_desc("handheld").
known_media_desc("print").
known_media_desc("projection").
known_media_desc("screen").
known_media_desc("tty").
known_media_desc("tv").
