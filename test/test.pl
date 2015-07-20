:- ensure_loaded(debug).

:- use_module(library(char_ext)).
:- use_module(library(closure)).
:- use_module(library(code_ext)).
%/dcg
  :- use_module(library(dcg/dcg_abnf)).
  :- use_module(library(dcg/dcg_abnf_common)).
  :- use_module(library(dcg/dcg_abnf_rules)).
  :- use_module(library(dcg/dcg_arrow)).
  :- use_module(library(dcg/dcg_ascii)).
  :- use_module(library(dcg/dcg_bracketed)).
  :- use_module(library(dcg/dcg_call)).
  :- use_module(library(dcg/dcg_cardinal)).
  :- use_module(library(dcg/dcg_char)).
  :- use_module(library(dcg/dcg_code)).
  :- use_module(library(dcg/dcg_content)).
  :- use_module(library(dcg/dcg_peek)).
  :- use_module(library(dcg/dcg_phrase)).
  :- use_module(library(dcg/dcg_quoted)).
  :- use_module(library(dcg/dcg_strip)).
  :- use_module(library(dcg/dcg_unicode)).
  :- use_module(library(dcg/dcg_word)).
:- use_module(library(deb_ext)).
:- use_module(library(default)).
:- use_module(library(file_ext)).
%/html
  :- use_module(library(html/html_dcg)).
  :- use_module(library(html/html_dom)).
  :- use_module(library(html/html_download)).
  :- use_module(library(html/html_table)).
%/http
  :- use_module(library(http/http_request)).
  :- use_module(library(http/http_server)).
:- use_module(library(list_ext)).
%/math
  :- use_module(library(math/math_ext)).
  :- use_module(library(math/positional)).
  :- use_module(library(math/radconv)).
  :- use_module(library(math/rational_ext)).
:- use_module(library(persistency_ext)).
%/svg
  :- use_module(library(svg/svg_dom)).
:- use_module(library(typecheck)).
:- use_module(library(typeconv)).
%/xml
  :- use_module(library(xml/xml_dom)).
