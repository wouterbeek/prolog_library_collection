:- ensure_loaded(debug).

:- use_module(library(char_ext)).
%/dcg
    :- use_module(library(dcg/dcg_abnf)).
    :- use_module(library(dcg/dcg_abnf_common)).
    :- use_module(library(dcg/dcg_ascii)).
    :- use_module(library(dcg/dcg_bracketed)).
    :- use_module(library(dcg/dcg_call)).
    :- use_module(library(dcg/dcg_content)).
    :- use_module(library(dcg/dcg_phrase)).
    :- use_module(library(dcg/dcg_quoted)).
    :- use_module(library(dcg/dcg_unicode)).
    :- use_module(library(dcg/dcg_word)).
:- use_module(library(deb_ext)).
:- use_module(library(default)).
:- use_module(library(file_ext)).
%/http
    :- use_module(library(http/html_dcg)).
    :- use_module(library(http/html_download)).
    :- use_module(library(http/html_table)).
    :- use_module(library(http/http_request)).
    :- use_module(library(http/http_server)).
%/math
    :- use_module(library(math/math_ext)).
    :- use_module(library(math/positional)).
    :- use_module(library(math/radconv)).
    :- use_module(library(math/rational_ext)).
:- use_module(library(persistency_ext)).
:- use_module(library(typecheck)).
:- use_module(library(typeconv)).

