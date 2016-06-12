:- use_module(library(assoc_ext)).
:- use_module(library(atom_ext)).
:- use_module(library(batch)).
:- use_module(library(char_ext)).
:- use_module(library(chr_ext)).
:- use_module(library(cli_ext)).
:- use_module(library(closure)).
:- use_module(library(code_ext)).
:- use_module(library(counter)).
:- use_module(library(csv_ext)).
%/date_time
  :- use_module(library(date_time/date_time)).
  :- use_module(library(date_time/rfc3339)).
:- use_module(library(db_ext)).
%/dcg
  :- use_module(library(dcg/bibtex)).
  :- use_module(library(dcg/dcg_ascii)).
  :- use_module(library(dcg/dcg_atom)).
  :- use_module(library(dcg/dcg_char)).
  :- use_module(library(dcg/dcg_code)).
  :- use_module(library(dcg/dcg_collection)).
  :- use_module(library(dcg/dcg_ext)).
  :- use_module(library(dcg/dcg_file)).
  :- use_module(library(dcg/dcg_list)).
  :- use_module(library(dcg/dcg_option)).
  :- use_module(library(dcg/dcg_peek)).
  :- use_module(library(dcg/dcg_pl)).
  :- use_module(library(dcg/dcg_split)).
  :- use_module(library(dcg/dcg_strip)).
  :- use_module(library(dcg/dcg_table)).
  :- use_module(library(dcg/dcg_tree)).
  :- use_module(library(dcg/dcg_unicode)).
  :- use_module(library(dcg/dcg_word_wrap)).
  :- use_module(library(dcg/record_jar)).
  :- use_module(library(dcg/rfc2234)).
:- use_module(library(debug_ext)).
:- use_module(library(default)).
:- use_module(library(dict_ext)).
:- use_module(library(dlist)).
%/fca
  :- use_module(library(fca/fca)).
:- use_module(library(flag_ext)).
% /geo
  :- use_module(library(geo/wkt)).
%/graph
  :- use_module(library(graph/betweenness)).
  :- use_module(library(graph/graph_closure)).
  :- use_module(library(graph/graph_traverse)).
  :- use_module(library(graph/graph_walk)).
  % /l
    :- use_module(library(graph/l/l_graph)).
  % /s
    :- use_module(library(graph/s/s_edge)).
    :- use_module(library(graph/s/s_graph)).
    :- use_module(library(graph/s/s_metrics)).
    :- use_module(library(graph/s/s_subgraph)).
    :- use_module(library(graph/s/s_test)).
    :- use_module(library(graph/s/s_type)).
:- use_module(library(hash_ext)).
%/html
  :- use_module(library(html/html_dcg)).
  :- use_module(library(html/html_dom)).
  :- use_module(library(html/html_resource)).
%/http
  :- use_module(library(http/cors)).
  :- use_module(library(http/csp2)).
  :- use_module(library(http/dcg_http)).
  :- use_module(library(http/google_headers)).
  :- use_module(library(http/http11)).
  :- use_module(library(http/http11_test)).
  :- use_module(library(http/http_download)).
  :- use_module(library(http/http_ext)).
  :- use_module(library(http/http_reply)).
  :- use_module(library(http/http_request)).
  :- use_module(library(http/http_server)).
  :- use_module(library(http/ie_headers)).
  :- use_module(library(http/rest)).
  :- use_module(library(http/rfc1034)).
  :- use_module(library(http/rfc2109)).
  :- use_module(library(http/rfc2616)).
  :- use_module(library(http/rfc2617)).
  :- use_module(library(http/rfc2965)).
  :- use_module(library(http/rfc4790)).
  :- use_module(library(http/rfc5987)).
  :- use_module(library(http/rfc6265)).
  :- use_module(library(http/rfc6266)).
  :- use_module(library(http/rfc6454)).
  :- use_module(library(http/rfc6797)).
  :- use_module(library(http/rfc7034)).
:- use_module(library(image_ext)).
%/iri
  :- use_module(library(iri/iri_ext)).
  :- use_module(library(iri/rfc3987)).
:- use_module(library(json_ext)).
:- use_module(library(list_ext)).
%/ltag
  :- use_module(library(ltag/ltag_match)).
  :- use_module(library(ltag/rfc3066)).
  :- use_module(library(ltag/rfc4646)).
  :- use_module(library(ltag/rfc4647)).
  :- use_module(library(ltag/rfc5646)).
%/mail
:- use_module(library(mail/rfc5322)).
%/math
  :- use_module(library(math/dimension)).
  :- use_module(library(math/math_ext)).
  :- use_module(library(math/positional)).
  :- use_module(library(math/radconv)).
  :- use_module(library(math/rational_ext)).
:- use_module(library(memoization)).
%/nlp
  :- use_module(library(nlp/dbpedia_spotlight)).
  :- use_module(library(nlp/detect_language)).
  :- use_module(library(nlp/nlp_dictionary)).
  :- use_module(library(nlp/nlp_emoticon)).
  :- use_module(library(nlp/nlp_grammar)).
  :- use_module(library(nlp/nlp_lang)).
:- use_module(library(option_ext)).
%/os
  :- use_module(library(os/archive_ext)).
  :- use_module(library(os/compress_ext)).
  :- use_module(library(os/date_time_file)).
  :- use_module(library(os/dir_ext)).
  :- use_module(library(os/external_program)).
  :- use_module(library(os/file_ext)).
  :- use_module(library(os/gnu_plot)).
  :- use_module(library(os/gnu_sort)).
  :- use_module(library(os/io_ext)).
  :- use_module(library(os/open_any2)).
  :- use_module(library(os/os_ext)).
  :- use_module(library(os/pdf)).
  :- use_module(library(os/process_ext)).
  :- use_module(library(os/thread_counter)).
  :- use_module(library(os/thread_ext)).
  :- use_module(library(os/tts)).
:- use_module(library(pagination)).
:- use_module(library(pair_ext)).
%/pl
  :- use_module(library(pl/pl_term)).
:- use_module(library(pool)).
:- use_module(library(print_ext)).
:- use_module(library(service_db)).
%/set
  :- use_module(library(set/direct_subset)).
  :- use_module(library(set/equiv)).
  :- use_module(library(set/intersection)).
  :- use_module(library(set/ordset_closure)).
  :- use_module(library(set/poset)).
  :- use_module(library(set/reflexive_closure)).
  :- use_module(library(set/relation)).
  :- use_module(library(set/relation_closure)).
  :- use_module(library(set/set_ext)).
  :- use_module(library(set/set_ext_experimental)).
  :- use_module(library(set/symmetric_closure)).
  :- use_module(library(set/transitive_closure)).
%/sgml
  :- use_module(library(sgml/sgml_ext)).
:- use_module(library(stream_ext)).
:- use_module(library(string_ext)).
%/stat
:- use_module(library(stat/r_ext)).
%/svg
  :- use_module(library(svg/svg_ext)).
%/tree
  :- use_module(library(tree/l_tree)).
  :- use_module(library(tree/s_tree)).
:- use_module(library(true)).
:- use_module(library(typecheck)).
:- use_module(library(typeconv)).
%/uri
  :- use_module(library(uri/rfc2396)).
  :- use_module(library(uri/rfc3986)).
  :- use_module(library(uri/uri_ext)).
  :- use_module(library(uri/uri_file_name)).
%/url
  :- use_module(library(url/rfc1738)).
:- use_module(library(uuid_ext)).
%/xml
  :- use_module(library(xml/marcxml)).
  :- use_module(library(xml/xml_attr_decl)).
  :- use_module(library(xml/xml_cdata)).
  :- use_module(library(xml/xml_char_data)).
  :- use_module(library(xml/xml_char_ref)).
  :- use_module(library(xml/xml_comment)).
  :- use_module(library(xml/xml_conditional)).
  :- use_module(library(xml/xml_document)).
  :- use_module(library(xml/xml_dom)).
  :- use_module(library(xml/xml_dtd)).
  :- use_module(library(xml/xml_element)).
  :- use_module(library(xml/xml_element_type_decl)).
  :- use_module(library(xml/xml_entity_decl)).
  :- use_module(library(xml/xml_entity_ref)).
  :- use_module(library(xml/xml_external_subset)).
  :- use_module(library(xml/xml_literal)).
  :- use_module(library(xml/xml_name_token)).
  :- use_module(library(xml/xml_notation_decl)).
  :- use_module(library(xml/xml_pi)).
  :- use_module(library(xml/xml_prolog)).
  :- use_module(library(xml/xml_standalone)).
  :- use_module(library(xml/xml10_code)).
  :- use_module(library(xml/xml11_code)).
%/xpath
  :- use_module(library(xpath/xpath_table)).
