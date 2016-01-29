:- module(
  http_print,
  [
    http_iri_metadata//2, % +Indent, +Metadata
    http_status_code//1   % +StatusCode
  ]
).

/** <module> HTTP print

Printing HTTP info.

@author Wouter Beek
@version 2016/01
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(http/http_info)).





%! archive_metadata(+Indent, +Metadata)// is det.

archive_metadata(I, M) -->
  {get_dict(archive_entry, M, MArchiveEntry)}, !,
  archive_entry(I, MArchiveEntry).
archive_metadata(_, _) --> "".

archive_entry(_, [_]) --> !, "".
archive_entry(I, L) -->
  section(I, "Archive metadata:", *(nvpair0(I), L)).



%! http_iri_metadata(+Indent, +Metadata)// is det.

http_iri_metadata(I, M) -->
  iri_metadata(I, M),
  http_metadata(I, M.'llo:http').



%! http_header(+Indent, +Header:pair)// is det.

http_header(I, Key-Values) -->
  *(http_header0(I, Key), Values).

http_header0(I, Key, Value) -->
  tab(I),
  nvpair(http_header_key(Key), http_header_value(Value)).



%! http_header_key(+Key)// is det.

http_header_key(Key1) -->
  {
    atomic_list_concat(Subkeys1, -, Key1),
    maplist(capitalize_atom, Subkeys1, Subkeys2),
    atomic_list_concat(Subkeys2, -, Key2)
  },
  atom(Key2).
  


%! http_header_value(+Value)// is det.

http_header_value(Value) -->
  {string_codes(Value.'llo:raw', Cs)},
  string(Cs),
  nl.



%! http_headers(+Indent, +Headers:list(pair))// is det.

http_headers(I, L) -->
  {partition(http_header_status0, L, L1, L2, L3)},
  http_headers(I, L1, L2, L3).

http_header_status0(_-Vs, Comp) :-
  (Vs = [V|_] -> true ; Vs = V),
  http_header_status_comparator0(V.'llo:parser', Comp).

http_header_status_comparator0(unrecognized, <) :- !.
http_header_status_comparator0(invalid,      =) :- !.
http_header_status_comparator0(valid,        >).



%! http_headers(
%!   +Indent,
%!   +Unrecognized:list(pair),
%!   +Invalid:list(pair),
%!   +Valid:list(pair)
%! )// is det.

http_headers(I1, L1, L2, L3) -->
  {I2 is I1 + 1},
  section(I1, "Unrecognized:", *(http_header(I2), L1)),
  section(I1, "Invalid:", *(http_header(I2), L2)),
  section(I1, "Valid:", *(http_header(I2), L3)).



%! http_metadata(+Indent, +Metadata)// is det.

http_metadata(I1, M) -->
  {
    I2 is I1 + 1,
    I3 is I2 + 1,
    dict_pairs(M.headers, _, L)
  },
  section(I1, "HTTP metadata:", (
    tab_nl(I2, http_status_code(M.'llo:status-code')),
    tab_nl(I2, nvpair("Version", pl_pair(M.'llo:version'))),
    tab_nl(I2, nvpair("Final IRI", iri(M.'llo:final_iri'))),
    section(I2, "Headers:", http_headers(I3, L))
  )).



%! http_status_code(+Code:between(100,599))// is det.

http_status_code(Code) -->
  "HTTP status code: ",
  integer(Code),
  " ",
  {http_status_label(Code, Label)},
  "(", atom(Label), ")".



%! iri_metadata(+Indent, +Metadata)// is det.

iri_metadata(I1, M) -->
  {I2 is I1 + 1},
  section(I1, "IRI metadata:", tab_nl(I2, nvpair("Base IRI", iri(M.'llo:base-iri')))).
