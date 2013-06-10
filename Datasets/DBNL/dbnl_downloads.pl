:- module(
  dbnl_downloads,
  [
    dbnl_downloads//2, % +Graph:atom
                      % +Text:uri
    dbnl_scans/3 % +Graph:atom
                 % +Text:uri
                 % +Contents:dom
  ]
).

/** <module> DBNL DOWNLOADS

Scraping a DBNL text download page.

There are two typoes of download page:
  * dbnl_downloads/3, identified by:
    =|xpath2(Contents, //h3(content), ['Downloads'])|=
  * dbnl_scans/3, identified by:
    =|xpath2(Contents, //div(@class='pb-scan'), _)|=

@author Wouter Beek
@tbd Also scrape individual pages in dbnl_scans/3.
@version 2013/05
*/

:- use_module(dbnl(dbnl_generic)).
:- use_module(dcg(dcg_generic)).
:- use_module(generics(uri_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



%! dbnl_downloads(+Graph:atom, +Text:uri, +Contents:dom) is det.
% Processed the downloads page for a given title.
%
% We are interested in the following information:
%   1. ePub format files.
%   2. PDF files of text.
%   3. PDF files of originals.
%   4. Original scans.
%
% Example of a URI pointing to original scans:
% ~~~{.txt}
% http://www.dbnl.org/tekst/saveas.php?
%   filename=_12m00112me01_01.pdf&
%   dir=/arch/_12m00112me01_01/pag&
%   type=pdf&
%   common=1
% ~~~

% Parse nested DIVs.
dbnl_downloads(Graph, Text) -->
  dcg_element(div, [], DIV_T),
  !,
  {phrase(dbnl_downloads(Graph, Text), DIV_T)},
  dbnl_downloads(Graph, Text).
% Skip subheaders.
dbnl_downloads(Graph, Text) -->
  dcg_element(h3, [], _),
  !,
  dbnl_downloads(Graph, Text).
% Skip line breaks.
dbnl_downloads(Graph, Text) -->
  dcg_element(br, [], _),
  !,
  dbnl_downloads(Graph, Text).
% Process links. There are four categories.
dbnl_downloads(Graph, Text) -->
  dcg_element(a, [name=Name1], _),
  !,
  {dbnl_downloads_translate(Name1, Name2)},
  dbnl_downloads_link(Graph, Text, Name2),
  dbnl_downloads(Graph, Text).
dbnl_downloads(_Graph, _Text) --> [], !.
% Debug.
dbnl_downloads(_Graph, _Text) -->
  dcg_debug.

% Zero links. Only a message saying nothing is here.
dbnl_downloads_link(Graph, Text, Name) -->
  dcg_element(p, [], [Atom]),
  {atom(Atom)},
  !,
  dbnl_downloads_link(Graph, Text, Name).
% A paragraph with DOM content.
dbnl_downloads_link(Graph, Text, Name) -->
  dcg_element(p, [], Content),
  !,
  {phrase(dbnl_downloads_link(Graph, Text, Name), Content)},
  dbnl_downloads_link(Graph, Text, Name).
% A download link. This is scraped.
dbnl_downloads_link(Graph, Text, Name) -->
  dcg_element(a, [href=RelativeURI], [download]),
  ['-'],
  dcg_element(a, [href=_RelativeURI], [bekijk]),
  [_FileSizeAtom],
  !,
  {dbnl_downloads_link0(Graph, Text, RelativeURI, Name)},
  dbnl_downloads_link(Graph, Text, Name).
% Consecutive download links may occur, interspersed by linebreaks.
dbnl_downloads_link(Graph, Text, Name) -->
  [_Atom],
  dcg_element(a, [href=RelativeURI], _),
  dcg_element(br, [], _),
  !,
  {dbnl_downloads_link0(Graph, Text, RelativeURI, Name)},
  dbnl_downloads_link(Graph, Text, Name).
dbnl_downloads_link(_Graph, _Text, _Name) --> [].

dbnl_downloads_link0(Graph, Text, RelativeURI, Name):-
  dbnl_uri_resolve(RelativeURI, AbsoluteURI),

  % Remote link.
  atomic_list_concat([remote,Name], '_', RemoteName),
  rdf_global_id(dbnl:RemoteName, RemotePredicate),
  rdf_assert(Text, RemotePredicate, AbsoluteURI, Graph),

  % Local link.
  % Base the file name on a query item.
  uri_query(AbsoluteURI, filename, FileName),
  absolute_file_name(file(FileName), File, []),
  uri_to_file(AbsoluteURI, File),
  atomic_list_concat([local,Name], '_', LocalName),
  rdf_global_id(dbnl:LocalName, LocalPredicate),
  rdf_assert_datatype(Text, LocalPredicate, file, File, Graph).

dbnl_downloads_translate(epub_tekst, epub    ).
dbnl_downloads_translate(orig,       original).
dbnl_downloads_translate(pdf_orig,   scan    ).
dbnl_downloads_translate(pdf_tekst,  pdf     ).



% SCANS %

dbnl_scans(Graph, Text, Contents):-
  forall(
    (
      xpath2(Contents, //a(self), A),
      A = element(a, Attrs, [Atom]),
      (Atom = 'Bekijk het PDF bestand.' ; Atom = 'Bekijk het OCR resultaat.')
    ),
    (
      memberchk(href=RelativeURI, Attrs),
      dbnl_uri_resolve(RelativeURI, AbsoluteURI),
      rdf_assert(Text, dbnl:remote_original, AbsoluteURI, Graph),
      % Base the file name on the URI.
      uri_to_file_name(AbsoluteURI, FileName),
      % The extension is already present in the file name,
      % as determined by the URI.
      absolute_file_name(file(FileName), ScansFile, []),
      uri_to_file(AbsoluteURI, ScansFile),
      rdf_assert_datatype(Text, dbnl:local_original, file, ScansFile, Graph)
    )
  ).

