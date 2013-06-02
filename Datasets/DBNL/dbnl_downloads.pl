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
% ==
% http://www.dbnl.org/tekst/saveas.php?
%   filename=_12m00112me01_01.pdf&
%   dir=/arch/_12m00112me01_01/pag&
%   type=pdf&
%   common=1
% ==

% Parse nested DIVs.
dbnl_downloads(Graph, Text) -->
  [element(div, _, DIV_T)],
  !,
  {phrase(dbnl_downloads(Graph, Text), DIV_T)},
  dbnl_downloads(Graph, Text).
% Skip subheaders.
dbnl_downloads(Graph, Text) -->
  [element(h3, _, _)],
  !,
  dbnl_downloads(Graph, Text).
% Skip line breaks.
dbnl_downloads(Graph, Text) -->
  [element(br, _, _)],
  !,
  dbnl_downloads(Graph, Text).
% Skip empty ePUB format.
dbnl_downloads(Graph, Text) -->
  [
    element(a, [name=epub_tekst|_], _),
    element(p, [], ['Geen e-book van tekstbestand gevonden.'])
  ],
  !,
  dbnl_downloads(Graph, Text).
% Skip empty PDFs of the text.
dbnl_downloads(Graph, Text) -->
  [
    element(a, [name=pdf_tekst|_], _),
    element(p, [], ['Geen pdf van tekstbestand gevonden.'])
  ],
  !,
  dbnl_downloads(Graph, Text).
% PDFs of the text.
dbnl_downloads(Graph, Text) -->
  [
    element(a, [name=pdf_tekst|_], _),
    element(p, [], [element(a, [href=PDFTextRelativeURI|_], _)|_])
  ],
  !,
  {
    dbnl_uri_resolve(PDFTextRelativeURI, PDFTextAbsoluteURI),
    rdf_assert(Text, dbnl:remote_pdftext, PDFTextAbsoluteURI, Graph),
    % Base the file name on a query item.
    uri_query(PDFTextAbsoluteURI, filename, PDFTextFileName),
    absolute_file_name(file(PDFTextFileName), PDFTextFile, []),
    uri_to_file(PDFTextAbsoluteURI, PDFTextFile),
    rdf_assert_datatype(Text, dbnl:local_pdftext, file, PDFTextFile, Graph)
  },
  dbnl_downloads(Graph, Text).
% Skip empty PDFs of the originals.
dbnl_downloads(Graph, Text) -->
  [
    element(a, [name=pdf_orig|_], _),
    element(p, [], ['Geen pdf van originelen gevonden'])
  ],
  !,
  dbnl_downloads(Graph, Text).
% PDFs of the originals.
dbnl_downloads(Graph, Text) -->
  [
    element(a, [name=pdf_orig|_], _),
    element(p, [], [element(a, [href=ScansRelativeURI], _)|_])
  ],
  !,
  {
    dbnl_uri_resolve(ScansRelativeURI, ScansAbsoluteURI),
    rdf_assert(Text, dbnl:remote_scans, ScansAbsoluteURI, Graph),
    % Base the file name on a query item.
    uri_query(ScansAbsoluteURI, filename, FileName),
    absolute_file_name(file(FileName), ScansFile, []),
    uri_to_file(ScansAbsoluteURI, ScansFile),
    rdf_assert_datatype(Text, dbnl:local_scans, file, ScansFile, Graph)
  },
  dbnl_downloads(Graph, Text).
% Scans of originals.
dbnl_downloads(Graph, Text) -->
  dcg_element(a, [name=orig], _),
  !,
  dcg_star(dbnl_downloads_original(Graph, Text)),
  dbnl_downloads(Graph, Text).
dbnl_downloads(_Graph, _Text) --> [], !.
% Debug.
dbnl_downloads(_Graph, _Text) -->
  dcg_debug.

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

dbnl_downloads_original(Graph, Text) -->
  [_Atom, element(a, Attrs, _), _BR],
  !,
  {
    memberchk(href=RelativeURI, Attrs),
    dbnl_uri_resolve(RelativeURI, AbsoluteURI),
    rdf_assert(Text, dbnl:remote_original, AbsoluteURI, Graph),
    % Base the file name on a query item.
    uri_query(AbsoluteURI, filename, FileName),
    absolute_file_name(file(FileName), ScansFile, []),
    uri_to_file(AbsoluteURI, ScansFile),
    rdf_assert_datatype(Text, dbnl:local_original, file, ScansFile, Graph)
  }.

