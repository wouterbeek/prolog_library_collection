:- module(
  dbnl_downloads,
  [
    dbnl_downloads/3 % +Graph:atom
                     % +Title:uri
                     % +URI:uri
  ]
).

/** <module> DBNL DOWNLOADS

Scraping a DBNL text download page.

@author Wouter Beek
@version 2013/05
*/

:- use_module(dbnl(dbnl_generic)).
:- use_module(generics(uri_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



%! dbnl_downloads(+Graph:atom, +Title:uri, +URI:uri) is det.
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

dbnl_downloads(Graph, Title, URI):-
  dbnl_uri_to_html(URI, DOM),
  dbnl_dom_center(DOM, Contents0),
  xpath2(Contents0, div(content), Contents1),
  
  % ePUB format of the text.
  Contents1 = [_H3, _BR1, element(a, [name=epub_tekst | _], _) | Contents2],
  Contents2 =
    [element(p, _, ['Geen e-book van tekstbestand gevonden.']) | Contents3],

  % PDFs of the text.
  Contents3 = [_BR2, element(a, [name=pdf_tekst | _], _) | Contents4],
  (
    Contents4 =
      [element(p, _, ['Geen pdf van tekstbestand gevonden.']) | Contents5]
  ->
    true
  ;
    Contents4 =
      [
        element(p, _, [element(a, [href=PDFTextRelativeURI | _], _) | _])
      | Contents5
      ],
    dbnl_uri_resolve(PDFTextRelativeURI, PDFTextAbsoluteURI),
    rdf_assert(Title, dbnl:remote_pdftext, PDFTextAbsoluteURI, Graph),
    uri_query(PDFTextAbsoluteURI, filename, PDFTextFileName),
    absolute_file_name(file(PDFTextFileName), PDFTextFile, []),
    uri_to_file(PDFTextAbsoluteURI, PDFTextFile),
    rdf_assert_datatype(Title, dbnl:local_pdftext, file, PDFTextFile, Graph)
  ),

  % PDFs of the originals.
  Contents5 = [_BR3, element(a, [name=pdf_orig | _], _) | Contents6],
  Contents6 =
    [element(p, _, ['Geen pdf van originelen gevonden']) | Contents7],

  % Scans of the originals.
  (
    Contents7 = []
  ->
    true
  ;
    Contents7 = [_BR4, element(a, [name=orig | _], _) | Contents8],
    Contents8 = [_Text, element(a, [href=ScansRelativeURI | _], _) | _],
    dbnl_uri_resolve(ScansRelativeURI, ScansAbsoluteURI),
    rdf_assert(Title, dbnl:remote_scans, ScansAbsoluteURI, Graph),
    uri_query(ScansAbsoluteURI, filename, FileName),
    absolute_file_name(file(FileName), ScansFile, []),
    uri_to_file(ScansAbsoluteURI, ScansFile),
    rdf_assert_datatype(Title, dbnl:local_scans, file, ScansFile, Graph)
  ),
  !.
% Debug.
dbnl_downloads(_Graph, _Title, URI):-
  gtrace, %DEB
  format(user_output, '~w\n', [URI]).

