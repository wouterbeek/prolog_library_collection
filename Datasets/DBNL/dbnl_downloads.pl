:- module(
  dbnl_downloads,
  [
    dbnl_downloads/3 % +Graph:atom
                     % +Text:uri
                     % +Contents:dom
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

% Done!
dbnl_downloads(_Graph, _Text, []):-
  !.
% Parse nested DIVs.
dbnl_downloads(Graph, Text, [element(div, _, DIV_T) | T]):-
  !,
  dbnl_downloads(Graph, Text, DIV_T),
  dbnl_downloads(Graph, Text, T).
% Skip subheaders.
dbnl_downloads(Graph, Text, [element(h3, _, _) | T]):-
  !,
  dbnl_downloads(Graph, Text, T).
% Skip line breaks.
dbnl_downloads(Graph, Text, [element(br, _, _) | T]):-
  !,
  dbnl_downloads(Graph, Text, T).
% Skip empty ePUB format.
dbnl_downloads(
  Graph,
  Text,
  [
    element(a, [name=epub_tekst | _], _),
    element(p, [], ['Geen e-book van tekstbestand gevonden.'])
  | T]
):-
  !,
  dbnl_downloads(Graph, Text, T).
% Skip empty PDFs of the text.
dbnl_downloads(
  Graph,
  Text,
  [
    element(a, [name=pdf_tekst | _], _),
    element(p, [], ['Geen pdf van tekstbestand gevonden.'])
  | T]
):-
  !,
  dbnl_downloads(Graph, Text, T).
% PDFs of the text.
dbnl_downloads(
  Graph,
  Text,
  [
    element(a, [name=pdf_tekst | _], _),
    element(p, [], [element(a, [href=PDFTextRelativeURI], _)])
  | T]
):-
  !,
  dbnl_uri_resolve(PDFTextRelativeURI, PDFTextAbsoluteURI),
  rdf_assert(Text, dbnl:remote_pdftext, PDFTextAbsoluteURI, Graph),
  uri_query(PDFTextAbsoluteURI, filename, PDFTextFileName),
  absolute_file_name(file(PDFTextFileName), PDFTextFile, []),
  uri_to_file(PDFTextAbsoluteURI, PDFTextFile),
  rdf_assert_datatype(Text, dbnl:local_pdftext, file, PDFTextFile, Graph),
  dbnl_downloads(Graph, Text, T).
% Skip empty PDFs of the originals.
dbnl_downloads(
  Graph,
  Text,
  [
    element(a, [name=pdf_orig | _], _),
    element(p, [], ['Geen pdf van originelen gevonden'])
  | T]
):-
  !,
  dbnl_downloads(Graph, Text, T).
% PDFs of the originals.
dbnl_downloads(
  Graph,
  Text,
  [
    element(a, [name=pdf_orig | _], _),
    element(p, [], [element(a, [href=ScansRelativeURI], _)])
  | T]
):-
  !,
  dbnl_uri_resolve(ScansRelativeURI, ScansAbsoluteURI),
  rdf_assert(Text, dbnl:remote_scans, ScansAbsoluteURI, Graph),
  uri_query(ScansAbsoluteURI, filename, FileName),
  absolute_file_name(file(FileName), ScansFile, []),
  uri_to_file(ScansAbsoluteURI, ScansFile),
  rdf_assert_datatype(Text, dbnl:local_scans, file, ScansFile, Graph),
  dbnl_downloads(Graph, Text, T).
% Scans of originals.
dbnl_downloads(
  Graph,
  Text,
  [element(a, [name=orig | _], _), _, element(a, [href=RelativeURI | _], _) | T]
):-
  !,
  dbnl_uri_resolve(RelativeURI, AbsoluteURI),
  rdf_assert(Text, dbnl:remote_original, AbsoluteURI, Graph),
  uri_query(AbsoluteURI, filename, FileName),
  absolute_file_name(file(FileName), ScansFile, []),
  uri_to_file(AbsoluteURI, ScansFile),
  rdf_assert_datatype(Text, dbnl:local_original, file, ScansFile, Graph),
  dbnl_downloads(Graph, Text, T).
% Debug.
dbnl_downloads(Graph, Text, [H | T]):-
  gtrace, %DEB
  format(user_output, '~w\n', [H]),
  dbnl_downloads(Graph, Text, T).

