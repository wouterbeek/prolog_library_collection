:- module(
  xml_dom,
  [
    atom_to_xml_dom/2, % +Atom:atom
                       % -Dom:list(compound)
    xml_dom_to_atom/2, % +Dom, -Atom
    xml_dom_to_atom/3, % +Dom:list(compound)
                       % -Atom:atom
                       % +Options:list(compound)
    xml_serve_dom/2 % +Dom:list(compound)
                    % +Options:list(compound)
  ]
).

/** <module> XML DOM

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(http/http_path)).
:- use_module(library(http/http_request)).
:- use_module(library(memfile)).
:- use_module(library(option)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).

:- predicate_options(xml_dom_to_atom/3, 3, [
     style(+atom),
     pass_to(xml_write/3, 3)
   ]).





%! atom_to_xml_dom(+Atom:atom, Dom:compound) is det.

atom_to_xml_dom(A, Dom):-
  setup_call_cleanup(
    atom_to_memory_file(A, Handle),
    setup_call_cleanup(
      open_memory_file(Handle, read, Read),
      load_xml(Read, Dom, []),
      close(Read)
    ),
    free_memory_file(Handle)
  ).



%! stylesheet_pi(+Spec:compound, -Pi:compound) is det.
% Wrapper around stylesheet_pi/3 using MIME `text/css`.

stylesheet_pi(Spec, Pi):-
  stylesheet_pi('text/css', Spec, Pi).

%! stylesheet_pi(+Mine:atom, +Spec:compound, -Pi:compound) is det.

stylesheet_pi(Mime, Spec, pi(Pi)):-
  http_absolute_location(Spec, Loc, []),
  format(atom(Pi), 'xml-stylesheet type="~w" href="~w"', [Mime,Loc]).



%! xml_dom_to_atom(+Dom:list(compound), -Xml:atom) is det.
% Wrapper around xml_dom_to_atom/3.

xml_dom_to_atom(Dom, A):-
  xml_dom_to_atom(Dom, A, []).


%! xml_dom_to_atom(
%!   +Dom:list(compound),
%!   -Xml:atom,
%!   +Options:list(compound)
%! ) is det.
% The following options are supported:
%   * `dtd(+Doctype:atom)`
%     The atomic name of the DTD that should be used for the XML DOM.
%     The DTD is first searched for in the cache of DTD objects.
%     If the given doctype has no associated DTD in the cache,
%     it searches for a file using the file search path =dtd=.
%   * `style(+StyleName:atom)`
%     The atomic name of a style file on the =css= search path.

xml_dom_to_atom(Dom, A, Opts1):-
  % Add style to XML DOM.
  (   select_option(style(Base), Opts1, Opts2)
  ->  file_name_extension(Base, css, LocalName),
      stylesheet_pi(css(LocalName), Pi),
      Dom0 = [Pi|Dom]
  ;   Dom0 = Dom,
      Opts2 = Opts1
  ),

  % Set the header to false, since this XML content will be inserted inside
  % a Web page.
  % We do add the stylesheet parsing instruction, as this is allowed
  % in -- at least -- Firefox.
  merge_options([header(false)], Opts2, Opts3),
  with_output_to(atom(A), xml_write(Dom0, Opts3)).



%! xml_serve_dom(+Dom:list(compound), +Options:list(compound)) is det.
% Serves the given XML DOM.
%
% The following options are supported:
%   * `dtd(+Doctype:atom)`
%     The atomic name of the DTD that should be used for the XML DOM.
%     The DTD is first searched for in the cache of DTD objects.
%     If the given doctype has no associated DTD in the cache,
%     it searches for a file using the file search path =dtd=.
%   * `style(+StyleName:atom)`
%     The atomic name of a style file on the =css= search path.

xml_serve_dom(Dom, Options):-
  xml_dom_to_atom(Dom, A, Options),
  % The User Agent needs to know the content type and encoding.
  % If the UTF-8 encoding is not given here explicitly,
  % Prolog throws an IO exception on `format(XML)`.
  format('Content-type: application/xml; charset=utf-8~n~n'),
  format(A).
