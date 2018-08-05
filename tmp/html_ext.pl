%! html_insert_dom(+Dom:list(compound))// is det.

html_insert_dom(Dom) -->
  {with_output_to(atom(Atom), xml_write_canonical(current_output, Dom, []))},
  html(\[Atom]).
