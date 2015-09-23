:- module(
  archive_ext,
  [
    archive_named_entry/3 % +EntryName:atom
                          % +Archive:blob
                          % -Read:blob
  ]
).
:- reexport(library(archive)).

/** <module> Archive extensions

@author Wouter Beek
@version 2015/09
*/





%! archive_named_entry(+Entry:atom, +Archive:archive, -Read:stream) is det.

archive_named_entry(Entry, Archive, Read):-
  archive_next_header(Archive, Entry0),
  (   Entry0 == Entry
  ->  archive_open_entry(Archive, Read)
  ;   archive_named_entry(Entry, Archive, Read)
  ).
