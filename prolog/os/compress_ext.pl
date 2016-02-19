:- module(
  compress_ext,
  [
    compress_file/3 % +From, +Compress:oneof([deflate,gzip,none]), +To
  ]
).

/** <module> Compress extension

@author Wouter Beek
@version 2016/01
*/

:- use_module(library(zlib)).





%! compress_file(+From, +Compress:oneof([deflate,gzip,none]), +To) is det.

compress_file(From, none, To) :- !,
  rename_file(From, To).
compress_file(From, Compress, To) :-
  setup_call_cleanup(
    gzopen(To, write, Write, [format(Compress)]),
    setup_call_cleanup(
      open(From, read, Read),
      copy_stream_data(Read, Write),
      close(Read)
    ),
    close(Write)
  ).
