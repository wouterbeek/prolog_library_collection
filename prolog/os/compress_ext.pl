:- module(
  compress_ext,
  [
    compress_file/1, % +From
    compress_file/2, % +From, +Compress
    compress_file/3  % +From, +Compress, +To
  ]
).

/** <module> Compress extension

@author Wouter Beek
@version 2016/01, 2016/03
*/

:- use_module(library(os/file_ext)).
:- use_module(library(zlib)).





%! compress_file(+From) is det.
%! compress_file(+From, +Compress) is det.
%! compress_file(+From, +Compress, +To) is det.
% The following values are supported for Compress:
%   - `deflate’
%   - `gzip’ (default)
%   - `none’

compress_file(From) :-
  compress_file(From, gzip).


compress_file(From, Compress) :-
  thread_file(From, Tmp),
  compress_file(From, Compress, Tmp),
  rename_file(Tmp, From).


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
