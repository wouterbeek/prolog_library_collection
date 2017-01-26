:- module(
  compress_ext,
  [
    compress_file/1, % +From
    compress_file/2, % +From, +To
    compress_file/3  % +From, +To, +Compress
  ]
).

/** <module> Compress extension

@author Wouter Beek
@version 2016/01, 2016/03, 2016/08
*/

:- use_module(library(file_ext)).
:- use_module(library(zlib)).





%! compress_file(+From) is det.
%! compress_file(+From, +To) is det.
%! compress_file(+From, +To, +Compress) is det.
%
% The following values are supported for Compress:
%
%   - `deflate'
%
%   - `gzip' (default)
%
%   - `none'

compress_file(From) :-
  thread_file(From, Tmp),
  compress_file(From, Tmp, gzip),
  rename_file(Tmp, From).


compress_file(From, To) :-
  compress_file(From, To, gzip).


compress_file(From, To, none) :- !,
  rename_file(From, To).
compress_file(From, To, Compress) :-
  setup_call_cleanup(
    gzopen(To, write, Write, [format(Compress)]),
    setup_call_cleanup(
      open(From, read, Read),
      copy_stream_data(Read, Write),
      close(Read)
    ),
    close(Write)
  ).
