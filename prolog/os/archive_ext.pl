:- module(
  archive_ext,
  [
    archive_info/1, % +In
    call_archive/2, % +In
                    % :Goal_1
    call_archive_entry/2, % +In
                          % :Goal_1
    call_archive_entry/3 % +In
                         % +Entry:atom
                         % :Goal_1
  ]
).
:- reexport(library(archive)).

/** <module> Archive extensions

@author Wouter Beek
@version 2015/09-2015/10
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_msg)).
:- use_module(library(debug)).
:- use_module(library(iostream)).

:- meta_predicate(call_archive(+,1)).
:- meta_predicate(call_archive_entry(+,1)).
:- meta_predicate(call_archive_entry(+,+,1)).
:- meta_predicate(call_archive_entry0(+,1,+)).





%! archive_info(+In) is det.
% Writes archive information for the given file or URL to current input.
%
% ### Example
%
% ```prolog
% ?- absolute_file_name(data('abcde.tar.gz'), File, [access(read)]),
%    archive_info(File).
% ab.tar.gz
%   filetype(file)
%   mtime(1402126051.0)
%   size(128)
%   format(posix ustar format)
%   a.txt
%     filetype(file)
%     mtime(1402126033.0)
%     size(2)
%     format(posix ustar format)
%   b.txt
%     filetype(file)
%     mtime(1402126038.0)
%     size(2)
%     format(posix ustar format)
% cd.tar.gz
%   filetype(file)
%   mtime(1402126098.0)
%   size(128)
%   format(posix ustar format)
%   d.txt
%     filetype(file)
%     mtime(1402126074.0)
%     size(2)
%     format(posix ustar format)
%   c.txt
%     filetype(file)
%     mtime(1402126067.0)
%     size(2)
%     format(posix ustar format)
% e.txt
%   filetype(file)
%   mtime(1402126131.0)
%   size(2)
%   format(posix ustar format)
% File = '.../data/abcde.tar.gz'.
% ```

archive_info(In):-
  call_archive(In, archive_info0(0)).

archive_info0(M, Arch):-
  repeat,
  (   archive_next_header(Arch, Entry),
      \+ is_leaf_entry(Arch, Entry)
  ->  dcg_msg_normal(archive_entry(M, Arch, Entry)),
      succ(M, N),
      setup_call_cleanup(
	archive_open_entry(Arch, Read),
	call_archive(Read, archive_info0(N)),
	close(Read)
      ),
      fail
  ;   !,
      true
  ).



%! call_archive(+In, :Goal_1) is det.

call_archive(In, Goal_1):-
  ArchOpts = [close_parent(false),filter(all),format(all),format(raw)],
  setup_call_cleanup(
    open_any(In, read, Read, Close, []),
    setup_call_cleanup(
      archive_open(Read, Arch, ArchOpts),
      call(Goal_1, Arch),
      archive_close(Arch)
    ),
    close_any(Close)
  ).



%! call_archive_entry(+In, :Goal_1) is nondet.

call_archive_entry(In, Goal_1):-
  call_archive_entry(In, _, Goal_1).


%! call_archive_entry(+In, +Entry:atom, :Goal_1) is nondet.

call_archive_entry(In, Entry, Goal_1):-
  call_archive(In, call_archive_entry0(Entry, Goal_1)).

call_archive_entry0(Entry, Goal_1, Arch):-
  archive_next_header(Arch, Entry0),
  debug(archive_ext, "Archive entry: ~w", [Entry0]),
  (   Entry = Entry0
  ->  archive_open_entry(Arch, Read),
      call(Goal_1, Read)
  ;   call_archive_entry0(Entry, Goal_1, Arch)
  ).





% HELPERS %

%! is_leaf_entry(+Archive:archive, +Entry:atom) is semidet.

is_leaf_entry(Arch, Entry):-
  archive_header_property(Arch, format(Format)),
  Entry == data,
  Format == raw.





% MESSAGES %

archive_entry(M, Arch, Entry) -->
  archive_header(M, Entry),
  {
    findall(P, archive_header_property(Arch, P), Ps),
    succ(M, N)
  },
  archive_properties(N, Ps).

archive_header(M, Entry) -->
  indent(M),
  atom(Entry),
  nl.

archive_properties(_, []) --> !, [].
archive_properties(M, [H|T]) -->
  indent(M),
  atom(H),
  nl,
  archive_properties(M, T).
