:- module(
  archive_ext,
  [
    archive_info/1, % +In
    archive_info/2, % +In
                    % +Options:list(compound)
    call_archive/2, % +In
                    % :Goal_2
    call_archive/3, % +In
                    % :Goal_2
                    % +Options:list(compound)
    call_archive_entry/2, % +In
                          % :Goal_2
    call_archive_entry/3, % +In
                          % :Goal_2
                          % +Options:list(compound)
    call_archive_entry/4 % +In
                         % +Entry:atom
                         % :Goal_2
                         % +Options:list(compound)
  ]
).
:- reexport(library(archive)).

/** <module> Archive extensions

@author Wouter Beek
@version 2015/09-2015/10
*/

:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(open_any2)).

:- meta_predicate(call_archive(+,2)).
:- meta_predicate(call_archive(+,2,+)).
:- meta_predicate(call_archive_entry(+,2)).
:- meta_predicate(call_archive_entry(+,2,+)).
:- meta_predicate(call_archive_entry(+,+,2,+)).
:- meta_predicate(call_archive_entry0(+,2,+,+,+)).

:- predicate_options(archive_info/2, 2, [
     indent(+nonneg),
     pass_to(call_archive_entry/3, 3)
   ]).
:- predicate_options(call_archive/3, 3, [
     pass_to(open_any/5, 5),
     pass_to(archive_open/3, 3)
   ]).
:- predicate_options(call_archive_entry/3, 3, [
     pass_to(call_archive_entry/3, 3)
   ]).
:- predicate_options(call_archive_entry/4, 4, [
     pass_to(call_archive/3, 3),
     pass_to(call_archive_entry0/5, 3)
   ]).
:- predicate_options(call_archive_entry0/5, 3, [
     pass_to(archive_data_stream/3, 3)
   ]).





%! archive_info(+In) is det.
% Wrapper around archive_info/2 with default options.

archive_info(In):-
  archive_info(In, []).


%! archive_info(+In, +Options:list(compound)) is det.
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
%
% The following options are supported:
%   * indent(+nonneg)
%     Default is 0.

archive_info(In, Opts):-
  option(indent(I), Opts, 0),
  call_archive_entry(In, archive_info0(I), Opts).

archive_info0(I, M, _):-
  print_dict(M, I).



%! call_archive(+In, :Goal_2) is det.
% Wrapper around call_archive/3 with default options.

call_archive(In, Goal_2):-
  call_archive(In, Goal_2, []).


%! call_archive(+In, :Goal_2, +Options:list(compound)) is det.

call_archive(In, Goal_2, Opts):-
  % Archive options that cannot be overridden.
  merge_options([close_parent(false)], Opts, ArchOpts0),
  % Archive options that can be overridden.
  merge_options(ArchOpts0, [filter(all),format(all),format(raw)], ArchOpts),
  setup_call_cleanup(
    open_any2(In, read, Read, Close_0, [metadata(OpenM)|Opts]),
    setup_call_cleanup(
      archive_open(Read, Arch, ArchOpts),
      call(Goal_2, OpenM, Arch),
      archive_close(Arch)
    ),
    close_any2(Close_0)
  ).



%! call_archive_entry(+In, :Goal_2) is nondet.
% Wrapper around call_archive_entry/3 with default options.

call_archive_entry(In, Goal_2):-
  call_archive_entry(In, Goal_2, []).


%! call_archive_entry(+In, :Goal_2, +Options:list(compound)) is nondet.
% Wrapper around call_archive_entry/4 with uninstantiated Entry.

call_archive_entry(In, Goal_2, Opts):-
  call_archive_entry(In, _, Goal_2, Opts).


%! call_archive_entry(
%!   +In,
%!   ?Entry:atom,
%!   :Goal_2,
%!   +Options:list(compound)
%! ) is nondet.

call_archive_entry(In, Entry, Goal_2, Opts):-
  call_archive(In, call_archive_entry0(Entry, Goal_2, Opts), Opts).

call_archive_entry0(Entry, Goal_2, Opts, MArch1, Arch):-
  merge_options([meta_data(MEntry)], Opts, Opts0),
  archive_data_stream(Arch, Read, Opts0),
  (   MEntry = [MEntry1|_],
      MEntry1.name = Entry
  ->  put_dict(entry, MArch1, MEntry, MArch2),
      gtrace,
      call(Goal_2, MArch2, Read)
  ;   close(Read),
      fail
  ).
