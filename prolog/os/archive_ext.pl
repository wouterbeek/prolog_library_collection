:- module(
  archive_ext,
  [
    archive_info/1, % +In
    archive_info/2, % +In
                    % +Options:list(compound)
    call_on_archive/2, % +In
                       % :Goal_2
    call_on_archive/3 % +In
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
:- use_module(library(lambda)).
:- use_module(library(os/open_any2)).

:- meta_predicate(call_on_archive(+,2)).
:- meta_predicate(call_on_archive(+,2,+)).
:- meta_predicate(call_on_archive0(+,2,+)).
:- meta_predicate(call_on_archive_entry0(+,2,+,+,+)).

:- predicate_options(archive_info/2, 2, [
     indent(+nonneg),
     pass_to(call_on_archive/3, 3)
   ]).
:- predicate_options(call_on_archive/3, 3, [
     archive_entry(+atom),
     pass_to(call_on_archive_entry0/5, 3),
     pass_to(call_on_archive0/3, 3)
   ]).
:- predicate_options(call_on_archive0/3, 3, [
     pass_to(open_any2/5, 5),
     pass_to(archive_open/3, 3)
   ]).
:- predicate_options(call_on_archive_entry0/5, 3, [
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
  call_on_archive(In, \M^_^print_dict(M, I), Opts).



%! call_on_archive(+In, :Goal_2) is det.
% Wrapper around call_on_archive/3 with default options.

call_on_archive(In, Goal_2):-
  call_on_archive(In, Goal_2, []).

call_on_archive(In, Goal_2, Opts):-
  option(archive_entry(Entry), Opts, _),
  call_on_archive0(In, call_on_archive_entry0(Entry, Goal_2, Opts), Opts).

call_on_archive0(In, Goal_2, Opts1):-
  % Archive options that cannot be overridden.
  merge_options([close_parent(false)], Opts1, Opts2),
  % Archive options that can be overridden.
  merge_options(Opts2, [filter(all),format(all),format(raw)], Opts3),
  setup_call_cleanup(
    open_any2(In, read, Read, Close_0, [metadata(M)|Opts1]),
    setup_call_cleanup(
      archive_open(Read, Arch, Opts3),
      call(Goal_2, M, Arch),
      archive_close(Arch)
    ),
    close_any2(Close_0)
  ).

call_on_archive_entry0(Entry, Goal_2, Opts1, M1, Arch):-
  merge_options([meta_data(MEntry)], Opts1, Opts2),
  archive_data_stream(Arch, Read, Opts2),
  (   MEntry = [MEntry1|_],
      MEntry1.name = Entry
  ->  put_dict(entry, M1, MEntry, M2),
      call(Goal_2, M2, Read)
  ;   close(Read),
      fail
  ).
