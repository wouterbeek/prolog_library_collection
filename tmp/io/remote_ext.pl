:- module(
  remote_ext,
  [
    clear_remote_directory/1, % +RemoteDirectory:compound
    exists_remote_file/1, % +RemoteFile:compound
    make_remote_directory/1, % +RemoteDirectory:compound
    make_remote_directory_path/1, % +RemoteDirectory:compound
    remote_open/3, % +RemoteFile:compound
                   % +Mode:oneof([append,read,write])
                   % -Stream:stream
    remote_open/4 % +RemoteFile:compound
                  % +Mode:oneof([append,read,write])
                  % -Stream:stream
                  % +Options:list(nvpair)
  ]
).

/** <module> Remote extensions

Support for files residing on remote machines.

@author Wouter Beek
@version 2014/05, 2015/03
*/

:- use_module(library(filesex)).

:- use_module(plc(io/dir_ext)).
:- use_module(plc(process/process_ext)).

:- predicate_options(remote_open/4, 4, [
  pass_to(open/4, 4)
]).





%! clear_remote_directory(+RemoteDirectory:compound) is det.

clear_remote_directory(remote_file(User,Machine,Dir)):-
  atomic_list_concat([User,Machine], '@', UserMachine),
  append_directories(Dir, '*', Regex),
  atomic_list_concat([ssh,UserMachine,rm,Regex], ' ', Command),
  handle_process(sh, ['-c',Command], [program(Command)]).



%! exists_remote_file(+RemoteFile:compound) is semidet.

exists_remote_file(remote_file(User,Machine,File)):-
  atomic_list_concat([User,Machine], '@', UserMachine),
  atomic_list_concat([ssh,UserMachine,ls,File], ' ', Command),
  handle_process(sh, ['-c',Command], [program(Command)]).



%! make_remote_directory(+RemoteDirectory:compound) is det.

make_remote_directory(remote_file(User,Machine,Dir)):-
  atomic_list_concat([User,Machine], '@', UserMachine),
  atomic_list_concat([ssh,UserMachine,mkdir,Dir], ' ', Command),
  handle_process(sh, ['-c',Command], [program(Command)]).



%! make_remote_directory_path(+RemoteDirectory:compound) is det.

make_remote_directory_path(remote_file(User,Machine,Dir)):-
  atomic_list_concat([User,Machine], '@', UserMachine),
  atomic_list_concat([ssh,UserMachine,mkdir,'-p',Dir], ' ', Command),
  handle_process(sh, ['-c',Command], [program(Command)]).



%! remote_open(
%!   +RemoteFile:compound,
%!   +Mode:oneof([append,read,write]),
%!   -Stream:stream
%! ) is det.

remote_open(RemotePath, Mode, Stream):-
  remote_open(RemotePath, Mode, Stream, []).

%! remote_open(
%!   +RemoteFile:compound,
%!   +Mode:oneof([append,read,write]),
%!   -Stream:stream,
%!   +Options:list(nvpair)
%! ) is det.
% Options are passed to open/4.

remote_open(remote_file(User,Machine,Path), Mode, Stream, Options):-
  atomic_list_concat([User,Machine], '@', UserMachine),
  atomic_concat(Path, '"', Suffix),

  % CAT append uses a double greater than sign.
  once(mode_cat_sign(Mode, CatSign)),
  
  atomic_list_concat([ssh,UserMachine,'"cat',CatSign,Suffix], ' ', Command),
  
  open(pipe(Command), Mode, Stream, Options).

%! mode_cat_sign(
%!   ?Mode:oneof([append,read,write]),
%!   ?Sign:oneof(['<','>','>>'])
%! ) is multi.

mode_cat_sign(append, '>>').
mode_cat_sign(read, '<').
mode_cat_sign(write, '>').

