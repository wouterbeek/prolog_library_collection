:- module(
  remote_ext,
  [
    make_remote_directory/1 % +RemoteDir
  ]
).

/** <module> Remote file operations

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(os/process_ext)).





make_remote_directory(remote_file(User,Machine,Dir)):-
  atomic_list_concat([User,Machine], '@', UserMachine),
  atomic_list_concat([ssh,UserMachine,mkdir,Dir], ' ', Command),
  run_process(sh, ['-c',Command], [program(Command)]).
