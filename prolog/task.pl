:- module(
  task,
  [
    run_task/1, % :Goal_0
    run_task/2, % :Goal_0, +Format
    run_task/3  % :Goal_0, +Format, +Args
  ]
).
:- reexport(library(debug)).

/** <module> Debug extensions

@author Wouter Beek
@version 2017/08-2017/09
*/

:- use_module(library(ansi_term)).
:- use_module(library(file_ext)).

:- meta_predicate
    run_task(0),
    run_task(0, +),
    run_task(0, +, +).





%! run_task(:Goal_0) is det.
%! run_task(:Goal_0, +Format:string) is det.
%! run_task(:Goal_0, +Format:string, +Args:list) is det.
%
% Verbose calling of Goal_0.

run_task(Mod:Goal_0) :-
  term_string(Goal_0, Format),
  run_task(Mod:Goal_0, Format).


run_task(Goal_0, Format) :-
  run_task(Goal_0, Format, []).


run_task(Mod:Goal_0, Format, Args) :-
  format(atom(Local), "~w", [Goal_0]),
  file_name_extension(Local, task, File),
  (   exists_file(File)
  ->  ansi_format([fg(green)], "Task was already performed earlier: ~a\n",
                  [Local])
  ;   get_time(Start),
      format(Format, Args),
      (   catch(Mod:Goal_0, E, true)
      ->  (   var(E)
          ->  get_time(End),
              Delta is End - Start,
              ansi_format([fg(green)], "~`.t success (~2f sec.)~72|", [Delta])
          ;   message_to_string(E, S),
              ansi_format([fg(red)], "~`.t ERROR: ~w~72|", [S])
          )
      ;   ansi_format([fg(red)], "~`.t ERROR: (failed)~72|", [])
      ),
      nl,
      touch(File)
  ).
