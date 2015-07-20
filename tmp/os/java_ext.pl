:- module(
  java_ext,
  [
    run_jar/3 % +Jar:atom
              % +Args:list
              % +Options:list(nvpair)
  ]
).

/** <module> Java extensions

Extensions for executing Java JARs from within Prolog.

@author Wouter Beek
@version 2014/12-2015/01
*/

:- use_module(library(option)).

:- use_module(plc(generics/db_ext)).
:- use_module(plc(process/process_ext)).

:- db_add_novel(user:prolog_file_type(jar, jar)).
:- db_add_novel(user:prolog_file_type(log, log)).

:- predicate_options(run_jar/3, 3, [
  pass_to(handle_process/3, 3)
]).

:- meta_predicate(run_jar(+,+,:)).

is_meta(output_goal).





%! run_jar(+Jar:atom, +Args:list, +Options:list(nvpair)) is det.
% Runs the given JAR file with the given commandline arguments.
%
% Options are passed to handle_process/3.

run_jar(Jar, Args, Options1):-
  meta_options(is_meta, Options1, Options2),

  % Set the program option.
  file_base_name(Jar, JarName),
  format(atom(Program), 'Java/JAR ~a', [JarName]),
  merge_options([program(Program)], Options2, Options3),

  handle_process(java, ['-jar',file(Jar)|Args], Options3).

