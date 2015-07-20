:- module(
  gnu_plot,
  [
    gnu_plot/3 % +ScriptSpec:compound
               % +Arguments:list(nvpair)
               % +Options:list(nvpair)
  ]
).

/** <module> GNU Plot

Support for calling GNU Plot.

Example
-------

```bash
$ gnuplot -e "input_file='data/2015/04/01/16_33_29.csv';output_dir='data/';" su_plot.plt
```

---

@author Wouter Beek
@version 2015/04
*/

:- use_module(library(dcg/basics)).

:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_quote)).
:- use_module(plc(process/process_ext)).

:- predicate_options(gnu_plot/3, 3, [
  pass_to(handle_process/3, 3)
]).





%! gnu_plot(
%!   +ScriptSpec:compound,
%!   +Arguments:list(nvpair),
%!   +Options:list(nvpair)
%! ) is det.

gnu_plot(ScriptSpec, Args, Options):-
  absolute_file_name(ScriptSpec, Script, [access(execute),extensions([plt])]),
  relative_file(Script, RelScript),
  atom_phrase(gnu_plot_args(Args), Arg),
  handle_process(
    gnuplot,
    ['-e',Arg,file(RelScript)],
    Options
  ).

gnu_plot_args([]) --> "".
gnu_plot_args([K=V|T]) -->
  gnu_plot_arg(K, V),
  gnu_plot_args(T).

gnu_plot_arg(K, V) -->
  atom(K),
  "=\'",
  gnu_plot_value(V),
  "\';".

gnu_plot_value(file(File)) --> !,
  {relative_file(File, RelFile)},
  atom(RelFile).
gnu_plot_value(V) -->
  atom(V).

relative_file(File, RelFile):-
  absolute_file_name(., Dir, [access(read),file_type(directory)]),
  relative_file_name(File, Dir, RelFile).

