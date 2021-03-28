:- module(
  cli_version,
  [
    cli_version/0
  ]
).

/** <module> Command-line tools: version message

*/

:- dynamic
    name/1,
    title/1,
    version/1.



%! cli_version is det.

cli_version :-
  % Requires that the CLI tool was run from a directory containing a
  % pack.pl metadata file.
  exists_file('pack.pl'),
  consult(pack),
  name(Name),
  title(Title),
  version(Version),
  format(user_output, "~a: ~a (~a)\n", [Name,Title,Version]).
