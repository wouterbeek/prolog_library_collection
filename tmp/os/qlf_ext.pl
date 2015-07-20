:- module(
  qlf_ext,
  [
    compiled_file/2 % +PlFile:atom
                    % -QlfFile:atom
  ]
).

/** <module> QLF_EXT

Predicates for loading Prolog files in the Quick Load Format (QLF).

@author Wouter Beek
@version 2013/06, 2014/10
*/

:- use_module(library(debug)).

:- use_module(plc(io/file_ext)).





%! compiled_file(+PlFile:atom, -QlfFile:atom) is det.
% Returns the compiled version of the given Prolog file.
%
% Compiled files are called Quick Load Files (QLF).
%
% This method checks whether the Prolog file needs to be recompiled,
% or whether a previously compiled QLF file can be used instead.
%
% @arg PlFile The atomic name of a Prolog file.
% @arg QlfFile The atomic name of a QLF file.

compiled_file(PlFile, QlfFile):-
  file_alternative(PlFile, _, _, quick_load_file, QlfFile),
  (   exists_file(QlfFile),
      younger_file(QlfFile, PlFile)
  ->  true
  ;   access_file(QlfFile, write)
  ->  qcompile(PlFile)
  ;   print_message(informational, cannot_write_qlf(PlFile))
  ).



% MESSAGES

:- multifile(prolog:message//1).

prolog:message(cannot_write_qlf(PlFile)) -->
  ['Cannot write to QLF, loading from ',PlFile,'.'].
