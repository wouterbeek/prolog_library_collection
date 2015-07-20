:- module(
  replace_in_file,
  [
    replace_in_file/4, % +OldFile:atom
                       % :FromDcg
                       % :ToDcg
                       % ?NewFile
    trim_spaces/2 % +OldFile:atom
                  % ?NewFile:atom
  ]
).

/** <module> Replace in file

Make arbitrary consecutive replacements in text files.

@author Wouter Beek
@version 2013/09, 2014/10-2014/12, 2015/02
*/

:- use_module(library(apply)).
:- use_module(library(debug)).

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_replace)).
:- use_module(plc(generics/atom_ext)). % Meta-option.
:- use_module(plc(generics/code_ext)).
:- use_module(plc(io/file_ext)).
:- use_module(plc(prolog/pl_control)).

:- meta_predicate(replace_in_file(+,//,//,-)).
:- meta_predicate(replace_in_file(+,//,//,?,?)).





%! replace_in_file(+OldFile:atom, :FromDcg, :ToDcg, ?NewFile:atom) is det.

replace_in_file(F1, FromDcg, ToDcg, F2):-
  % Type checks.
  is_absolute_file_name(F1),
  xor(
    % Catch instantiation_error thrown by is_absolute_file(-).
    catch(is_absolute_file_name(F2), _, fail),
    new_file_name(F1, F2)
  ),
  flag(replace_lines, _, 0),
  setup_call_cleanup(
    open(F2, write, Out),
    phrase_from_file(replace_in_file(Out, FromDcg, ToDcg), F1),
    close(Out)
  ).



%! replace_in_file(+Output:stream, :FromDcg, :ToDcg)// is det.

replace_in_file(Out, FromDcg, ToDcg) -->
  '...'(Line1),
  end_of_replace_line, !,
  {
    % Replace line
    once(phrase(dcg_replace(FromDcg, ToDcg), Line1, Line2)),

    % DEB
    (   Line1 \= Line2
    ->  maplist(atom_codes, [Atom1,Atom2], [Line1,Line2]),
        debug(replace_in_file, 'FROM:\t~a\nTO:\t~a\n', [Atom1,Atom2])
    ;   true
    ),

    % Write line
    put_codes(Out, Line2),
    nl(Out),
    flush_output(Out)
  },
  replace_in_file(Out, FromDcg, ToDcg).
replace_in_file(_, _, _) --> [].



%! trim_spaces(+OldFile:atom, ?NewFile:atom) is det.

trim_spaces(F1, F2):-
  replace_in_file(F1, '+'(space, []), space, F2).





% HELPERS %

end_of_replace_line -->
  end_of_line,
  {
    flag(replace_lines, N, N + 1),
    debug(replace_in_file, 'REPLACE-IN-FILE ~D', [N])
  }.
