:- module(
  user_input,
  [
    input_directory_name/3,  % +Msg, +Local, -Path
    input_file_path_name/3,  % +Msg, +PrefixPath, -Path
    input_local_file_name/3, % +Msg, +PrefixDir, -Path
    input_password/2,        % +Msg, -UnencryptedPassword:list(code)
    user_input/3,            % +Msg, :LegalAnswer_0, -Answer
    user_interaction/5       % +Action, :Goal_0, +Headers, +Tuples, +Opts
  ]
).

/** <module> User input

Handles user input and sequences in which user input is needed
continuously (i.e., user interaction).

@author Wouter Beek
@version 2013/10-2013/12, 2014/11-2014/12, 2016/09
*/

:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(readutil)).

:- meta_predicate
    user_input(+, 3, +),
    user_interaction(+, +, :, +, +),
    user_interaction(+, +, :, +, +, +, +),
    user_interaction(+, +, +, :, +, +, +, +).





%! input_directory_name(+Msg, +Local, -Path) is det.
%
% Allow the user to specify a directory for the given local file name.

input_directory_name(Msg0, Local, Path) :-
  format(
    string(Msg),
    "~s~nEnter the directory holding file ‘~a’.",
    [Msg0,Local]
  ),
  repeat,
  user_input(Msg, input_directory_name, Dir),
  (   directory_file_path(Dir, Local, Path)
  ->  !
  ;   fail
  ).



%! input_file_path_name(+Msg, +Prefix, -Path) is det.
%
% Allows the user to enter a path relative to the given directory
% prefix.  The user can specify a local file name with or without
% prefixed subdirectories.
%
% Prefix is an absolute path representing a directory.

input_file_path_name(Msg, Prefix, Path) :-
  repeat,
  user_input(Msg, input_file_path_name, Postfix),
  % Postfix is not allowed to contain `..` segments since Path could
  % end up ouside of Prefix.
  (   sub_atom(Postfix, _, 2, _, '..')
  ->  fail
  ;   !
  ),
  relative_file_name(Path, Prefix, Postfix).



%! input_local_file_name(+Msg, +Dir, -Path) is det.
%
% Allows the user to enter a path relative to the given Directory.
% The user can ony specify the local file name (i.e., no
% subdirectories).

input_local_file_name(Msg, Dir, Path) :-
  repeat,
  user_input(Msg, input_local_file_name, Local),
  (   directory_file_path(Dir, Local, Path)
  ->  !
  ;   fail
  ).



%! input_password(+Msg, -UnencryptedPassword:list(code)) is det.

input_password(Msg0, UnencryptedPassword) :-
  format(
    string(Msg),
    "~s~nThe password must consist of 7 or more ASCII graphic characters.",
    [Msg0]
  ),
  user_input(Msg, input_password, UnencryptedPassword).


input_password(Cs) -->
  'm*'(7, graphic, Cs, []).



%! user_interaction(+ActionDesc, :Goal_0, +Headers, +Tuples, +Opts) is det.
%! user_interaction(
%!   +ActionDesc,
%!   :Goal_0,
%!   +TupleIndex,
%!   +NumTuples,
%!   +Headers,
%!   +Tuples,
%!   +Opts
%! ) is det.
%
% The generic predicate for executing arbitray Prolog goals for
% arbitrary sequences of Prolog terms under user-interaction.
%
% One of the use cases is cleaning a database, where a list of
% `Tuples` has been identified for removal by Goal_0, but a user is
% required to assent to each removal action.
%
% Receiving input from the user does not work in threads!
%
% The following options are supported:
%
%   * answer(+oneof(['A',n,q,y]]))
%
% @arg ActionDesc A string describing the action performed by Goal_0.
%
% @arg Goal An arbitrary Prolog goal that takes the number of elements
%      in each tuple as the number of arguments.
%
% @arg Headers A list of atoms describing the entries in each tuple.
%      The number of headers and the number of elements in each tuple
%      are assumed to be the same.
%
% @arg Tuples A list of tuples. These are the element lists for which
%      Goal_0 is executed after user-confirmation.

user_interaction(ActDesc, G, Hs, Tuples, Opts) :-
  length(Tuples, NumTuples),
  user_interaction(ActDesc, G, 1, NumTuples, Hs, Tuples, Opts).


user_interaction(Act, _, _, _, _, [], _) :-
  format(user_output, "~n-----~nDONE! <~s>~n-----~n", [Act]), !.
user_interaction(Act, G, I, L, Hs, Ts, Opts) :-
  option(answer(UserAtom), Opts), !,
  user_interaction(UserAtom, Act, G, I, L, Hs, Ts, Opts).
user_interaction(Act, G, I, L, Hs, Ts, Opts) :-
  % Construct the message.
  nth1(I, Ts, T),
  findall(
    HeaderedElement,
    (
      nth0(J, Hs, H),
      nth0(J, T, Element),
      format(string(HeaderedElement), "~w: <~w>", [H,Element])
    ),
    HeaderedElements
  ),
  atomics_to_string(HeaderedElements, "~n~t", MsgTail),
  format(string(Msg), "[~w/~w] ~s~n~t~w~n(y/n/q)~n?: ", [I,L,Act,MsgTail]),

  % Ask for legal user input.
  user_input(Msg, legal_user_interaction, Answer),

  % Execute the goal based on the user input.
  user_interaction(Answer, Act, G, I, L, Hs, Ts, Opts).


%! user_interaction(
%!   +Answer:oneof(['A',n,q,y]),
%!   +ActionDescription:string,
%!   :Goal,
%!   +IndexOfTuple:positive_integer,
%!   +NumberOfTuples:positive_integer,
%!   +Headers:list(atom),
%!   +Tuples:list,
%!   +Opts
%! ) is det.

% User choice: all.
user_interaction('A', _, G, I1, L, _, Ts, _) :- !,
  forall(
    between(I1, L, J),
    (
      nth1(J, Ts, Juple),
      apply(G, Juple)
    )
  ).
% User choice: no.
user_interaction(n, Act, G, I1, L, Hs, Ts, Opts) :- !,
  I2 is I1 + 1,
  user_interaction(Act, G, I2, L, Hs, Ts, Opts).
% User choice: quit.
user_interaction(q, _, _, _, _, _, _, _) :- !.
% User choice: yes.
user_interaction(y, Act, G, I1, L, Hs, Ts, Opts) :- !,
  nth1(I1, Ts, T),
  apply(G, T),
  I2 is I1 + 1,
  user_interaction(Act, G, I2, L, Hs, Ts, Opts).





% HELPERS %

%! user_interaction(-LegalUserInput:char)// is semidet.

legal_user_interaction(Char) -->
  (   a_uppercase(Code)
  ;   n_lowercase(Code)
  ;   q_lowercase(Code)
  ;   y_lowercase(Code)
  ),
  {char_code(Char, Code)}.
