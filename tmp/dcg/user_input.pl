:- module(
  user_input,
  [
    input_directory_name/3, % +Message:string
                            % +Local:atom
                            % -Path:atom
    input_file_path_name/3, % +Message:string
                            % +PrefixPath:atom
                            % -Path:atom
    input_local_file_name/3, % +Message:string
                             % +PrefixDir:atom
                             % -Path:atom
    input_password/2, % +Message:string
                      % -UnencryptedPassword:list(code)
    user_input/3, % +Message:atom
                  % :LegalAnswer
                  % -Answer
    user_interaction/5 % +Action:atom
                       % :Goal
                       % +Headers:list(atom)
                       % +Tuples:list(list)
                       % +Options:list(nvpair)
  ]
).

/** <module> User input

Handles user input and sequences in which user input is needed continuously
(called "user interaction").

@author Wouter Beek
@version 2013/10-2013/12, 2014/11-2014/12
*/

:- use_module(library(filesex)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(option)).
:- use_module(library(readutil)).

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_meta)).
:- use_module(plc(dcg/dcg_unicode)).
:- use_module(plc(generics/string_ext)).
:- use_module(plc(io/file_ext)).

:- meta_predicate(user_input(+,3,+)).
:- meta_predicate(user_interaction(+,+,:,+,+)).
:- meta_predicate(user_interaction(+,+,:,+,+,+,+)).
:- meta_predicate(user_interaction(+,+,+,:,+,+,+,+)).





%! input_directory_name(+Message:string, +Local:atom, -Path:atom) is det.
% Allow the user to specify a directory for the given local file name.

input_directory_name(Msg0, Local, Path):-
  format(
    string(Msg),
    '~s\nEnter the directory holding file ~w.',
    [Msg0,Local]
  ),
  repeat,
  user_input(Msg, input_directory_name, Dir),
  (   directory_file_path(Dir, Local, Path)
  ->  !
  ;   fail
  ).



%! input_file_path_name(+Message:string, +Prefix:atom, -Path:atom) is det.
% Allows the user to enter a path relative to the given directory prefix.
% The user can specify a local file name with or without
% prefixed subdirectories.
%
% Prefix is an absolute path representing a directory.

input_file_path_name(Msg, Prefix, Path):-
  repeat,
  user_input(Msg, input_file_path_name, Postfix),

  % Postfix is not allowed to contain `..` segments since
  % Path could end up ouside of Prefix.
  (   sub_atom(Postfix, _, 2, _, '..')
  ->  fail
  ;   !
  ),

  relative_file_path(Path, Prefix, Postfix).



%! input_local_file_name(+Message:string, +Directory:atom, -Path:atom) is det.
% Allows the user to enter a path relative to the given Directory.
% The user can ony specify the local file name (i.e., no subdirectories).

input_local_file_name(Msg, Dir, Path):-
  repeat,
  user_input(Msg, input_local_file_name, Local),
  (   directory_file_path(Dir, Local, Path)
  ->  !
  ;   fail
  ).



%! input_password(+Message:string, -UnencryptedPassword:list(nonneg)) is det.

input_password(Msg0, UnencryptedPassword):-
  format(
    string(Msg),
    '~s\nThe input_password must consist of 7 or more ASCII graphic characters.',
    [Msg0]
  ),
  user_input(Msg, input_password, UnencryptedPassword).

input_password(Codes) -->
  'm*'(7, graphic, Codes, []).



%! user_input(+Message:atom, :LegalAnswer, -Answer) is det.

user_input(Msg, Legal, Answer):-
  repeat,
  format(user_output, '~w\n', [Msg]),
  read_line_to_codes(user_input, Codes),
  (   once(phrase(dcg_call_cp(Legal, Answer), Codes))
  ->  !
  ;   fail
  ).



%! user_interaction(
%!   +ActionDescription:string,
%!   :Goal,
%!   +Headers:list(atom),
%!   +Tuples:list,
%!   +Options:list(nvpair)
%! ) is det.
% The generic predicate for executing arbitray Prolog goals for arbitrary
% sequences of Prolog terms under user-interaction.
%
% One of the use cases is cleaning a database, where a list of =Tuples=
% has been identified for removal by =Goal=, but a user is required to
% assent to each removal action.
%
% Receiving input from the user does not work in threads!
%
% The following options are supported:
%   * `answer(+Answer:oneof(['A',n,q,y]]))`
%
% @arg ActionDescription A string describing the action performed by Goal.
% @arg Goal An arbitrary Prolog goal that takes the number of elements
%      in each tuple as the number of arguments.
% @arg Headers A list of atoms describing the entries in each tuple.
%      The number of headers and the number of elements in each
%      tuple are assumed to be the same.
% @arg Tuples A list of tuples. These are the element lists for which goal
%      is executed after user-confirmation.
% @arg Options A list of name-value pairs.

user_interaction(Act, G, Hs, Ts, Options):-
  length(Ts, NumberOfTs),
  user_interaction(Act, G, 1, NumberOfTs, Hs, Ts, Options).


%! user_interaction(
%!   +ActionDescription:string,
%!   :Goal,
%!   +IndexOfTuple:positive_integer,
%!   +NumberOfTuples:positive_integer,
%!   +Headers:list(atom),
%!   +Tuples:list,
%!   +Options:list(nvpair)
%! ) is det.
% The following options are supported:
%   * `answer(+Answer:oneof(['A',n,q,y]]))`

user_interaction(Act, _, _, _, _, [], _):-
  format(user_output, '\n-----\nDONE! <~s>\n-----\n', [Act]), !.
user_interaction(Act, G, I, L, Hs, Ts, Options):-
  option(answer(UserAtom), Options), !,
  user_interaction(UserAtom, Act, G, I, L, Hs, Ts, Options).
user_interaction(Act, G, I, L, Hs, Ts, Options):-
  % Construct the message.
  nth1(I, Ts, T),
  findall(
    HeaderedElement,
    (
      nth0(J, Hs, H),
      nth0(J, T, Element),
      format(string(HeaderedElement), '~w: <~w>', [H,Element])
    ),
    HeaderedElements
  ),
  string_list_concat(HeaderedElements, '\n\t', MsgTail),
  format(string(Msg), '[~w/~w] ~s\n\t~w\n(y/n/q)\n?: ', [I,L,Act,MsgTail]),

  % Ask for legal user input.
  user_input(Msg, legal_user_interaction, Answer),

  % Execute the goal based on the user input.
  user_interaction(Answer, Act, G, I, L, Hs, Ts, Options).


%! user_interaction(
%!   +Answer:oneof(['A',n,q,y]),
%!   +ActionDescription:string,
%!   :Goal,
%!   +IndexOfTuple:positive_integer,
%!   +NumberOfTuples:positive_integer,
%!   +Headers:list(atom),
%!   +Tuples:list,
%!   +Options:list(nvpair)
%! ) is det.

% User choice: all.
user_interaction('A', _, G, I1, L, _, Ts, _):- !,
  forall(
    between(I1, L, J),
    (
      nth1(J, Ts, Juple),
      apply(G, Juple)
    )
  ).
% User choice: no.
user_interaction(n, Act, G, I1, L, Hs, Ts, Options):- !,
  I2 is I1 + 1,
  user_interaction(Act, G, I2, L, Hs, Ts, Options).
% User choice: quit.
user_interaction(q, _, _, _, _, _, _, _):- !.
% User choice: yes.
user_interaction(y, Act, G, I1, L, Hs, Ts, Options):- !,
  nth1(I1, Ts, T),
  apply(G, T),
  I2 is I1 + 1,
  user_interaction(Act, G, I2, L, Hs, Ts, Options).





% HELPERS %

%! user_interaction(-LegalUserInput:char)// is semidet.

legal_user_interaction(Char) -->
  (   a_uppercase(Code)
  ;   n_lowercase(Code)
  ;   q_lowercase(Code)
  ;   y_lowercase(Code)
  ),
  {char_code(Char, Code)}.
