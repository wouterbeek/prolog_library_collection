:- module(
  error_ext,
  [
    exit_code_reason/2, % ?StatusCode:nonneg
                        % ?Reason:atom
    extract_error/2, % +Error:compound
                     % -PlainError:compound
    handle_catcher/1, % +Catcher:compound
    rethrow/3, % :Goal
               % +Catcher
               % +Exception
% NEW ERRORS
    idle_error/1, % +Reason
    mode_error/2, % +Mode:oneof([det,nondet,semidet])
                  % +Goal:term
    process_error/2 % +Program:atom
                    % +Status:positive_integer
  ]
).
:- reexport(
  library(error),
  [
    domain_error/2, % +Domain
                    % +Term
    existence_error/2, % +Type
                       % +Term
    instantiation_error/1, % +Term
    permission_error/3, % +Action
                        % +Type
                        % +Term
    representation_error/1, % +Reason
    syntax_error/1, % +Culprit
    type_error/2 % +Type
                 % +Term
  ]
).

/** <module> Error extensions

Exception handling predicates.

@author Wouter Beek
@version 2013/01, 2013/12-2014/03, 2014/10
*/

:- use_module(library(debug)).

:- meta_predicate rethrow(0,+,+).



exit_code_reason(1, 'Catchall for general/miscellaneous errors.').
% Status code 1 is seldom seen, and usually defaults to exit code 1.
exit_code_reason(2, 'Misuse for general errors.').
exit_code_reason(126, 'Command cannot be executed. Permission problem or \c
    command is not an executable.').
exit_code_reason(127, 'Command not found.').
exit_code_reason(128, 'Invalid argument to the exit command; \c
    only takes integer args in the range 0-255.').
exit_code_reason(130, 'Script terminated by Control-C.').
exit_code_reason(_, 'Unknown reason').


%! extract_error(+Error:compound, -PlainError:compound) is det.
% Make sure the error terms are of the same form,
% removing the outer functor 'error` when present.

extract_error(error(Type,_), Error):- !,
  compound_name_arity(Type, Error, _).
extract_error(Error, Error).



handle_catcher(exit):- !.
handle_catcher(fail):- !,
  print_message(warning, process_failed_silently).
handle_catcher(!):- !,
  print_message(warning, process_succeeded_with_choicepoints).
handle_catcher(exception(Exception)):- !,
  print_message(warning, Exception).
handle_catcher(external_exception(Exception)):- !,
  print_message(warning, Exception).



%! retrhow(:Goal, +Catcher, +Exception) is det.
% Catches an exception that is thrown lower in the stack, and reappropriates
% it for a new exception, to be caught by another method higher in the stack.
% This is used to provide more detailed ('higher-level') information for
% more generic ('lower-level') exceptions.
%
% Example: =convert_to_jpeg= catches the exception thrown by
% =convert_to_anything= and adds the more specific information that it is a
% conversion to *jpeg* that causes the exception, reusing a generic exception
% for convesions.
%
% @arg Goal
% @arg Catcher
% @arg Exception

rethrow(Goal, Catcher, Exception):-
  catch(Goal, Catcher, throw(Exception)).



% NEW ERRORS %

%! idle_error(+Reason:or([atom,pair(atom,list)])) is det.
% Throws an idle error.

idle_error(Format-Args):- !,
  format(atom(Reason), Format, Args),
  idle_error(Reason).
idle_error(Reason):-
  throw(error(idle_error(Reason), _)).


%! mode_error(+Mode:oneof([det,nondet,semidet]), +Goal:term) is det.
% Throws a mode error.
% This happens when a goal does not have the required mode.

mode_error(Mode, Goal):-
  format(atom(Reason), 'Goal ~k does not have mode ~a.', [Goal,Mode]),
  throw(error(mode_error(Reason), _)).


process_error(Program, Status):-
  throw(
    error(
      process_error(Program,exit(Status)),
      _Context
    )
  ).

