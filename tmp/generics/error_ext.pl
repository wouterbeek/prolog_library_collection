:- module(
  error_ext,
  [
    exit_code_reason/2, % ?StatusCode:nonneg
                        % ?Reason:atom
    rethrow/3 % :Goal
              % +Catcher
              % +Exception
  ]
).

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



%! retrhow(:Goal_0, +Catcher, +Exception:compound) is det.
% Catches an exception that is thrown lower in the stack, and reappropriates
% it for a new exception, to be caught by another method higher in the stack.
% This is used to provide more detailed ('higher-level') information for
% more generic ('lower-level') exceptions.
%
% Example: =convert_to_jpeg= catches the exception thrown by
% =convert_to_anything= and adds the more specific information that it is a
% conversion to *jpeg* that causes the exception, reusing a generic exception
% for convesions.

rethrow(Goal_0, Catcher, E):-
  catch(Goal_0, Catcher, throw(E)).
