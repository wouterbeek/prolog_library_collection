:- module(
  exception_handling,
  [
    rethrow/3 % :Goal
              % +Catcher
              % +Exception
  ]
).

/** <module> Exception handling

Exception handling predicates.

@author Wouter Beek
@version 2013/01
*/

:- meta_predicate rethrow(0,+,+).



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
