:- module(
  deb_ext,
  [
    test/2, % +Goal:term
            % +Stream
    test/3 % +Goal:term
           % +TestDescription:atom
           % +Stream
  ]
).

/** <module> DEB_EXT

Extensions for debugging and running in debug mode.

Methods that are used while developing and inspecting code.

@author Wouter Beek
@tbd Test this module.
@version 2011/11-2012/07, 2012/09
*/

:- meta_predicate(test(0,+)).
:- meta_predicate(test(0,+,+)).



%! test(:Goal, +Stream) is det.
% Runs the given goal as a test.
%
% @arg Goal A compound term that is the goal that is tested.
% @arg Stream The stream to which the test results are written.
% @see test/3 allows the name of the test to be specified.

test(Goal, Stream):-
  term_to_atom(Goal, TestName),
  test(Goal, TestName, Stream).

%! test(:Goal, +TestName:atom, +Stream) is det.
% Runs a test that is the given goal.
%
% @arg Goal A compound term that is the goal that is tested.
% @arg TestName The name of the test.
% @arg Stream The stream to which the test results are written.

test(Goal, TestName, Stream):-
  get_time(BeginTime),
  catch(
    (
      call(Goal),
      Status = 'OK'
    ;
      Status = 'FAILED'
    ),
    _Catcher,
    Status = 'ERROR'
  ),
  get_time(EndTime),
  DeltaTime is EndTime - BeginTime,
  format(
    Stream,
    'Test: ~w. Status: ~w. Time taken: ~2f.\n',
    [TestName, Status, DeltaTime]
  ),
  flush_output(Stream).
