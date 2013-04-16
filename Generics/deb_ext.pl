:- module(
  deb_ext,
  [
    test/2, % +Goal:term
            % +Stream
    test/3, % +Goal:term
            % +TestDescription:atom
            % +Stream
    write_attribute_value_pair/3 % +Stream
                                 % +Attribute:atom
                                 % +Value:atom
  ]
).

/** <module> Extensions for debugging and running in debug mode.

Methods that are used while developing and inspecting code.

@author Wouter Beek
@version 2011/11-2012/07, 2012/09
*/

:- meta_predicate(test(0,+)).
:- meta_predicate(test(0,+,+)).



%% test(:Goal, +Stream) is det.
% Runs the given goal as a test.
%
% @param Goal A compound term that is the goal that is tested.
% @param Stream The stream to which the test results are written.
% @see test/3 allows the name of the test to be specified.

test(Goal, Stream):-
  term_to_atom(Goal, TestName),
  test(Goal, TestName, Stream).

%% test(:Goal, +TestName:atom, +Stream) is det.
% Runs a test that is the given goal.
%
% @param Goal A compound term that is the goal that is tested.
% @param TestName The name of the test.
% @param Stream The stream to which the test results are written.

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

%% write_attribute_value_pair(+Stream, +Attribute:term, +Value:term) is det.
% Writes and attribute/value-pair to the given stream.
%
% @param Stream A stream.
% @param Attribute A term.
% @param Value A term.

write_attribute_value_pair(Stream, Attribute, Value):-
  format(Stream, '~w = ~w\n', [Attribute, Value]),
  flush_output(Stream).

