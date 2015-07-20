:- module(
  statistics,
  [
    t1_error/3, % +Set1:ordset
                % +Set2:ordset
                % -T1_Error:integer
    t2_error/3 % +Set1:ordset
               % +Set2:ordset
               % -T2_Error:integer
  ]
).

/** <module> STATISTICS

Statics predicates.

@author Wouter Beek
@version 2013/04
*/

:- use_module(library(ordsets)).



%! t1_error(+True:ordset, +Verified:ordset, T1_Error:integer) is det.
% The number of cases in which the hypothesis is actually false but
% is said to be true.
%
% # Synonyms
%     * false alarm.
%     * false positive error
%     * asserting something that is absent
%     * false hit


t1_error(HypothesisTrue, HypothesisVerified, T1_Error):-
  ord_subtract(HypothesisVerified, HypothesisTrue, X),
  length(X, T1_Error).

%! t2_error(+True:ordset, +Verified:ordset, T2_Error:integer) is det.
% The number of cases in which the hypothesis is actually true but
% is said to be false.
%
% # Synonyms
%     * false negative error
%     * failing to assert what is present
%     * miss

t2_error(HypothesisTrue, HypothesisVerified, T2_Error):-
  ord_subtract(HypothesisTrue, HypothesisVerified, X),
  length(X, T2_Error).

