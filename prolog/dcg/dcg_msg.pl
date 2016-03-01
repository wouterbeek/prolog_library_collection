:- module(
  dcg_msg,
  [
    dcg_msg_emphasis/1, % :Dcg_0
    dcg_msg_normal/1, % :Dcg_0
    dcg_msg_notification/1, % :Dcg_0
    dcg_msg_success/1, % :Dcg_0
    dcg_msg_warning/1 % :Dcg_0
  ]
).

/** <module> DCG messages

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(msg_ext)).

:- meta_predicate(dcg_msg_emphasis(//)).
:- meta_predicate(dcg_msg_normal(//)).
:- meta_predicate(dcg_msg_notification(//)).
:- meta_predicate(dcg_msg_success(//)).
:- meta_predicate(dcg_msg_warning(//)).





dcg_msg_emphasis(Dcg_0):-
  string_phrase(Dcg_0, S),
  msg_emphasis(S).



dcg_msg_normal(Dcg_0):-
  string_phrase(Dcg_0, S),
  msg_normal(S).



dcg_msg_notification(Dcg_0):-
  string_phrase(Dcg_0, S),
  msg_notification(S).



dcg_msg_success(Dcg_0):-
  string_phrase(Dcg_0, S),
  msg_success(S).



dcg_msg_warning(Dcg_0):-
  string_phrase(Dcg_0, S),
  msg_warning(S).
