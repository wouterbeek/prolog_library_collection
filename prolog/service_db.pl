:- module(
  service_db,
  [
    register_service/2, % +Service:term
                        % +Term:term
    service/2 % ?Service:term
              % ?Term:term
  ]
).

/** <module> Service DB

Persistent store for user+password registrations for services.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(error)).
:- use_module(library(os/file_ext)).
:- use_module(library(persistency)).

%! service(?Service:term, ?Term:term) is nondet.

:- persistent(service(service:term,term:term)).

:- initialization(service_db_init).





%! register_service(+Service:term, +Term:term) is det.
% Registers a user to the persistent user database.
%
% Succeeds without change in case the exact same registration already exists.

register_service(Service, Term):-
  service(Service, Term), !.
register_service(Service, Term):-
  assert_service(Service, Term).





% INITIALIZATION %

%! service_db_file(-File:atom) is det.

service_db_file(File):-
  absolute_file_name(service, File, [access(write),file_type(database)]).


%! service_db_init is det.

service_db_init:-
  service_db_file(File),
  (   exists_file(File)
  ->  true
  ;   touch(File)
  ),
  db_attach(File, []).
