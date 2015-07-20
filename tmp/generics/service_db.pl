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
@version 2014/08, 2015/05
*/

:- use_module(library(error)).
:- use_module(library(persistency)). % Declarations.

:- use_module(plc(generics/persistent_db_ext)).

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
  absolute_file_name(
    project('service.db'),
    File,
    [access(write),file_type(database)]
  ).



%! service_db_init is det.

service_db_init:-
  service_db_file(File),
  init_persistent_db(File, service_db_update).



%! service_db_update(+Age:nonneg) is det.

service_db_update(_).

