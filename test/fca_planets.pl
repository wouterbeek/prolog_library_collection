:- module(fca_planets, [planets_fca/0]).

:- use_module(library(aggregate)).
:- use_module(library(fca/fca_export)).
:- use_module(library(os/external_program)).
:- use_module(library(os/pdf)).

:- initialization(list_external_programs).

planets_fca:-
  planets_context(Context),
  fca_export(
    Context,
    File,
    [concept_label(objects),object_label(planet_abbr)]
  ),
  open_pdf(File).

planets_context(context(Os,As,fca_planets:planet_property)):-
  aggregate_all(set(O), planet_property(O, _), Os),
  aggregate_all(set(A), planet_property(_, A), As).

planet_abbr('Earth')   --> "E".
planet_abbr('Jupiter') --> "J".
planet_abbr('Mars')    --> "Ma".
planet_abbr('Mercury') --> "Me".
planet_abbr('Neptune') --> "N".
planet_abbr('Pluto')   --> "P".
planet_abbr('Saturn')  --> "S".
planet_abbr('Uranus')  --> "U".
planet_abbr('Venus')   --> "V".

planet_property('Mercury', size(small)).
planet_property('Mercury', distance_from_sun(near)).
planet_property('Mercury', moon(no)).
planet_property('Venus',   size(small)).
planet_property('Venus',   distance_from_sun(near)).
planet_property('Venus',   moon(no)).
planet_property('Earth',   size(small)).
planet_property('Earth',   distance_from_sun(near)).
planet_property('Earth',   moon(yes)).
planet_property('Mars',    size(small)).
planet_property('Mars',    distance_from_sun(near)).
planet_property('Mars',    moon(yes)).
planet_property('Jupiter', size(large)).
planet_property('Jupiter', distance_from_sun(far)).
planet_property('Jupiter', moon(yes)).
planet_property('Saturn',  size(large)).
planet_property('Saturn',  distance_from_sun(far)).
planet_property('Saturn',  moon(yes)).
planet_property('Uranus',  size(medium)).
planet_property('Uranus',  distance_from_sun(far)).
planet_property('Uranus',  moon(yes)).
planet_property('Neptune', size(medium)).
planet_property('Neptune', distance_from_sun(far)).
planet_property('Neptune', moon(yes)).
planet_property('Pluto',   size(small)).
planet_property('Pluto',   distance_from_sun(far)).
planet_property('Pluto',   moon(yes)).
