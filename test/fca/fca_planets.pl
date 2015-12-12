:- module(
  fca_planets,
  [
    fca_planets/0
  ]
).

/** FCA planets example

@author Wouter Beek
@version 2015/11-2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(fca/fca_viz)).
:- use_module(library(os/external_program)).
:- use_module(library(os/pdf)).
:- use_module(library(solution_sequences)).

:- initialization(list_external_programs).





%! fca_planets is det.

fca_planets:-
  Context = context(
    fca_planets:planet_object,
    fca_planets:planet_attribute,
    fca_planets:planet_property
  ),
  fca_viz(
    Context,
    File,
    [
      attribute_label(planet_attribute_label),
      concept_label(both),
      graph_label("FCA for planets"),
      object_label(planet_object_label)
    ]
  ),
  open_pdf(File).



%! planet_attribute(-Object:atom) is multi.

planet_attribute(A):- distinct(A, planet_property(_, A)).



%! planet_attribute_label(+Attribute:compound)// is det.

planet_attribute_label(T) -->
  {T =.. L, maplist(atom_codes, L, [[X|_],[Y|_]])},
  [X,Y].



%! planet_object(-Object:atom) is multi.

planet_object(O):- distinct(O, planet_property(O, _)).



%! planet_object_label(+Object:atom)// is det.

planet_object_label('Earth')   --> "E".
planet_object_label('Jupiter') --> "J".
planet_object_label('Mars')    --> "Ma".
planet_object_label('Mercury') --> "Me".
planet_object_label('Neptune') --> "N".
planet_object_label('Pluto')   --> "P".
planet_object_label('Saturn')  --> "S".
planet_object_label('Uranus')  --> "U".
planet_object_label('Venus')   --> "V".



%! planet_property(?Object:atom, ?Attribute:compound) is nondet.

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
