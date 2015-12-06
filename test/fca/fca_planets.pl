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

:- initialization(list_external_programs).





%! fca_planets is det.

fca_planets:-
  planets_context(Context),
  fca_viz(
    Context,
    File,
    [
      attribute_label(planet_a_abbr),
      concept_label(both),
      graph_label("FCA for planets"),
      object_label(planet_o_abbr)
    ]
  ),
  open_pdf(File).



%! planets_concet(-Context:compound) is det.

planets_context(context(Os,As,fca_planets:planet_property)):-
  aggregate_all(set(O), planet_property(O, _), Os),
  aggregate_all(set(A), planet_property(_, A), As).



%! planet_a_abbr(+Attribute:compound)// is det.

planet_a_abbr(T) -->
  {
    T =.. L,
    maplist(atom_codes, L, [[X|_],[Y|_]])
  },
  [X,Y].



%! planet_o_abbr(+Object:atom)// is det.

planet_o_abbr('Earth')   --> "E".
planet_o_abbr('Jupiter') --> "J".
planet_o_abbr('Mars')    --> "Ma".
planet_o_abbr('Mercury') --> "Me".
planet_o_abbr('Neptune') --> "N".
planet_o_abbr('Pluto')   --> "P".
planet_o_abbr('Saturn')  --> "S".
planet_o_abbr('Uranus')  --> "U".
planet_o_abbr('Venus')   --> "V".



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
