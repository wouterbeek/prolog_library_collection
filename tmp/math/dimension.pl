:- module(
  dimension,
  [
    dimension_scale/4 % +Content:list(float)
                      % +Container:list(float)
                      % -Scale:float
                      % -ScaledContent:list(float)
  ]
).

/** <module> Dimension

Operations on multi-dimensional shapes.

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).



%! dimension_scale(
%!   +Content:list(float),
%!   +Container:list(float),
%!   -Scale:float,
%!   -ScaledContent:list(float)
%! ) is det.
% Succeeds if ScaledContent is the maximal multi-dimentional rectangle
% that Content can inhabit inside Container.
%
% @throws domain_error if Content and Container
%         are of different size.
%
% # Example
%
% ```prolog
% ?- dimension_scale([1.0,1.5], [2.5,3.0], Scale, ScaledContent).
% Scale = 2.0,
% ScaledContent = [2.0,3.0].
% ```

dimension_scale(L1, L2, _, _):-
  \+ same_length(L1, L2), !,
  domain_error(equinumerous_lists, [L1,L2]).
dimension_scale(L1, L2, Scale, L3):-
  maplist(div, L1, L2, L0),
  max_list(L0, InvScale),
  findall(
    Y,
    (
      member(X, L1),
      Y is X / InvScale
    ),
    L3
  ),
  Scale is 1 / InvScale.

div(X, Y, Z):- Z is X / Y.
