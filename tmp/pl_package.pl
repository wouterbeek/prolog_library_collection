:- module(
  pl_package,
  [
    load_pl_package/1 % +Pack:atom
  ]
).

/** <module> Prolog package

Support for SWI-Prolog packages.

@author Wouter Beek
@see http://www.swi-prolog.org/pack/list
@version 2014/03-2014/04
*/

:- use_module(library(prolog_pack)).



%! load_pl_package(+Pack:atom) is det.

load_pl_package(Pack):-
  catch(
    use_module(library(Pack)),
    _,
    (
      pack_install(Pack),
      use_module(library(Pack))
    )
  ).

