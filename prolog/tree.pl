:- module(
  tree,
  [
    depth/2,   % +Tree, ?Depth
    shortest/2 % +Trees, ?Tree
  ]
).

/** <module> Tree data structure support
 *
 * Support library for working with tree data structures.
 *
 * A tree is represented by a compound term with the following components:
 *
 *   - `Label` :: A (compound) term that describes the relationship between
 *                the `Parent` node and the `Children` subtrees.
 *   - `Parent` :: A (compound) term that represents the parent node.
 *   - `Children` :: A list of trees.  Empty if `Parent` is a leaf node.
 *
 * Here are some examples of tree terms:
 *
 * ```pl
 * tree(leaf, 'Electra', [])
 *
 * tree(father,
 *      'Agamemnon',
 *      [tree(leaf, 'Electra', []),
 *       tree(leaf, 'Orestes', [])])
 *
 * tree('Modus ponens',
 *      'Socrates is mortal',
 *      [tree(premise, 'Sorcrates is a man', []),
 *       tree(premise, 'Men are mortal', [])])
 * ```
 *
 * @author Wouter Beek
 * @version 2021-03-18
 */

:- use_module(library(aggregate)).
:- use_module(library(lists)).



%! depth(+Tree:compound, +Depth:nonneg) is semidet.
%! depth(+Tree:compound, -Depth:nonneg) is det.
%
% Succeeds for the Depth of the Tree.
%
% The depth of a tree is defined inductively:
%   - 0 for leaf nodes.
%   - 1 plus the maxium of the depth of the children.

depth(tree(_,_,[]), 0).
depth(tree(_,_,Trees), Depth) :-
  aggregate_all(
    max(Depth),
    (
      member(Tree, Trees),
      depth(Tree, Depth)
    ),
    Depth
  ).



%! shortest(+Trees:list(compound), +Tree:compound) is semidet.
%! shortest(+Trees:list(compound), -Tree:compound) is det.
%
% Succeeds if Tree is the shotests of Trees.

shortest(Trees, Tree) :-
  aggregate_all(
    min(Depth0,Tree0),
    (
      member(Tree0, Trees),
      depth(Tree0, Depth0)
    ),
    min(_, Tree)
  ).
