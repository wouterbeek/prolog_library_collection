:- module(
  proof,
  [
    proof_depth/2,   % +Proof, ?Depth
    shortest_proof/3 % +Proof1, +Proof2, -ShortestProof
  ]
).

/** <module> Predicates for working with proofs

*/

:- use_module(library(apply)).
:- use_module(library(lists)).





%! proof_depth(+Proof:compound, -Depth:nonneg) is det.

proof_depth(p(_,_,[]), 0) :- !.
proof_depth(p(_,_,Proofs), Depth) :-
  maplist(proof_depth, Proofs, Depths),
  max_list(Depths, Depth0),
  Depth is Depth0 + 1.



%! shortest_proof(+Proof1:compound, +Proof2:compound, -ShortestProof:compound) is det.

shortest_proof(Proof1, Proof2, Proof) :-
  maplist(proof_depth, [Proof1,Proof2], [Depth1,Depth2]),
  (Depth2 < Depth1 -> Proof = Proof2 ; Proof = Proof1).
