:- module(
  dcg_replace,
  [
    dcg_replace//2 % :FromDCG
                   % :ToDCG
  ]
).

/** <module> DCG replace

DCG rules for replacing content.

# Discussion on dcg_replace//2

```
Is it possible to use DCG pushback lists in order to replace occurrences of an arbitrary DCG rule X by occurrences of an arbitrary DCG rule Y?
<wbeek> dcg_replace(_, _, [], []):- !.
<wbeek> dcg_replace(FromDCG, ToDCG), ToDCG -->
<wbeek>   FromDCG, !,
<wbeek>   dcg_replace(FromDCG, ToDCG).
<wbeek> dcg_replace(FromDCG, ToDCG), [Code] -->
<wbeek>   [Code],
<wbeek>   dcg_replace(FromDCG, ToDCG).
<wbeek> The above predicate does not have tail recursion. Is it possible to fix this?
<wbeek> The listing is as follows:
<wbeek> dcg_replace(_, _, [], []):- !.
<wbeek> dcg_replace(A, D, B, F):-
<wbeek>  phrase(A, B, C), !,
<wbeek>  E=C,
<wbeek>  dcg_replace(A, D, E, G),
<wbeek>  phrase(D, F, G).
<wbeek> dcg_replace(B, C, A, E):-
<wbeek>  A=[F|D],
<wbeek>  dcg_replace(B, C, D, G),
<wbeek>  E=[F|G].
<wbeek> A nice application for this predicate would be phrase(dcg_replace(iso_dataTime, rfc_dataTime), ISO_Codes, RFC_Codes)
<RLa> i guess you could code dcg_replace manually
<RLa> in the second clause the order of last 2 calls is for steadfastness?
<ski> i think the problem is that the DCG expander (i assume you're using the one in SWI) assumes worst case, iow that the translated calls may not be steadfast, and so can't move the output unification earlier
<ski> either that, or they haven't worked so hard with making it output more performant code
<wbeek> @ski: I'm indeed assuming SWI DCG expansion
<ski> wbeek : also, IRC custom is to direct messages to users by (usually) starting the message with their nickname (no leading `@'), followed by e.g. a colon or a comma, and the rest of the message :)
<wbeek> Thanks RLa and ski, if I understand correctly expansion could produce a version that uses tail recursion, provided the expander knows that `ToDCG` is steadfast.
<ski> (some IRC clients highlight/alert when your nickname is being mentioned. some only highlight when it's being mentioned at the start of a message)
<wbeek> thanks
<ski> wbeek : no, it's rather the recursive call in the last clause which it's (presumably) treating as if it might be non-steadfast
* moriarty has quit (Ping timeout: 246 seconds)
<wbeek> ski : I don't see immediately why in the last clause the line `E=[F|G]` is needed, since `E` and `G` must be lists.
<wbeek> Is there an example of a non-steadfast substitution for `C` that would indeed cause a problem here?
* michael_lee (~michael_l@222.90.49.113) has joined ##prolog
<ski> hm, i don't follow "I don't see immediately why in the last clause the line `E=[F|G]` is needed, since `E` and `G` must be lists"
<ski> for non-steadfastness, assume that `E' is given in the last clause, and that, somehow, the recursive call there weren't steadfast. then moving `E=[F|G]' before the recursive call would provide the recursive call with more information (and so non-steadfastness might get us into trouble)
<ski> of course, one could argue in general about whether it's the "provide more info" or "provide less info" that's the proper use of a given non-steadfast predicate
* michael_lee has quit (Ping timeout: 260 seconds)
<ski> however, the last component of the DCG accumulator pair is output in the parsing mode. it might be either output or input in the serializing mode, but for most internal calls (namely the non-last ones), it will be output here as well
<ski> and providing more info than expected in an "output" argument is exactly the usual non-steadfastness pitfall
<wbeek> ski : ok, now I understand
* edoop has quit (Quit: This computer has gone to sleep)
* michael_lee (~michael_l@222.90.49.113) has joined ##prolog
* like-a-boss has quit (Quit: leaving)
<ski> generally, we'd prefer to move the pushback before the last call, due to last-call-optimization concerns, as you say
<ski> but it seems hard in the general presense of non-steadfastness
* like-a-boss (~nekro@89-64-212-152.dynamic.chello.pl) has joined ##prolog
<ski> perhaps one could change the semantics of pushbash lists, and declare that its unification will happen before the last call, possibly even before any calls in the body (so that if you want it to happen after the last call, you have to explicitly add an auxilary call after it. that wouldn't be hard)
<ski> (maybe placing it just before the last call would be nicer. but then one'd need to decide how to handle `p,[a] --> ( q,r ; s ).' e.g.)
<ski> also, with the SWI DCG feature of allowing a variable pushback goal (`ToDCG' above), one could ask whether that should also be moved then (and to the start, or just before the last call)  ? or only unifications get moved ?
<ski> i suppose since the main point is to allow the pushback goal to serialize out some tokens onto the penultimate accumulator state, one might argue for placing it almost anywhere, assuming it doesn't depend on variables instantiated by other goals
<nlogn> i;i "serialize out some tokens onto the penultimate accumulator" ;)
<ski> (technically such a serialization could inspect the accumulator state which it's to prepend tokens onto, but at least for parsing/serialization, i think this would never or almost never happen. if you (ab)use DCG for pushdown automata or something, it might perhaps happen)
<ski> nlogn : yes ? ;)
<nlogn> ski: I just think it's a funny thing to say out of context in my simple mind.
<nlogn> :)
<ski> anyway, i think putting the pushback goal last would correspond to least surprise. moving the unification could be viewed as an optimization
<ski> (moving the pushback goal might either improve or worsen the performance, depending on the particular code in question)
<ski> nlogn : i like dragging in sometimes ususual words and phrases in my sentences :)
* schlaftier has quit (Quit: Leaving)
<nlogn> ski: You are eloquent :)
<ski> wbeek : anyway, i suppose a conclusion to my ramblings might be that i wouldn't be opposed to considering changing the details of when a pushback list gets processed
<nlogn> You might be able to just paste from IRC to an editor and have a book. :)
* nlogn re-lurks
<ski> in fact, it might in some cases be really nice if the pushback list unification got done in the head, since then you could possibly run some computations backwards
<ski> (pushback general goals being another matter)
<ski> btw, i'm somewhat surprised to see the DCG transpiler residualized `E=C' above, instead of executing it statically, replacing `E' and `C' with a common variable
<wbeek> ski : not ramblings at all. I find both your initial explanation and your subsequent discussion very useful.
<wbeek> ski : unification in the head would probably result in the following:
<wbeek> dcg_replace(_, _, [], []):- !.
<wbeek> dcg_replace(A, D, B, F):-
<wbeek>  phrase(A, B, C), !,
<wbeek>  dcg_replace(A, D, C, G),
<wbeek>  phrase(D, F, G).
<wbeek> dcg_replace(B, C, [F|D], [F|G]):-
<wbeek>  dcg_replace(B, C, D, G).
<ski> (later compiler phases might remove such goals anyway, but iirc it already goes to some effort to avoid generating such needless trivial goals, so why not here as well ? mayhaps i misremember of the "already goes to some effort")
<ski> wbeek : yea
<ski> btw, regarding the predicate itself, it's not clear to me why you have included the first clause
* Saizan_ (~saizan@li265-65.members.linode.com) has joined ##prolog
<wbeek> Indeed if I use the above predicate (not using DCG notation) it works the way I wanted it to.
<ski> it would seem more uniform to me to let `FromDCG' and `ToDCG' decide whether or not the call should be able to generate tokens out of an empty list (or alternatively backwards generate tokens out of an empty list)
<ski> also, i think your first clause there could mean dcg_replace/4 isn't steadfast ;)
* ski ponders it
* michael_lee has quit (*.net *.split)
* RLa has quit (*.net *.split)
* oleo has quit (*.net *.split)
* JuanDaugherty has quit (*.net *.split)
* Saizan has quit (*.net *.split)
<ski> hmm
* JuanDaugherty (~juand@cpe-198-255-198-157.buffalo.res.rr.com) has joined ##prolog
* oleo (~oleo@xdsl-78-35-133-3.netcologne.de) has joined ##prolog
<wbeek> The first clause is indeed there for the empty list. But I do acknowledge that for some applications this would not be ideal, e.g. having `ToDCG` writing something to demarcate the end of the input list is reached.
<ski> i suppose if `FromDCG' can materialize solutions, while consuming zero tokens (and perhaps producing some), then we could potentially apply it an infinite number of times in the replacement process
<ski> which is probably why you wrote the first clause
<wbeek> ski : what do you mean with "materialize solutions"?
<ski> (well, i'm probably opposed to "having `ToDCG` writing something to demarcate the end of the input list is reached", anyway ;)
<wbeek> :-)
* ccxlap has quit (Quit: WeeChat 0.4.2)
<ski> wbeek : just that it succeeding (once or more) without consuming any tokens (and maybe or maybe not pushing back some)
<ski> (to clarify, it's the special treatment of "end of input list" in parsing which i'm opposed to)
<ski> (s/, anyway/ on principle, anyway/)
* Saizan_ is now known as Saizan
* RLa (~RL@82.131.41.255.cable.starman.ee) has joined ##prolog
* opticdelusion has quit (Ping timeout: 246 seconds)
<ski> wbeek : anyway, i think the basic problem here is similar to the one for append/2
<ski> namely, consider (hypothetical, not in order)
<ski>   ?- append(Lists,[a,b]).
* opticdelusion (~opticdelu@p57BB876C.dip0.t-ipconnect.de) has joined ##prolog
<ski>   Lists = [[a,b]] ;
<ski>   Lists = [[],[a,b]] ;
<ski>   Lists = [[a],[b]] ;
<ski>   Lists = [[a,c],[]] ;
<ski>   Lists = [[],[],[a,b]] ;
<ski>   Lists = [[],[a],[b]] ;
<ski>   Lists = [[],[a,b],[]] ;
<ski>   Lists = [[a],[],[b]] ;
<ski>   Lists = [[a],[b],[]] ;
<ski>   Lists = [[a,b],[],[]] ;
<ski>   ...
<ski> there an infinite number of empty lists one could insert, to make the concatenation of the resulting list be the original list
* michael_lee (~michael_l@222.90.49.113) has joined ##prolog
<wbeek> Yes, so `FromDCG` should process at least one more code than it consumes?
<wbeek> "consumes" should be "generates" :-(
<wbeek> typo
<ski> for this reason, if one wants to use the mode append(-,+), i think it's better to actually make a separate predicate (*not* just changing the behaviout of append/2 when called in this mode. that would not be logically consistent with the other modes) which is like append/2, except that it also requires all the element lists to be nonempty
<ski> so, i think one could argue along the same lines for your dcg_replace/2/2 (though here there is probably not even a mode where this doesn't apply, unless we consider non-runtime-checkable stuff like considering a mode where the passed `ToDCG' is assumed to not be able to succeed while consuming no token)
* michael_lee has quit (Ping timeout: 260 seconds)
<ski> wbeek : i think it's enough to require/assume that it won't succeed while consuming zero tokens
<ski> you're not rerunning your transformations over already transformed parts of the input
<ski> if `ToDCG' can succeed while not consuming any tokens (called "nullable" in parser literature), then it can successfully be applied at any point in the process (unless you explicitly bar that)
<ski> hmm
<ski> actually checking whether input tokens is `[]' wouldn't be enough
<ski> that would only prohibit it from being run at the end of input
<ski> hm
* michael_lee (~michael_l@222.90.49.113) has joined ##prolog
<ski> to detect it, you'd want to check that `C' isn't some tail part of `G', i suppose
* j_wright (~jwright@unaffiliated/j-wright/x-9145068) has joined ##prolog
<ski> if you could detect that, you could just opt to fail in that case, effectively removing the "nullable" solutions of `ToDCG'
<ski> hrm .. actually, sorry. i just realized i've been talking about `ToDCG' for a while (since "so, i think one could argue along ...", i think) when i meant `FromDCG'
<ski> (and perhaps one'd also want to do something similar for `ToDCG' (in addition to `FromDCG' above), considering the "converse transforming" mode)
<ski> anyway, i think this option (attempting to catch the problematic case) sounds complicate here
<ski> (you'd possibly need to ponder how to do the right thing when the two lists being compared for non-tail-part are partial (can it happen in sensible code ?))
<ski> i think it's just simpler to declare "don't do that, then". i.e. say in the docs that the predicate can only be expected to work in case `FromDCG' can't match and (possibly) expand on the empty contiguous sublist, and likewise (though conversely) for `ToDCG' (for symmetry, mostly)
<ski> concretely, this means replacing
<ski>   dcg_replace(_, _, [], []):- !.
<ski> with
<ski>   dcg_replace(_, _, Ts, Ts).
<ski> which is the same as
<ski>   dcg_replace(_, _) --> [].
* ski reponders
<ski> perhaps that's not quite right. perhaps i was thinking too much about parsing here
<ski> if you're thinking of this as a "global" list transform, rather than as a "local" parser (and generator) tool, then you probably want to process the whole list (but then this doesn't really fit as nicely in DCG as in the parsing and grammar idea)
<ski> (fitting in the *implementation* of DCG as state-passing/threading is another thing)
<ski> so, if you want this, then i would suggest changing to
<ski>   dcg_replace(_, _, [], []).
<ski> reminds me, can i figure out a reasonable breaking case with the cut in place ?
<ski> yes, i have it :)
<ski>   epsilon --> [].
<ski>   foo --> [a].
<ski> and then the queries with solutions
<ski>   ?- phrase(dcg_replace(epsilon,foo),[],Out),Out = [a].
<ski>   false.
<ski>   ?- Out = [a],phrase(dcg_replace(epsilon,foo),[],Out).
<ski>   Out = [a] ;
<ski>   false.
<ski> this shows that dcg_replace/2/2 isn't steadfast
<ski> now, this is a case where `FromDCG', iow `epsilon' violates the "no nullable" requirement
<ski> so is there an example that doesn't violate that requirement ?
<ski> hm, actually, i don't think there are
<ski> considering
<ski>   dcg_replace(_, _, [], []):- !.
<ski> where we're only considering the mode (+(+,-),+(-,+),+,-) (which is probably how the predicate will normally used anyway)
<ski> to trigger the non-steadfastness, we need to make the behaviour differ according to whether the "output" (iow last argument) is actually provided as "extra input" or not
<ski> the the last argument is `[]', then there's no difference
<ski> so, assuming it should be a list, then if it is `[A|As]', then unifying the output argument with this after the call will fail (since the output got instantiated to `[]', and the cut prohibits trying out solutions from the other clauses)
<ski> while alternatively unifying the output argument with `[A|As]' before the call will make the head unification fail *before* hitting the cut, thus we get to the other clauses
<ski> since in the former case, we fail, we want to make the latter case succeed, to find a discrepancy
<ski> the above example queries was one way of making the latter case succeed
<ski> however, let's now assume that `FromDCG' is not nullable
<ski> also, recall that the input list is already known to be `[]' (otherwise we couldn't contrast with triggering the cut in the former case)
<ski> so, the last two arguments are thus `[]' and `[A|As]'
<ski> now, because `FromDCG' is not nullable, it can't succeed when being feeded `[]' as input, so the middle clause falls away
<ski> and the last clause attempts to match the input with `[F|D]', which also fails since it's `[]'
<ski> therefore, i don't think there's any possibility of triggering non-steadfastness if we assume `FromDCG' is not nullable (at least not in this (+(+,-),+(-,+),+,-) of the predicate :)
```

@author Wouter Beek
@see http://stackoverflow.com/users/1613573/mat
@see http://stackoverflow.com/questions/6392725/using-a-prolog-dcg-to-find-replace-code-review
@version 2013/05-2013/09, 2013/11-2014/01
*/

:- use_module(library(error)).

:- use_module(plc(dcg/dcg_generics)).

:- meta_predicate(dcg_replace(//,//,?,?)).
:- meta_predicate(dcg_replace0(//,//,?,?)).
:- meta_predicate(is_not_nullable(//)).





%! dcg_replace(:FromDCG, :ToDCG)// is det.
% Generic DCG rule-based replacements.
%
% @arg FromDCG A DCG rule that parses, i.e. instantiation `(+,-)`,
%      and that is not nullable.
%      In parsing, a nullable rule is one that succeeds on the null string.
% @arg ToDCG A DCG rule that generates, i.e. instantiation `(-,+)`.
%
% # Implementation details: Why we did not use DCG notation
%
% Using DCGs we could not acchieve tail recursion in the last clause,
% due to non-steadfastness. See the following code:
%
% ```prolog
% dcg_replace(_, _, [], []):- !.
% dcg_replace(FromDCG, ToDCG), ToDCG -->
%   FromDCG, !,
%   dcg_replace(FromDCG, ToDCG).
% dcg_replace(FromDCG, ToDCG), [Code] -->
%   [Code],
%   dcg_replace(FromDCG, ToDCG).
% ```
%
% The listing of the above DCG rule more clearly shows the non-steadfastness:
%
% ```prolog
% dcg_replace(_, _, [], []):- !.
% dcg_replace(A, D, B, F):-
%   phrase(A, B, C), !,
%   E=C,
%   dcg_replace(A, D, E, G),
%   phrase(D, F, G).
% dcg_replace(B, C, A, E):-
%   A=[F|D],
%   dcg_replace(B, C, D, G),
%   E=[F|G].
% ```

dcg_replace(FromDCG, _, _, _):-
  \+ \+ phrase(FromDCG, ``),
  domain_error('Non-nullable parse rule', FromDCG).
dcg_replace(FromDCG, ToDCG, X, Y):-
  % Parse input & generate output.
  nonvar(X),
  var(Y),
  dcg_replace0(FromDCG, ToDCG, X, Y).

dcg_replace0(_, _, [], []):- !.
dcg_replace0(FromDCG, ToDCG, L1, L2):-
  % Parse.
  nonvar(L1),
  phrase(FromDCG, L1, L11), !,
  
  % Recurse.
  dcg_replace0(FromDCG, ToDCG, L11, L22),
  
  % Generate.
  var(L2),
  phrase(ToDCG, L2, L22).
dcg_replace0(FromDCG, ToDCG, [H|T1], [H|T2]):-
  dcg_replace0(FromDCG, ToDCG, T1, T2).


%! is_not_nullable(:DCG) is semidet.
% Succeeds if the given DCG rule is not nullable.
%
% In parsing, a nullable rule is one that succeeds on the null string.

is_not_nullable(DCG):-
  \+ \+ phrase(DCG, []).

