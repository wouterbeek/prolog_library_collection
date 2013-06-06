:- module(
  dcg_generic,
  [
% AGGREGATES %
    dcg_word//1, % -Word:atom

% ALL/UNTIL %
    dcg_atom_all//1, % -Atom:atom
    dcg_atom_until//2, % :End:dcg
                       % -Atom:atom
    dcg_atom_without//2, % :End:dcg
                         % -Atom:atom
    dcg_codes_all//1, % -Codes:list(code)
    dcg_codes_until//2, % :End:dcg
                        % -Codes:list(code)
    dcg_codes_without//2, % :End:dcg
                          % -Codes:list(code)

% DEBUG %
    dcg_debug//0,

% DOM %
    dcg_element//3, % ?Name:atom
                    % ?Attrs:list(nvpair)
                    % ?Content:dom

% LIST %
    dcg_separated_list//2, % :Separator:dcg
                           % -Codess:list(list(codes))

% OTHERS %
    conj//1, % ?Lang:atom
    dcg_peek//1, % -X:code
    disj//1, % ?Lang:atom
    language//1, % ?Lang:atom
    uncertainty//1, % ?Lang:atom

% PHRASE EXTENSION %
    dcg_phrase/2, % :DCGBody:dcg
                  % ?In:list(code)
    dcg_phrase/3, % :DCGBody:dcg
                  % ?In:list(code)
                  % ?Out:list(code)

% RE %
    dcg_plus//1, % +DCGBody:dcg
    dcg_star//1 % +DCGBody:dcg
  ]
).

/** <module>

Generic DCG clauses.

@author Wouter Beek
@version 2013/05-2013/06
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(generics(cowspeak)).
:- use_module(html(html)).
:- use_module(library(lists)).

:- reexport(library(dcg/basics)).

% ALL/UNTIL %
:- meta_predicate(dcg_atom_until(//,-,+,-)).
:- meta_predicate(dcg_atom_without(//,-,+,-)).
:- meta_predicate(dcg_codes_until(//,-,+,-)).
:- meta_predicate(dcg_codes_without(//,-,+,-)).
% LIST %
:- meta_predicate(dcg_separated_list(//,-,+,-)).
% PHRASE EXTENSIONS %
:- meta_predicate(dcg_phrase(//,?)).
:- meta_predicate(dcg_phrase(//,?,?)).
% RE %
:- meta_predicate(dcg_plus(//,?,?)).
:- meta_predicate(dcg_star(//,?,?)).



% AGGREGATES %

dcg_word(Word) -->
  dcg_atom_until(space, Word).



% ALL/UNTIL %

dcg_atom_all(Atom) -->
  {var(Atom)},
  dcg_codes_all(Codes),
  {atom_codes(Atom, Codes)}.
dcg_atom_all(Atom) -->
  {nonvar(Atom)},
  {atom_codes(Atom, Codes)},
  dcg_codes_all(Codes).

%! dcg_atom_until(+End:dcg, -Atom:atom)// is det.
% @see dcg_codes_until//2

dcg_atom_until(End, Atom) -->
  {var(Atom)},
  dcg_codes_until(End, Codes),
  {atom_codes(Atom, Codes)}.
dcg_atom_until(End, Atom) -->
  {nonvar(Atom)},
  {atom_codes(Atom, Codes)},
  dcg_codes_until(End, Codes).

dcg_atom_without(End, Atom) -->
  {var(Atom)},
  dcg_codes_without(End, Codes),
  {atom_codes(Atom, Codes)}.
dcg_atom_without(End, Atom) -->
  {nonvar(Atom)},
  {atom_codes(Atom, Codes)},
  dcg_codes_without(End, Codes).

dcg_codes_all([]) -->
  [].
dcg_codes_all([H|T]) -->
  [H],
  dcg_codes_all(T).

%! dcg_codes_until(+End:dcg, -Codes:list(code))// is det.
% Returns the codes that occur before =End= can be consumed.
%
% =End= is not an arbitrary DCG body, since disjunction does not play out
% well (note that =End= occurs before and fater the =|-->|=).
%
% We enforce determinism after the first occurrence of =End= is consumed.
%
% @tbd There are problems with list elements: meta_predicate/1 cannot
%      identity the modules for DCGs in this case.

dcg_codes_until(End, []), End -->
  End,
  !.
dcg_codes_until(End, [H|T]) -->
  [H],
  dcg_codes_until(End, T).

%! dcg_codes_without(+End:dcg, -Codes:list(code))// is det.

dcg_codes_without(End, []) -->
  End,
  !.
dcg_codes_without(End, [H|T]) -->
  [H],
  dcg_codes_without(End, T).



% DEBUG %

dcg_debug(L, []):-
  format(
    atom(Text),
    'Wouter, Wouter, Wouter. I have the feeling that something is wrong here.\n~w\n',
    [L]
  ),
  thread_create(cowspeak(Text), _ID, []),
  gtrace. %DEB



% HTML %

dcg_element(Name, MatchAttrs, Content) -->
  {var(MatchAttrs)},
  dcg_element(Name, [], Content).
dcg_element(Name, MatchAttrs, Content) -->
  {is_list(MatchAttrs)},
  [element(Name, Attrs, Content)],
  {maplist(html_attribute(Attrs), MatchAttrs)}.
dcg_element(Name, MatchAttr, Content) -->
  {\+ is_list(MatchAttr)},
  dcg_element(Name, [MatchAttr], Content).



% LIST %

dcg_separated_list(Separator, [H|T]) -->
  string(H),
  % Allow symmetric spaces.
  (Separator ; blank, Separator, blank),
  !,
  dcg_separated_list(Separator, T).
dcg_separated_list(_Separator, [H]) -->
  string(H).



% OTHERS %

conj(en) --> "and".
conj(nl) --> "en".
conj(_Lang) --> comma.

dcg_peek(X), [X] -->
  [X].

disj(nl) --> "of".

language(nl) --> "Latijn".

% Three dots uncertainty representation.
uncertainty(_Lang) --> "...".
% Question mark uncertainty representation.
uncertainty(Lang) -->
  opening_round_bracket,
  uncertainty(Lang),
  closing_round_bracket.
uncertainty(Lang) -->
  opening_square_bracket,
  uncertainty(Lang),
  closing_square_bracket.
uncertainty(_Lang) --> question_mark.
uncertainty(_Lang) --> "ca.".



% PHRASE EXTENSION

dcg_phrase(DCGBody, In):-
  dcg_phrase(DCGBody, In, []).

dcg_phrase(DCGBody, In, Out):-
  is_list(In),
  !,
  phrase(DCGBody, In, Out).
dcg_phrase(DCGBody, In1, Out):-
  atomic(In1),
  !,
  atom_codes(In1, In2),
  dcg_phrase(DCGBody, In2, Out).



% RE %

dcg_plus(DCGBody) -->
  DCGBody,
  dcg_plus(DCGBody).
dcg_plus(DCGBody) -->
  DCGBody.

dcg_star(DCGBody) -->
  DCGBody,
  dcg_star(DCGBody).
dcg_star(_DCGBody) --> [].

