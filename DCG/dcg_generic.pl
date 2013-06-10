:- module(
  dcg_generic,
  [
% AGGREGATES %
    dcg_word//1, % -Word:list(code)
    dcg_word_atom//1, % -Word:atom

% ALL/UNTIL %
    dcg_all//1, % -Codes:list(code)
    dcg_all_atom//1, % -Atom:atom
    dcg_until//2, % :End:dcg
                  % -Codes:list(code)
    dcg_until_atom//2, % :End:dcg
                       % -Atom:atom
    dcg_without//2, % :End:dcg
                    % -Codes:list(code)
    dcg_without_atom//2, % :End:dcg
                         % -Atom:atom

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
    disj//1, % ?Lang:atom
    language//1, % ?Lang:atom
    uncertainty//1, % ?Lang:atom

% PEEK %
    dcg_peek//1, % ?X:code
    dcg_peek_atom//1, % -Atom:atom
    dcg_peek_char//1, % ?Char:char
    dcg_peek_length//2, % ?Length:integer
                        % ?Codes:list(code)

% PHRASE EXTENSION %
    dcg_phrase/2, % :DCGBody:dcg
                  % ?In
    dcg_phrase/3, % :DCGBody:dcg
                  % ?In
                  % ?Out

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

:- use_module(library(dcg/basics)).
:- reexport(library(dcg/basics)).

% ALL/UNTIL %
:- meta_predicate(dcg_until(//,-,+,-)).
:- meta_predicate(dcg_until_atom(//,-,+,-)).
:- meta_predicate(dcg_without(//,-,+,-)).
:- meta_predicate(dcg_without_atom(//,-,+,-)).
% LIST %
:- meta_predicate(dcg_separated_list(//,-,+,-)).
% PEEK %
:- meta_predicate(dcg_peek_length(?,?,?,?)).
:- meta_predicate(dcg_peek_length(+,?,?,?,?)).
% PHRASE EXTENSIONS %
:- meta_predicate(dcg_phrase(//,?)).
:- meta_predicate(dcg_phrase(//,?,?)).
% RE %
:- meta_predicate(dcg_plus(//,?,?)).
:- meta_predicate(dcg_star(//,?,?)).



% AGGREGATES %

%! dcg_word(-Word:list(code)) is semidet.
% Returns the first word that occurs in the codes list.
%
% A word is defined as any sequence af alphabetic characters
% delimited by a non-alphabetic character.
%
% The delimiting character is not consumed.

dcg_word([Char|Word]) -->
  alpha_to_lower(Char),
  dcg_word(Word).
dcg_word([]), [Char] -->
  [Char].

dcg_word_atom(Word) -->
  dcg_word(Codes),
  {atom_codes(Word, Codes)}.



% ALL/UNTIL %

dcg_all_atom(Atom) -->
  {var(Atom)},
  dcg_all(Codes),
  {atom_codes(Atom, Codes)}.
dcg_all_atom(Atom) -->
  {nonvar(Atom)},
  {atom_codes(Atom, Codes)},
  dcg_all(Codes).

%! dcg_until_atom(+End:dcg, -Atom:atom)// is det.
% @see dcg_until//2

dcg_until_atom(End, Atom) -->
  {var(Atom)},
  dcg_until(End, Codes),
  {atom_codes(Atom, Codes)}.
dcg_until_atom(End, Atom) -->
  {nonvar(Atom)},
  {atom_codes(Atom, Codes)},
  dcg_until(End, Codes).

dcg_without_atom(End, Atom) -->
  {var(Atom)},
  dcg_without(End, Codes),
  {atom_codes(Atom, Codes)}.
dcg_without_atom(End, Atom) -->
  {nonvar(Atom)},
  {atom_codes(Atom, Codes)},
  dcg_without(End, Codes).

dcg_all([]) -->
  [].
dcg_all([H|T]) -->
  [H],
  dcg_all(T).

%! dcg_until(+End:dcg, -Codes:list(code))// is det.
% Returns the codes that occur before =End= can be consumed.
%
% =End= is not an arbitrary DCG body, since disjunction does not play out
% well (note that =End= occurs before and fater the =|-->|=).
%
% We enforce determinism after the first occurrence of =End= is consumed.
%
% @tbd There are problems with list elements: meta_predicate/1 cannot
%      identity the modules for DCGs in this case.

dcg_until(End, []), End -->
  End,
  !.
dcg_until(End, [H|T]) -->
  [H],
  dcg_until(End, T).

%! dcg_without(+End:dcg, -Codes:list(code))// is det.

dcg_without(End, []) -->
  End,
  !.
dcg_without(End, [H|T]) -->
  [H],
  dcg_without(End, T).



% DEBUG %

dcg_debug(L, []):-
  format(
    atom(Text),
    'Wouter, I have the feeling that something is wrong here.\n\c
     Unable to phrase <~w>\n',
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



% PEEK %

%! dcg_peek(-X:code) is semidet.
% Returns the next code in the codes list, if any.
% Does not consume anything.

dcg_peek(X), [X] -->
  [X].

%! dcg_peek_atom(+Atom:atom) is semidet.
% Succeeds if the given atom occurs next in the codes list,
% but does not consume anything.

dcg_peek_atom(Atom), Codes -->
  {atom_codes(Atom, Codes)},
  Codes.

%! dcg_peek_char(?Char) is semidet.

dcg_peek_char(Char), [Code] -->
  {nonvar(Char)},
  {char_code(Char, Code)},
  [Code].
dcg_peek_char(Char), [Code] -->
  {var(Char)},
  [Code],
  {char_code(Char, Code)}.

%! dcg_peek_length(?Length:integer, ?Peek:list(code)) is nondet.

% In order to prevent the generative call of DCG rule to result in an
% infinite loop over increasingly bigger lengths in length/2, we
% explicitly instigate an upper bound for length.
% This upper bound is the length of the code list that is parsed.
dcg_peek_length(Length, Peek, In, Out):-
  length(In, MaxLength),
  dcg_peek_length(MaxLength, Length, Peek, In, Out).
dcg_peek_length(MaxLength, Length, Peek), Peek -->
  {
    % Effectuate the upper bound to length.
    % In the generative call this is the predicate that backtracks, causing
    % the peek list to grow until it reaches the maximum allowed size.
    between(0, MaxLength, Length),
    % The length is always instantiated at this point.
    length(Peek, Length)
  },
  Peek.



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

%! dcg_plus(:DCGBody:dcg) is nondet.
% Applies the given DCGBody one or more times to the codes list.
%
% The longest codes list that can be parsed by the given DCG body
% is returned first. The singleton codes list is retuned last.
%
% @compat Inspired by the regular expression operator =+=.

dcg_plus(DCGBody) -->
  DCGBody,
  dcg_plus(DCGBody).
dcg_plus(DCGBody) -->
  DCGBody.

%! dcg_star(:DCGBody:dcg) is nondet.
% Applies the given DCGBody zero or more times to the codes list.
%
% The longest codes list that can be parsed by the given DCG body
% is returned first. The empty codes list is retuned last.
%
% @compat Inspired by the regular expression operator =*=.

dcg_star(DCGBody) -->
  DCGBody,
  dcg_star(DCGBody).
dcg_star(_DCGBody) --> [].

