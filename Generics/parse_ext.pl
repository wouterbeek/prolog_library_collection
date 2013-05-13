:- module(
  parse_ext,
  [
    parse_atom/2, % ?Atom:atom
                  % ?C:dlist
    parse_char/2, % :P
                  % ?C:dlist
    parse_char/3, % :P
                  % ?R:char
                  % ?C:dlist
    contains/3, % +O:list(nvpair)
                % +Ps:list(atom)
                % +C:dlist
    discard/1, % ?C:dlist
    discard_until/2, % +Atom:atom
                     % ?C:dlist
    exponent/3,
    integer/2, % ?Integer:integer
               % ?C:dlist
    parse_date/2, % ?Year:integer
                  % ?C:dlist
    parse_date/3, % ?Begin:integer
                  % ?End:integer
                  % ?C:dlist
    parse_word/2, % ?Word:atom
                  % ?C:dlist
    parse_words/2, % ?Words:list(atom)
                   % ?C:dlist
    parse_re/3, % +O1:list(nvpair)
                % +Ps:list(atom)
    parse_re/4 % +O1:list(nvpair)
               % +Ps:list(atom)
               % -Result:list(integer)
  ]
).

/** <module> Parse extensions

Shall we use DCGs for parsing or REs?

We need DCGs, because they allow us to define a complex grammar in a
modular way / good compositionality.

We need REs, because they allow easy quantification using the _Wilcards_
=|*|=, =|+|=, =|?|=, and the positive integers. Although DCG rules allow the use
of quantification as well, this muddies their compositionality considerably.

Suppose we have the following DCG:
==
white --> end_of_line.
white --> form_feed.
white --> space.
white --> tab.
==
And suppose we construe a new DCG to perform some domain-dependent parsing:
==
some_parse --> white, some_other_parse.
some_parse --> some_other_parse.
==
This is less nice than:
==
some_parse --> re([q('?')], white), some_other_parse.
==
And what to think of the following:
==
some_parse -->
  re([q('?')], white),
  some_other_parse,
  re([q('?')], white).
==
You need _2^N_ DCG clauses, where _N_ is the number of =|?|= Wilcards used in
a single RE.

The advantages of using the other Wildcards in REs are even more
significant, as positive integers require you to add an extra argument to
each DCG predicate for which you want to use (the quivalent of) this
Wildcard.

This is why I chose to integrate the use of these nice Wildcards into DCGs.

Another nice property of REs is the =i= flag for case-sensitive /
case-insensitive matching. In our implementation this can be done by using
either the production rule =letter= (i.e., case-insensitive) or the production
rules =letter_lowercase= and =letter_uppercase= (i.e., case-sensitive).
This shows another advantage of using DCGs: for every set of production rules
of the form
==
r_0 --> r_1.
...
r_0 --> r_n.
==
we can define subcategory dependence/independence by choosing either =r_0= or
a subset of \{ r_1, \ldots, r_n \}.

@author Wouter Beek
@version 2013/01-2013/03, 2013/05
*/

:- use_module(standards(ascii)). % Used for meta-predicates.



%! parse_atom(+Atom:atom, +Codes:dlist) is semidet.
%! parse_atom(+Atom, -Codes:dlist) is det.
%! parse_atom(-Atom, +Codes:dlist) is det.
% Parsing atoms in a case-sensitive way.

parse_atom(Atom, C1-C0):-
  nonvar(Atom),
  !,
  once((
    atom_codes(Atom, Codes),
    length(Codes, Length),
    parse_re([case(sensitive), q(Length)], [any], Codes-[], C1-C0)
  )).
% Since a code sequences can be read as an atom in multiple ways,
parse_atom(Atom, C1-C0):-
  var(Atom),
  !,
  once(parse_re([case(sensitive), out(atom), q('*')], [any], Atom, C1-C0)).

%! parse_char(:P, +C:dlist) is semidet.
%! parse_char(:P, -C:dlist) is nondet.
% @see A wrapper for char/3.

parse_char(P, C1-C0):-
  parse_char(P, _R, C1-C0).

%! parse_char(:P, ?R, +C:dlist) is semidet.
%! parse_char(:P, ?R, -C:dlist) is nondet.
% Parsing characters by one of their descriptive names.
%
% @arg P A public predicate from module ASCII.
% @arg R An atomic character value; the result of the parse.
% @arg C A difference list of codes.
%
% The nondet case can generate alternative parses, e.g.:
% ==
% ?- char(a, X, [Y]-[]).
% X = a,
% Y = 97 ;
% X = 'A',
% Y = 65 ;
% false.
% ==

parse_char(P, R, C1-C0):-
  parse_re([case(sensitive), out(char), q(1)], P, R, C1-C0).

contains(_O, _Ps, []-_Co):-
  !,
  fail,
  !.
contains(O, Ps, C1-_C0):-
  member(P, Ps),
  call(P, O, _R, C1-_C2),
  !.
contains(O, Ps, C1-C0):-
  call(any, O, _R, C1-C2),
  !,
  contains(O, Ps, C2- C0).

convert_epoch(Year, 'AD', Year):-
  !.
convert_epoch(UnsignedYear, 'BC', SignedYear):-
  SignedYear is -UnsignedYear.

%! discard(+C:diff_list) is det.
% Discards the rest of the difference list, always returning the empty list.
% This is done at the end of a parsing activity, when one is no longer
% interested in the rest of the code sequence.

discard(_Codes-[]).

%! discard_until(+Match:oneof([atom,codes]), +C:diff_list) is det.
% Discards codes until the given matching atom or code sequence is found.

discard_until(Match, C1-C0):-
  atomic(Match),
  !,
  atom_codes(Match, Match1),
  discard_until(Match1, C1-C0).
discard_until(Codes, C1-C0):-
  append(Codes, C0, C1),
  !.
discard_until(Codes, [_ | C1]-C0):-
  discard_until(Codes, C1-C0).

epoch('BC', C1-C0):-
  parse_char(v_lowercase, C1-C2),
  parse_re([q('?')], white, C2-C3),
  parse_char(dot, C3-C4),
  parse_re([q('?')], white, C4-C5),
  parse_atom('Chr', C5-C0).
epoch('AD', C-C).

exponent(O1, R1-R0, C1-C0):-
  merge_options([q(1)], O1, O2),
  parse_re(O2, exponent_sign, R1-R2, C1-C2),
  merge_options([q('+')], O1, O3),
  parse_re(O3, digit, R2-R0, C2-C0).

integer(Integer, C1-C0):-
  nonvar(Integer),
  !,
  number_codes(Integer, Codes),
  length(Codes, Length),
  parse_re([q(Length)], [decimal_digit], Codes-[], C1-C0).
integer(Integer, C1-C0):-
  var(Integer),
  !,
  parse_re([out(number), q('+')], [decimal_digit], Integer, C1-C0).

% Point
parse_date(Year, C1-C0):-
  parse_re([out(number), q(4)], decimal_digit, Year, C1-C0).

% Interval
parse_date(Begin, End, C1-C0):-
  parse_re([out(number), q(4)], decimal_digit, Begin0, C1-C2),
  parse_char(hyphen, C2-C3),
  parse_re([out(number), q(4)], decimal_digit, End0, C3-C4),
  epoch(Epoch, C4-C0),
  convert_epoch(Begin0, Epoch, Begin),
  convert_epoch(End0, Epoch, End).

parse_word(Word, C1-C0):-
  parse_re([q('*'), out(atom)], letter, Word, C1-C2),
  parse_re([q('?')], dot, C2-C3),
  parse_char(end_of_word, C3-C0),
  !.

parse_words([Word | Words], C1-C0):-
  parse_word(Word, C1-C2),
  parse_re([q('?')], space, C2-C3),
  !,
  parse_words(Words, C3-C0).
parse_words([Word], C1-C0):-
  parse_word(Word, C1-C0).
parse_words([], C-C).

%! parse_re(+O1:list(nvpair), +Ps:list(atom), ?C:dlist(list))
% @see Wrapper for parse_re/4.

parse_re(O1, Ps, C):-
  nonvar(Ps),
  parse_re(O1, Ps, _R, C).

%! parse_re(+O1:list(nvpair), +Ps:list(atom), ?R, ?C)
% @arg O1 A list of name-value pairs. The following options are
%        supported:
%        * =|case(oneof([insensitive,lower,sensitive,upper])|=
%        Case-sensitivity is used for the =letter= DCG.
%        * =|in(oneof([atom,codes,number]))|=
%        * =|lang(oneof([c,xml]))|=
%        Special language support.
%        * =|out(oneof([atom,codes,number]))|=
%        * =|q(or([integer,oneof(['?','+','*'])]))|=
%        Regular expression quantification.
%        * =|replace(From:atom,To:atom)|=
%        Replacements on the level of single codes.
% @arg Ps A list of atomic names of //0 DCG production rules.
%        If the input is not a list, then a singleton list holding this
%        input is created.
% @arg Results What was parsed by this invocation of re//3.
%        The type of this argument is determined by the =output_format=
%        option, defaults to =codes=.

% Conversion of the result.
parse_re(O1, Ps, R, C):-
  (var(R) ; \+ R = _-_),
  !,
  select_option(out(F), O1, O2, codes),
  parse_re(O2, Ps, R1-[], C),
  re_convert(F, R1, R).
% Conversion of the parsed codes.
parse_re(O1, Ps, R, C):-
  \+ C = _C1-_C0,
  !,
  select_option(in(F), O1, O2, codes),
  re_convert(F, C1, C),
  parse_re(O2, Ps, R, C1-[]).
% Non-list production rule.
parse_re(O1, P, R1-R0, C1-C0):-
  \+ is_list(P),
  !,
  parse_re(O1, [P], R1-R0, C1-C0).
parse_re(O1, _Ps, R-R, C-C):-
  option(q('?'), O1).
parse_re(O1, Ps, R1-R0, C1-C0):-
  select_option(q('?'), O1, O2),
  member(P, Ps),
  call(P, O2, R1-R0, C1-C0).
parse_re(O1, Ps, R1-R0, C1-C0):-
  select_option(q('+'), O1, O2),
  member(P, Ps),
  call(P, O2, R1-R2, C1-C2),
  merge_options([q('*')], O2, O3),
  parse_re(O3, Ps, R2-R0, C2-C0).
parse_re(O1, _Ps, R-R, C-C):-
  option(q('*'), O1).
parse_re(O1, Ps, R1-R0, C1-C0):-
  option(q('*'), O1),
  member(P, Ps),
  call(P, O1, R1-R2, C1-C2),
  merge_options([q('*')], O1, O2),
  parse_re(O2, Ps, R2-R0, C2-C0).
parse_re(O1, _Ps, R-R, C-C):-
  option(q(0), O1).
parse_re(O1, Ps, R1-R0, C1-C0):-
  select_option(q(L), O1, O2),
  integer(L),
  L > 0,
  !,
  member(P, Ps),
  call(P, O2, R1-R2, C1-C2),
  NewL is L - 1,
  merge_options([q(NewL)], O2, O3),
  parse_re(O3, Ps, R2-R0, C2-C0).

re_convert(atom, Codes, Atom):-
  !,
  atom_codes(Atom, Codes).
re_convert(char, [Code], Char):-
  !,
  char_code(Char, Code).
re_convert(codes, Codes, Codes):-
  !.
re_convert(number, Codes, Number):-
  !,
  number_codes(Number, Codes).
re_convert(Format, _, _):-
  gtrace, %WB
  debug(parse_ext, 'Unrecognized format ~w for codes conversion.', [Format]).
