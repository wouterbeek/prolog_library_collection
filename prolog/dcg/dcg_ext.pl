:- module(
  dcg_ext,
  [
    '...'//0,
    '...'//1,              % -Cs
    '?'//1,                % :Dcg_0
    '?'//2,                % :Dcg_1, -Args1
    '?'//3,                % :Dcg_2, -Args1, -Args2
    '*'//1,                % :Dcg_0
    '*'//2,                % :Dcg_1, -Args1
    '*'//3,                % :Dcg_2, -Args1, -Args2
    '+'//1,                % :Dcg_0
    '+'//2,                % :Dcg_1, -Args1
    '+'//3,                % :Dcg_2, -Args1, -Args2
    '#'//2,                % ?Occurrences, :Dcg_0
    '#'//3,                % ?Occurrences, :Dcg_1, -Args1
    '#'//4,                % ?Occurrences, :Dcg_2, -Args1, -Args2
    '*n'//2,               % ?High, :Dcg_0
    '*n'//3,               % ?High, :Dcg_1, -Args1
    '*n'//4,               % ?High, :Dcg_2, -Args1, -Args2
    'm*'//2,               % ?Low, :Dcg_0
    'm*'//3,               % ?Low, :Dcg_1, -Args1
    'm*'//4,               % ?Low, :Dcg_2, -Args1, -Args2
    'm*n'//3,              % ?Low, ?High, :Dcg_0
    'm*n'//4,              % ?Low, ?High, :Dcg_1, -Args1
    'm*n'//5,              % ?Low, ?High, :Dcg_2, -Args1, -Args2
    atom_ellipsis//2,      % +A, +Max
    atom_phrase/2,         % :Dcg_0, ?A
    atom_phrase/3,         % :Dcg_0, +A1, ?A2
    between//2,            % +Low:integer, +High:integer
    between//3,            % +Low:integer, +High:integer, ?Value:integer
    between_digit//2,      % +Low:hex, +High:hex
    between_digit//3,      % +Low:hex, +High:hex, -Weight:between(0,15)
    between_digit//4,      % +Low:hex, +High:hex, -Weight:between(0,15), -Code:code
    between_radix//2,      % +Low:compound, +High:compound
    between_radix//3,      % +Low:compound, +High:compound, ?Value:compound
    bit//1,                % ?I:between(0,1)
    bracketed//1,          % :Dcg_0
    bracketed//2,          % ?Type:oneof([angular,curly,round,square,ungular]), :Dcg_0
    bs//0,
    dcg/1,                 % :Dcg_0
    dcg_apply//2,          % :Dcg_1, +Args
    dcg_apply_cp//2,       % :Dcg_1, +Args
    dcg_atom//2,           % :Dcg_1, ?A
    dcg_between//2,        % :Between_0, :Dcg_0
    dcg_between//3,        % :Begin_0, :Dcg_0, :End_0
    dcg_call//1,           % :Dcg_0
    dcg_call//2,           % :Dcg_1, ?Arg1
    dcg_call//3,           % :Dcg_2, ?Arg1, ?Arg2
    dcg_call//4,           % :Dcg_3, ?Arg1, ?Arg2, ?Arg3
    dcg_call//5,           % :Dcg_4, ?Arg1, ?Arg2, ?Arg3, ?Arg4
    dcg_call//6,           % :Dcg_5, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5
    dcg_call_cp//1,        % :Dcg_0
    dcg_call_cp//2,        % :Dcg_1, ?Arg1
    dcg_call_cp//3,        % :Dcg_2, ?Arg1, ?Arg2
    dcg_call_cp//4,        % :Dcg_3, ?Arg1, ?Arg2, ?Arg3
    dcg_call_cp//5,        % :Dcg_4, ?Arg1, ?Arg2, ?Arg3, ?Arg4
    dcg_call_cp//6,        % :Dcg_5, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5
    dcg_goal//1,           % :Goal_0
    dcg_max_width/3,       % :Dcg_1, +Args, -MaxWidth
    dcg_once//1,           % :Dcg_0
    dcg_string//2,         % :Dcg_1, ?S
    dcg_tab//0,
    dcg_width/2,           % :Dcg_0, -Width
    dcg_with_output_to/2,  % +Sink, :Dcg_0
    debug/2,               % +Topic, :Dcg_0
    def//3,                % :Dcg_1, -Arg, +Def
    digit_code//1,         % ?C
    done//0,
    eol//0,
    frac_pos/2,            % +Frac:between(0.0,1.0), -Ds:list(between(0,9))
    generate_as_digits//2, % +N:nonneg, +NoDs
    generate_as_digits//3, % +N:nonneg, +Base:positive_integer, +NoDs
    generating//0,
    indent//1,             % +Indent:nonneg
    indent//2,             % +Indent:nonneg, :Dcg_0
    indent_nl//2,          % +Indent:nonneg, :Dcg_0
    nl//0,
    nonblank//0,
    number//0,
    opt//1,                % :Dcg_0
    opt//2,                % :Dcg_1, ?Arg
    parsing//0,
    perc//1,               % +Perc:between(0.0,1.0)
    perc_fixed//1,         % +Perc:between(0.0,1.0)
    pos/2,                 % +N:nonneg, -Ds:list(between(0,9))
    pos/3,                 % +N:nonneg, +Base, -Ds:list(between(0,9))
    pos_frac/2,            % +Ds:list(between(0,9)), -FracPart:rational
    pos_sum/2,             % +Ds:list(between(0,9)), -N:nonneg
    pos_sum/3,             % +Ds:list(nonneg), +Base:positive_integer, -N:nonneg
    progress_bar//2,       % +Processed, +All
    quoted//1,             % :Content_2
    quoted//2,             % :Quote_2, :Content_2
    quoted//3,             % ?Length, :Quote_2, :Content_2
    rest//0,
    rest//1,               % -Rest:list(code)
    section//3,            % +Indent:nonneg, +Message:string, :Dcg_0
    seplist//2,            % :Dcg_0, :Sep_0
    seplist//3,            % :Dcg_1, :Sep_0, +L
    skip_line//0,
    str//1,                % +S
    str_ellipsis//2,       % +S, +Max
    string//0,
    string_phrase/2,       % :Dcg_0, ?S
    string_phrase/3,       % :Dcg_0, +S1, ?S2
    string_without//1,     % +EndCs
    sum_pos/2,             % +N:nonneg, -Ds:list(between(0,9))
    sum_pos/3,             % +N:nonneg, +Base:positive_integer, -Ds:list(nonneg)
    tab//1,                % +Indent:nonneg
    tab//2,                % +Indent:nonneg, :Dcg_0
    tab_nl//2,             % +Indent:nonneg, :Dcg_0
    thousands//1,          % +Integer:integer
    ws//0
  ]
).
:- reexport(library(dcg/basics), except([digit//1,digits//1])).
:- reexport(library(url/rfc1738), [
     alpha//1,      % ?C
     alphadigit//1, % ?C
     digit//1,      % ?Weight:between(0,9)
     digit//2,      % ?Weight:between(0,9), ?C
     escape//1 as percent_escape, % ?C
     hex//1,        % ?Weigth:between(0,15)
     hex//2,        % ?Weigth:between(0,15), ?C
     hialpha//1,    % ?C
     lowalpha//1    % ?C
   ]).

/** <module> DCG extensions

My favorite collection of DCG rules.

@author Wouter Beek
@version 2015/11-2016/03, 2016/05-2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(atom_ext)).
:- use_module(library(code_ext)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(math/math_ext)).
:- use_module(library(math/radconv)).
:- use_module(library(settings)).
:- use_module(library(string_ext)).

:- meta_predicate
    ?(//, ?, ?),
    ?(3, -, ?, ?),
    ?(4, -, -, ?, ?),
    *(//, ?, ?),
    *(3, -, ?, ?),
    *(4, -, -, ?, ?),
    +(//, ?, ?),
    +(3, -, ?, ?),
    +(4, -, -, ?, ?),
    #(+, //, ?, ?),
    #(+, 3, -, ?, ?),
    #(+, 4, -, -, ?, ?),
    '*n'(?, //, ?, ?),
    '*n'(?, 3, -, ?, ?),
    '*n'(?, 4, -, -, ?, ?),
    'm*'(?, //, ?, ?),
    'm*'(?, 3, -, ?, ?),
    'm*'(?, 4, -, -, ?, ?),
    'm*n'(?, ?, //, ?, ?),
    'm*n'(?, ?, 3, -, ?, ?),
    'm*n'(?, ?, 4, -, -, ?, ?),
    'm*n__g'(?, ?, +, //, ?, ?),
    'm*n__g'(?, ?, +, 3, -, ?, ?),
    'm*n__g'(?, ?, +, 4, -, -, ?, ?),
    'm*n__p'(?, ?, +, //, ?, ?),
    'm*n__p'(?, ?, +, 3, -, ?, ?),
    'm*n__p'(?, ?, +, 4, -, -, ?, ?),
    atom_phrase(//, ?),
    atom_phrase(//, ?, ?),
    bracketed(//, ?, ?),
    bracketed(+, //, ?, ?),
    bracketed0(+, //, ?, ?),
    dcg(//),
    dcg_apply(//,+, ?, ?),
    dcg_apply_cp(//, +, ?, ?),
    dcg_atom(3, ?, ?, ?),
    dcg_between(//, //, ?, ?),
    dcg_between(//, //, //, ?, ?),
    dcg_call(//, ?, ?),
    dcg_call(3, ?, ?, ?),
    dcg_call(4, ?, ?, ?, ?),
    dcg_call(5, ?, ?, ?, ?, ?),
    dcg_call(6, ?, ?, ?, ?, ?, ?),
    dcg_call(7, ?, ?, ?, ?, ?, ?, ?),
    dcg_call_cp(//, ?, ?),
    dcg_call_cp(3, ?, ?, ?),
    dcg_call_cp(4, ?, ?, ?, ?),
    dcg_call_cp(5, ?, ?, ?, ?, ?),
    dcg_call_cp(6, ?, ?, ?, ?, ?, ?),
    dcg_call_cp(7, ?, ?, ?, ?, ?, ?, ?),
    dcg_goal(0, ?, ?),
    dcg_max_width(3, +, -),
    dcg_once(//, ?, ?),
    dcg_string(3, ?, ?, ?),
    dcg_width(//, -),
    dcg_with_output_to(+, //),
    debug(+, //),
    def(3, -, +, ?, ?),
    indent(+, //, ?, ?),
    indent_nl(+, //, ?, ?),
    opt(//, ?, ?),
    opt(3, ?, ?, ?),
    quoted(//, ?, ?),
    quoted(//, //, ?, ?),
    quoted(?, //, //, ?, ?),
    section(+, +, //, ?, ?),
    seplist(//, //, ?, ?),
    seplist(3, //, +, ?, ?),
    string_phrase(//, ?),
    string_phrase(//, ?, ?),
    tab(+, //, ?, ?),
    tab_nl(+, //, ?, ?).

:- setting(tab_size, integer, 8,
     'The number of spaces that go into one tab.'
   ).





%! ...// .
% Wrapper around ...//1 that does not return the processed codes.

... --> ...(_).


%! ...(-Codes:list(code))// .
% Wrapper around string//1.

...(Cs) --> string(Cs).



%! ?(:Dcg_0)// is det.
%! ?(:Dcg_1, -Args1)// is det.
%! ?(:Dcg_2, -Args1, -Args2)// is det.

?(Dcg_0) --> 'm*n'(0, 1, Dcg_0).
?(Dcg_1, L1) --> 'm*n'(0, 1, Dcg_1, L1).
?(Dcg_2, L1, L2) --> 'm*n'(0, 1, Dcg_2, L1, L2).



%! *(:Dcg_0)// is det.
%! *(:Dcg_1, -Args1)// is det.
%! *(:Dcg_2, -Args1, -Args2)// is det.

*(Dcg_0) --> 'm*n'(0, _, Dcg_0).
*(Dcg_1, L1) --> 'm*n'(0, _, Dcg_1, L1).
*(Dcg_2, L1, L2) --> 'm*n'(0, _, Dcg_2, L1, L2).



%! #(?Occurrences, :Dcg_0)// is det.
%! #(?Occurrences, :Dcg_1, -Args1)// is det.
%! #(?Occurrences, :Dcg_2, -Args1, -Args2)// is det.

#(N, Dcg_0) --> 'm*n'(N, N, Dcg_0).
#(N, Dcg_1, L1) --> 'm*n'(N, N, Dcg_1, L1).
#(N, Dcg_2, L1, L2) --> 'm*n'(N, N, Dcg_2, L1, L2).



%! '*n'(?High, :Dcg_0)// is det.
%! '*n'(?High, :Dcg_1, -Args1)// is det.
%! '*n'(?High, :Dcg_2, -Args1, -Args2)// is det.

+(Dcg_0) --> 'm*n'(1, _, Dcg_0).
+(Dcg_1, L1) --> 'm*n'(1, _, Dcg_1, L1).
+(Dcg_2, L1, L2) --> 'm*n'(1, _, Dcg_2, L1, L2).



%! '*n'(?High, :Dcg_0)// is det.
%! '*n'(?High, :Dcg_1, -Args1)// is det.
%! '*n'(?High, :Dcg_2, -Args1, -Args2)// is det.

'*n'(High, Dcg_0) --> 'm*n'(_, High, Dcg_0).
'*n'(High, Dcg_1, L1) --> 'm*n'(_, High, Dcg_1, L1).
'*n'(High, Dcg_2, L1, L2) --> 'm*n'(_, High, Dcg_2, L1, L2).



%! 'm*'(?Low, :Dcg_0)// is det.
%! 'm*'(?Low, :Dcg_1, -Args1)// is det.
%! 'm*'(?Low, :Dcg_2, -Args1, -Args2)// is det.

'm*'(Low, Dcg_0) --> 'm*n'(Low, _, Dcg_0).
'm*'(Low, Dcg_1, L1) --> 'm*n'(Low, _, Dcg_1, L1).
'm*'(Low, Dcg_2, L1, L2) --> 'm*n'(Low, _, Dcg_2, L1, L2).



%! 'm*n'(?Low, ?High, :Dcg_0)// is det.
%! 'm*n'(?Low, ?High, :Dcg_1, -Args1)// is det.
%! 'm*n'(?Low, ?High, :Dcg_2, -Args1, -Args2)// is det.

'm*n'(Low, High, Dcg_0) -->
  parsing, !,
  'm*n__p'(Low, High, 0, Dcg_0).
'm*n'(Low, High, Dcg_0) -->
  'm*n__g'(Low, High, 0, Dcg_0).

'm*n__g'(Low, _, Count, _) -->
  {(var(Low) -> true ; Low =< Count)}.
'm*n__g'(Low, High, Count1, Dcg_0) -->
  {(var(High) -> true ; Count1 < High)},
  Dcg_0, !,
  {Count2 is Count1 + 1},
  'm*n__g'(Low, High, Count2, Dcg_0).

'm*n__p'(Low, High, Count1, Dcg_0) -->
  {(var(High) -> true ; Count1 < High)},
  Dcg_0, !,
  {Count2 is Count1 + 1},
  'm*n__p'(Low, High, Count2, Dcg_0).
'm*n__p'(Low, _, Count, _) -->
  {(var(Low) -> true ; Low =< Count)}.


'm*n'(Low, High, Dcg_1, L1) -->
  parsing, !,
  'm*n__p'(Low, High, 0, Dcg_1, L1).
'm*n'(Low, High, Dcg_1, L1) -->
  'm*n__g'(Low, High, 0, Dcg_1, L1).

'm*n__g'(Low, _, Count, _, []) -->
  {(var(Low) -> true ; Low =< Count)}.
'm*n__g'(Low, High, Count1, Dcg_1, [H1|T1]) -->
  {(var(High) -> true ; Count1 < High)},
  dcg_call(Dcg_1, H1), !,
  {Count2 is Count1 + 1},
  'm*n__g'(Low, High, Count2, Dcg_1, T1).

'm*n__p'(Low, High, Count1, Dcg_1, [H1|T1]) -->
  {(var(High) -> true ; Count1 < High)},
  dcg_call(Dcg_1, H1), !,
  {Count2 is Count1 + 1},
  'm*n__p'(Low, High, Count2, Dcg_1, T1).
'm*n__p'(Low, _, Count, _, []) -->
  {(var(Low) -> true ; Low =< Count)}.


'm*n'(Low, High, Dcg_2, L1, L2) -->
  parsing, !,
  'm*n__p'(Low, High, 0, Dcg_2, L1, L2).
'm*n'(Low, High, Dcg_2, L1, L2) -->
  'm*n__g'(Low, High, 0, Dcg_2, L1, L2).

'm*n__g'(Low, _, Count, _, [], []) -->
  {(var(Low) -> true ; Low =< Count)}.
'm*n__g'(Low, High, Count1, Dcg_2, [H1|T1], [H2|T2]) -->
  {(var(High) -> true ; Count1 < High)},
  dcg_call(Dcg_2, H1, H2), !,
  {Count2 is Count1 + 1},
  'm*n__g'(Low, High, Count2, Dcg_2, T1, T2).

'm*n__p'(Low, High, Count1, Dcg_2, [H1|T1], [H2|T2]) -->
  {(var(High) -> true ; Count1 < High)},
  dcg_call(Dcg_2, H1, H2), !,
  {Count2 is Count1 + 1},
  'm*n__p'(Low, High, Count2, Dcg_2, T1, T2).
'm*n__p'(Low, _, Count, _, [], []) -->
  {(var(Low) -> true ; Low =< Count)}.



%! atom_ellipsis(+A, +Max)// .

atom_ellipsis(A1, Max) -->
  {
    Len is Max - 1,
    atom_truncate(A1, Len, A2)
  },
  atom(A2),
  ({A1 == A2} -> "" ; "…").



%! atom_phrase(:Dcg_0, ?A)// is nondet.
% @throws instantiation_error
% @throws type_error

atom_phrase(Dcg_0, A):-
  var(A), !,
  phrase(Dcg_0, Cs),
  atom_codes(A, Cs).
atom_phrase(Dcg_0, A):-
  must_be(atom, A),
  atom_codes(A, Cs),
  phrase(Dcg_0, Cs).


%! atom_phrase(:Dcg_0, +A1, ?A2)// is nondet.
% @throws instantiation_error
% @throws type_error

atom_phrase(Dcg_0, A1, A2):-
  must_be(atom, A1),
  atom_codes(A1, Cs1),
  phrase(Dcg_0, Cs1, Cs2),
  atom_codes(A2, Cs2).



%! between(+Low:integer, +High:integer)// .
%! between(+Low:integer, +High:integer, ?Value:integer)// .
% Consume integers between the given lower and higher bounds.

between(Low, High) --> between(Low, High, _).
between(Low, High, N) --> integer(N), {between(Low, High, N)}.



%! between_digit(+LowWeight:hex, +HighWeight:hex)// .
%! between_digit(+LowWeight:hex, +HighWeight:hex, -Weight:between(0,15))// .
%! between_digit(
%!   +LowWeight:hex,
%!   +HighWeight:hex,
%!   -Weight:between(0,15),
%!   -Code:code
%! )// .
% Consume digits between the given lower and higher bounds.
%
% This supports digits of hexadecimal radix.

between_digit(Low, High) --> between_digit(Low, High, _).
between_digit(Low, High, W) --> between_digit(Low, High, W, _).
between_digit(Low, High, W, C) --> hexadecimal_digit(W, C), {between(Low, High, W)}.



%! between_radix(+Low:compound, +High:compound)// .

between_radix(Low, High) --> between_radix(Low, High, _).


%! between_radix(+Low:compound, +High:compound, ?Value:compound)// .
% Consume integers that are specified in various bases
% (binary, octal, decimal, hexadecimal)
% and that are between the lower and higher bounds.
%
% ### Example
%
% ```prolog
% ?- phrase(between_radix(bin(1001), hex(f), oct(X)), Codes).
% X = 11,
% Codes = [57] ;
% X = 12,
% Codes = [49, 48] ;
% X = 13,
% Codes = [49, 49] ;
% X = 14,
% Codes = [49, 50] ;
% X = 15,
% Codes = [49, 51] ;
% X = 16,
% Codes = [49, 52] ;
% X = 17,
% Codes = [49, 53].
% ```

between_radix(Low, High, Value) -->
  {
    radconv(Low, dec(LowDec)),
    radconv(High, dec(HighDec)),
    between(LowDec, HighDec, ValueDec)
  },
  integer(ValueDec),
  {radconv(dec(ValueDec), Value)}.



%! bit(?I:between(0,1))// .
% Wrapper around bit//2.
%! bit(?I:between(0,1), ?C)// .
% Binary digit.
%
% ```abnf
% BIT = "0" / "1"
% ```

bit(I) --> bit(I, _).
bit(0, 0'0) --> "0".
bit(1, 0'1) --> "1".



%! bracketed(:Dcg_0)// .
% Wrapper around bracketed//2 using round brackets.

bracketed(Dcg_0) -->
  bracketed(round, Dcg_0).


%! bracketed(+Type:oneof([angular,curly,round,square,ungular]), :Dcg_0)// is det.
%! bracketed(-Type:oneof([angular,curly,round,square,ungular]), :Dcg_0)// is nondet.
% The following bracket types are supported:
%   - `angular`
%   - `curly`
%   - `round`
%   - `square`
%   - `ungular`

bracketed(Type, Dcg_0) -->
  (   {var(Type)}
  ->  bracketed0(Type, Dcg_0)
  ;   dcg_once(bracketed0(Type, Dcg_0))
  ).

bracketed0(Type, Dcg_0) -->
  dcg_between(
    opening_bracket(Type, _),
    Dcg_0,
    closing_bracket(Type, _)
  ).



%! bs// is nondet.
%
% “Blankspace”
%
% Parses as blank//0; generates as space.

bs -->
  parsing, !,
  blank.
bs -->
  " ".



%! dcg(:Dcg_0) is det.

dcg(Dcg_0) :-
  dcg_with_output_to(user_output, Dcg_0).



%! dcg_apply(:Dcg, +Args)// .
% Variant of apply/2 for DCGs.

dcg_apply(Dcg, Args1, X, Y):-
  append(Args1, [X,Y], Args2),
  apply(Dcg, Args2).



%! dcg_apply_cp(:Dcg, +Args)// .
% Variant of dcg_apply/2 where copies of Dcg are called.

dcg_apply_cp(Dcg, Args1, X, Y):-
  copy_term(Dcg, Dcg_),
  append(Args1, [X,Y], Args2),
  apply(Dcg_, Args2).



%! dcg_atom(:Dcg_1, ?A)// .
% This meta-DCG rule handles the translation
% between the word and the character level of parsing/generating.
%
% Typically, grammar *A* specifies how words can be formed out of characters.
% A character is a code, and a word is a list of codes.
% Grammar *B* specifies how sentences can be built out of words.
% Now the word is an atom, and the sentences in a list of atoms.
%
% This means that at some point,
% words in grammar *A*, i.e. lists of codes,
% need to be translated to words in grammar *B*, i.e. atoms.
%
% This is where dcg_atom//2 comes in.
% We illustrate this with a schematic example:
% ```prolog
% sentence([W1,...,Wn]) -->
%   word2(W1),
%   ...,
%   word2(Wn).
%
% word2(W) -->
%   dcg_atom(word1, W).
%
% word1([C1, ..., Cn]) -->
%   char(C1),
%   ...,
%   char(Cn).
% ```
%
% @throws instantiation_error
% @throws type_error

dcg_atom(Dcg_1, A) -->
  {var(A)}, !,
  dcg_call(Dcg_1, Cs),
  {atom_codes(A, Cs)}.
dcg_atom(Dcg_1, A) -->
  {must_be(atom, A)}, !,
  {atom_codes(A, Cs)},
  dcg_call(Dcg_1, Cs).



%! dcg_between(:Between_0, :Dcg_0)// .

dcg_between(Between_0, Dcg_0) -->
  dcg_between(Between_0, Dcg_0, Between_0).


%! dcg_between(:Begin_0, :Dcg_0, :End_0)// .

dcg_between(Begin_0, Dcg_0, End_0) -->
  Begin_0,
  Dcg_0,
  End_0.



%! dcg_call(:Dcg_0)// .
%! dcg_call(:Dcg_1, ?Arg1)// .
%! dcg_call(:Dcg_2, ?Arg1, ?Arg2)// .
%! dcg_call(:Dcg_3, ?Arg1, ?Arg2, ?Arg3)// .
%! dcg_call(:Dcg_4, ?Arg1, ?Arg2, ?Arg3, ?Arg4)// .
%! dcg_call(:Dcg_5, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5)// .
% Dcg is called directly (i.e., not copied).
% This means that multiple calls of the same Dcg may share variables.
%
% This is a DCG-based  variant of call//[1-5].

dcg_call(Dcg_0, X, Y):-
  call(Dcg_0, X, Y).


dcg_call(Dcg_1, A1, X, Y):-
  call(Dcg_1, A1, X, Y).


dcg_call(Dcg_2, A1, A2, X, Y):-
  call(Dcg_2, A1, A2, X, Y).


dcg_call(Dcg_3, A1, A2, A3, X, Y):-
  call(Dcg_3, A1, A2, A3, X, Y).


dcg_call(Dcg_4, A1, A2, A3, A4, X, Y):-
  call(Dcg_4, A1, A2, A3, A4, X, Y).


dcg_call(Dcg_5, A1, A2, A3, A4, A5, X, Y):-
  call(Dcg_5, A1, A2, A3, A4, A5, X, Y).



%! dcg_call_cp(:Dcg_0)// .
%! dcg_call_cp(:Dcg_1, ?Arg1)// .
%! dcg_call_cp(:Dcg_2, ?Arg1, ?Arg2)// .
%! dcg_call_cp(:Dcg_3, ?Arg1, ?Arg2, ?Arg3)// .
%! dcg_call_cp(:Dcg_4, ?Arg1, ?Arg2, ?Arg3, ?Arg4)// .
%! dcg_call_cp(:Dcg_5, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5)// .
% Variant of dcg_call//[0-5] where copies of Dcg are called.
%
% dcg_call_cp//1 is included for consistency, even though
% it is operationally equivalent to dcg_call//1.

dcg_call_cp(Dcg_0, X, Y):-
  copy_term(Dcg_0, Dcg__0),
  call(Dcg__0, X, Y).


dcg_call_cp(Dcg_1, A1, X, Y):-
  copy_term(Dcg_1, Dcg__1),
  call(Dcg__1, A1, X, Y).


dcg_call_cp(Dcg_2, A1, A2, X, Y):-
  copy_term(Dcg_2, Dcg__2),
  call(Dcg__2, A1, A2, X, Y).


dcg_call_cp(Dcg_3, A1, A2, A3, X, Y):-
  copy_term(Dcg_3, Dcg__3),
  call(Dcg__3, A1, A2, A3, X, Y).


dcg_call_cp(Dcg_4, A1, A2, A3, A4, X, Y):-
  copy_term(Dcg_4, Dcg__4),
  call(Dcg__4, A1, A2, A3, A4, X, Y).


dcg_call_cp(Dcg_5, A1, A2, A3, A4, A5, X, Y):-
  copy_term(Dcg_5, Dcg__5),
  call(Dcg__5, A1, A2, A3, A4, A5, X, Y).



%! dcg_goal(:Goal_0)// is det.

dcg_goal(Goal_0) -->
  {with_output_to(codes(Cs), Goal_0)},
  Cs.



%! dcg_max_width(:Dcg_1, +Args, -MaxWidth:nonneg) is det.
%
% MaxWidth is the number of longest string that is the result of
% printing all Args with the DCG_1 rule.

dcg_max_width(Dcg_1, Args, MaxW):-
  aggregate_all(
    max(W),
    (
      member(Arg, Args),
      dcg_width(dcg_call_cp(Dcg_1, Arg), W)
    ),
    MaxW
  ).



%! dcg_once(:Dcg_0)// .
% Calls the given DCG at most one time.

dcg_once(Dcg_0, X, Y):-
  once(phrase(Dcg_0, X, Y)).



%! dcg_string(:Dcg_1, ?S)// .
% @see Variants of dcg_atom//2 that supports SWI7 strings.

dcg_string(Dcg_1, S) -->
  {var(S)}, !,
  dcg_call(Dcg_1, Cs),
  {string_codes(S, Cs)}.
dcg_string(Dcg_1, S) -->
  {must_be(string, S)}, !,
  {string_codes(S, Cs)},
  dcg_call(Dcg_1, Cs).



%! dcg_tab// is det.
% This is not named tab//0 since that would conflict with the built-in tab/2.

dcg_tab --> tab(1).



%! dcg_width(:Dcg_0, -Width:nonneg) is det.

dcg_width(Dcg_0, W):-
  dcg_with_output_to(codes(Cs), Dcg_0),
  length(Cs, W).



%! dcg_with_output_to(+Sink, :Dcg_0) is nondet.

dcg_with_output_to(Sink, Dcg_0):-
  phrase(Dcg_0, Cs),
  with_output_to(Sink, put_codes(Cs)).



%! debug(+Flag, :Dcg_0) is det.
% Write the first generation of Dcg_0 as a debug message with given Flag.

debug(Flag, Dcg_0):-
  debugging(Flag), !,
  string_phrase(Dcg_0, S),
  debug(Flag, "~s", [S]).
debug(_, _).



%! def(:Dcg_1, -Arg, +Def)// .

def(Dcg_1, Arg, _) --> dcg_call(Dcg_1, Arg), !.
def(_, Def, Def) --> [].



%! digit_code(?C)// .
% Wrapper around digit//2.

digit_code(C) --> digit(C, _).



%! done// .

done(_, _).



%! eol// .

eol --> "\n".
eol --> "\r\n".



%! frac_pos(+Fractional:between(0.0,1.0), -Ds:list(between(0,9))) is det.

frac_pos(Frac, Ds):-
  fractional_integer(Frac, I),
  sum_pos(I, Ds).



%! generate_as_digits(+N:nonneg, +NoDs)// is det.

generate_as_digits(N, M) -->
  generate_as_digits(N, 10, M).


%! generate_as_digits(+N:nonneg, +Base:positive_integer, +NoDs)// is det.

generate_as_digits(_, _, 0) --> !, [].
generate_as_digits(N1, Base, M1) -->
  {M2 is M1 - 1},
  {D is N1 // Base ^ M2},
  digit(D),
  {N2 is N1 mod Base ^ M2},
  generate_as_digits(N2, Base, M2).



%! generating// is semidet.
%
% Succeeds if currently generating a list of codes (rather than
% parsing a list of codes).

generating(X, Y):-
  \+ parsing(X, Y).



%! indent(+Indent:nonneg)// is det.

indent(0) --> !, "".
indent(N1) --> " ", !, {N2 is N1 - 1}, indent(N2).


%! indent(+Indent:nonneg, :Dcg_0)// is det.

indent(I, Dcg_0) --> indent(I), Dcg_0.


%! indent_nl(+Indent:nonneg, :Dcg_0)// is det.

indent_nl(I, Dcg_0) --> indent(I, Dcg_0), nl.



%! nl// is det.

nl --> "\n".



%! nonblank// .
% Wrapper around nonblank//1 from library(dcg/basics).

nonblank --> nonblank(_).



%! number// .
% Wrapper around number//1 from library(dcg/basics).

number --> number(_).



%! opt(:Dcg_0)// .

opt(Dcg_0) --> Dcg_0, !.
opt(_) --> [].


%! opt(:Dcg_1, ?Arg)// .

opt(Dcg_1, Arg) --> dcg_call(Dcg_1, Arg), !.
opt(_, _) --> [].



%! parsing// is semidet.
%
% Succeeds if currently parsing a list of codes (rather than
% generating a list of codes).

parsing(H, H):-
  nonvar(H).



%! perc(+Perc:between(0.0,1.0))// is det.
% Generates a human-readable representation of a percentage.

perc(Perc0, Head, Tail) :-
  Perc is floor(Perc0 * 100),
  format(codes(Head, Tail), "~d%", [Perc]).



%! perc_fixed(+Perc:between(0.0,1.0))// is det.

perc_fixed(Perc) -->
  ({Perc < 0.1} -> "  " ; {Perc < 1.0} -> " " ; ""),
  perc(Perc).


%! pos(+I:nonneg, -Ds:list(between(0,9))) is det.
% Wrapper around pois/2 with decimal base.

pos(I, Ds):- pos(I, 10, Ds).


%! pos(+I:nonneg, +Base:positive_integer, -Ds:list(between(0,9))) is det.

pos(I, Base, Ds):-
  pos_rev(I, Base, Ds0),
  reverse(Ds0, Ds).

pos_rev(I, Base, [I]):-
  I < Base, !.
pos_rev(I1, Base, [H|T]):-
  H is I1 mod Base,
  I2 is I1 // Base,
  pos_rev(I2, Base, T).



%! pos_frac(+Ds:list(between(0,9)), -FractionalPart:rational) is det.
% Positional fractional.

pos_frac(Ds, Frac):-
  aggregate_all(sum(Rat), (nth1(I, Ds, D), Rat is D rdiv (10 ^ I)), Frac).



%! pos_sum(+Ds:list(between(0,9)), -I:nonneg) is det.
% Positional summation.

pos_sum(Ds, I):- pos_sum(Ds, 10, I).


%! pos_sum(+Ds:list(between(0,9)), +Base:positive_integer, -I:nonneg) is det.

pos_sum(Ds, Base, I):- pos_sum(Ds, Base, 0, I).
pos_sum([D|Ds], Base, I1, I):- !,
  I2 is I1 * Base + D,
  pos_sum(Ds, Base, I2, I).
pos_sum([], _, I, I).



%! progress_bar(+Processed, +All)// is det.

progress_bar(M, N) -->
  progress_bar(M, N, 10),
  ({M =:= N} -> " [done]" ; []).


progress_bar(M, N, Width) -->
  {(N =:= 0 -> Perc = 1.0 ; float_div_zero(M, N, Perc))},
  perc(Perc),
  " ",
  {
    M0 is M * Width,
    int_div_zero(M0, N, MChars)
  },
  #(MChars, "="),
  {NChars is Width - MChars},
  #(NChars, "-"),
  " (",
  thousands(M),
  "/",
  thousands(N),
  ")".



%! quoted(:Content_2)// .

quoted(Goal_2) -->
  quoted(double_quote, Goal_2).


%! quoted(:Quote_2, :Content_2)// .

quoted(Quote_2, Goal_2) -->
  quoted(1, Quote_2, Goal_2).


%! quoted(?Length:positive_integer, :Quote_2, :Content_0)// .
%
% Typical values for Quote_0 are:
%
%   * double_quote//0
%
%   * single_quote//0

quoted(N, Quote_2, Content_2) -->
  {quote(Quote_2)},
  dcg_between(#(N, Quote_2), Content_2).

quote(_:Quote) :- var(Quote), quote_goal(Quote).

quote_goal(double_quote).
quote_goal(single_quote).



%! rest// is det.
%! rest(-Rest:list(code))// is det.
%
% Same as `rest --> "".'

rest(X, X).


rest(X, X, []).



%! section(+Indent:nonneg, +Message:string, :Dcg_0)// is det.

section(I, Msg, Dcg_0) --> tab_nl(I, atom(Msg)), Dcg_0.



%! seplist(:Dcg_0, :Sep_0)// is det.
%! seplist(:Dcg_1, :Sep_0, +L)// is det.

seplist(Dcg_0, Sep_0) -->
  Dcg_0,
  (Sep_0 -> seplist(Dcg_0, Sep_0) ; "").
seplist(_, _) --> !, "".


% The first clause cannot contain a cut after the separator, because
% the separator may also appear after the separated list.  For
% example, "a b " could no longer be parsed if the separator was a
% space.
seplist(Dcg_1, Sep_0, [H1,H2|T]) -->
  call(Dcg_1, H1),
  Sep_0,
  seplist(Dcg_1, Sep_0, [H2|T]).
seplist(Dcg_1, _, [H]) -->
  call(Dcg_1, H), !.
seplist(_, _, []) --> !, [].



%! skip_line// is det.

skip_line -->
  ...,
  eol, !.



%! str(+String)// is det.

str(S) -->
  {string_codes(S, Cs)},
  Cs.



%! str_ellipsis(+S, +Max)// is det.

str_ellipsis(S1, Max) -->
  {
    Len is Max - 1,
    string_truncate(S1, Len, S2)
  },
  str(S2),
  ({S1 == S2} -> "" ; "…").



%! string// .
% Wrapper around string//1.

string -->
  string(_).



%! string_phrase(:Dcg_0, ?S)// is nondet.
%! string_phrase(:Dcg_0, +S1, ?S2)// is nondet.

string_phrase(Dcg_0, S):-
  var(S), !,
  phrase(Dcg_0, Cs),
  string_codes(S, Cs).
string_phrase(Dcg_0, S):-
  must_be(string, S),
  string_codes(S, Cs),
  phrase(Dcg_0, Cs).


string_phrase(Dcg_0, S1, S2):-
  must_be(string, S1),
  string_codes(S1, Cs1),
  phrase(Dcg_0, Cs1, Cs2),
  string_codes(S2, Cs2).



%! string_without(+EndCodes:list(code))// .
% Wrapper around string_without//2.

string_without(End) -->
  string_without(End, _).



%! sum_pos(+N:nonneg, -Ds:list(between(0,9))) is det.
%! sum_pos(+N:nonneg, +Base:nonneg, -Ds:list(between(0,9))) is det.
%
% The default Base is decimal.

sum_pos(I, Ds):-
  sum_pos(I, 10, Ds).


sum_pos(I, Base, Ds):-
  sum_pos0(I, Base, Ds0),
  reverse(Ds0, Ds).

sum_pos0(0, _, []):- !.
sum_pos0(I1, Base, [H|T]):-
  H is I1 mod Base,
  I2 is I1 // Base,
  sum_pos0(I2, Base, T).



%! tab(+Indent:nonneg)// is det.
%! tab(+Indent:nonneg, :Dcg_2)// is det.

tab(I) --> {setting(tab_size, N0), N is I * N0}, indent(N).


tab(I, Dcg_0) --> tab(I), Dcg_0.



%! tab_nl(+Indent:nonneg, :Dcg_0)// is det.

tab_nl(I, Dcg_0) --> tab(I, Dcg_0), nl.



%! thousands(+I)// is det.

thousands(I) -->
  {format(atom(A), "~D", [I])},
  atom(A).



%! ws// is nondet.
%
% “Whitespace”
%
% Parses as white//0; generate as space.
%
% @see bs//0

ws -->
  parsing, !,
  white.
ws -->
  " ".
