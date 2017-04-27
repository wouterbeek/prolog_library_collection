:- module(
  dcg_ext,
  [
    '?'//1,                % :Dcg_0
    '?'//2,                % :Dcg_1, -Args1
    '?'//3,                % :Dcg_2, -Args1, -Args2
    '*'//3,                % :Dcg_2, -Args1, -Args2
    '+'//3,                % :Dcg_2, -Args1, -Args2
    '#'//4,                % ?Occurrences, :Dcg_2, -Args1, -Args2
    '*n'//4,               % ?High, :Dcg_2, -Args1, -Args2
    'm*'//2,               % ?Low, :Dcg_0
    'm*'//3,               % ?Low, :Dcg_1, -Args1
    'm*'//4,               % ?Low, :Dcg_2, -Args1, -Args2
    'm*n'//5,              % ?Low, ?High, :Dcg_2, -Args1, -Args2
    atom_ci//1,            % ?A
    atom_lower//1,         % ?A
    atom_title//1,         % ?A
    atom_upper//1,         % ?A
    atom_uppercase//0,
    atom_uppercase//1,     % +A
    between_code//2,       % +Low, +High
    between_code//3,       % +Low, +High, ?C
    between_code_rad//2,   % +RadLow, +RadHigh
    between_code_rad//3,   % +RadLow, +RadHigh, -C
    between_digit//2,      % +Low:hex, +High:hex
    between_digit//3,      % +Low:hex, +High:hex, -Weight:between(0,15)
    between_digit//4,      % +Low:hex, +High:hex, -Weight:between(0,15), -Code:code
    between_int//2,        % +Low:integer, +High:integer
    between_int//3,        % +Low:integer, +High:integer, ?Value:integer
    between_radix//2,      % +Low:compound, +High:compound
    between_radix//3,      % +Low:compound, +High:compound, ?Value:compound
    bit//1,                % ?I:between(0,1)
    bracketed//1,          % :Dcg_0
    bracketed//2,          % ?Type:oneof([angular,curly,round,square,ungular]), :Dcg_0
    bs//0,
    code//1,               % ?C
    code_ci//1,            % ?C
    code_lower//1,         % ?C
    code_rad//1,           % ?RadC
    code_rad//2,           % ?RadC, -C
    code_upper//1,         % ?C
    dcg/1,                 % :Dcg_0
    dcg_apply//2,          % :Dcg_1, +Args
    dcg_apply_cp//2,       % :Dcg_1, +Args
    dcg_atom//2,           % :Dcg_1, ?A
    dcg_between//2,        % :Between_0, :Dcg_0
    dcg_between//3,        % :Begin_0, :Dcg_0, :End_0
    dcg_call//3,           % :Dcg_2, ?Arg1, ?Arg2
    dcg_call//4,           % :Dcg_3, ?Arg1, ?Arg2, ?Arg3
    dcg_call//5,           % :Dcg_4, ?Arg1, ?Arg2, ?Arg3, ?Arg4
    dcg_call//6,           % :Dcg_5, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5
    dcg_call_bool//2,      % :Dcg_0, -Bool
    dcg_call_cp//1,        % :Dcg_0
    dcg_call_cp//2,        % :Dcg_1, ?Arg1
    dcg_call_cp//3,        % :Dcg_2, ?Arg1, ?Arg2
    dcg_call_cp//4,        % :Dcg_3, ?Arg1, ?Arg2, ?Arg3
    dcg_call_cp//5,        % :Dcg_4, ?Arg1, ?Arg2, ?Arg3, ?Arg4
    dcg_call_cp//6,        % :Dcg_5, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5
    dcg_dict//1,           % +Dict
    dcg_dict//2,           % :Dcg_1, +Dict
    dcg_dict//3,           % :Dcg_1, +Dict, +I
    dcg_goal//1,           % :Goal_0
    dcg_list//1,           % +L
    dcg_list//2,           % :Dcg_1, +L
    dcg_list//3,           % :Dcg_1, +L, +I
    dcg_max_width/3,       % :Dcg_1, +Args, -MaxWidth
    dcg_once//1,           % :Dcg_0
    dcg_once//2,           % :Dcg_1, +Arg1
    dcg_once//3,           % :Dcg_2, +Arg1, +Arg2
    dcg_string//2,         % :Dcg_1, ?S
    dcg_strip//0,
    dcg_strip//1,          % +StripCs
    dcg_tab//0,
    dcg_width/2,           % :Dcg_0, -Width
    def//3,                % :Dcg_1, -Arg, +Def
    digit_code//1,         % ?C
    done//0,
    dq//1,                 % :Dcg_0
    eol//0,
    frac_pos/2,            % +Frac:between(0.0,1.0), -Ds:list(between(0,9))
    generate_as_digits//2, % +N:nonneg, +NoDs
    generate_as_digits//3, % +N:nonneg, +Base:positive_integer, +NoDs
    generating//0,
    indent//2,             % +Indent:nonneg, :Dcg_0
    indent_nl//2,          % +Indent:nonneg, :Dcg_0
    lowercase//0,
    number//0,
    opt//1,                % :Dcg_0
    opt//2,                % :Dcg_1, ?Arg
    ordinal//1,            % +M
    pair//2,               % :Dcg_0, :Dcg_0
    perc//1,               % +Perc:between(0.0,1.0)
    perc_fixed//1,         % +Perc:between(0.0,1.0)
    pos/2,                 % +N:nonneg, -Ds:list(between(0,9))
    pos/3,                 % +N:nonneg, +Base, -Ds:list(between(0,9))
    pos_frac/2,            % +Ds:list(between(0,9)), -FracPart:rational
    progress_bar//2,       % +Processed, +All
    quad//4,               % :DcgX_0, :DcgY_0, :DcgZ_0, :DcgQ_0
    quoted//1,             % :Content_0
    quoted//2,             % :Quote_0, :Content_0
    quoted//3,             % ?Length, :Quote_0, :Content_0
    rest//0,
    rest//1,               % -Rest:list(code)
    set//1,                % +L
    set//2,                % :Dcg_1, +L
    skip_line//0,
    sq//1,                 % :Dcg_0
    str_ci//1,             % ?Str
    str_ellipsis//2,       % +S, +Max
    string//0,
    string_atom_phrase/3,  % :Dcg_0, ?S, ?A
    string_phrase/3,       % :Dcg_0, +S1, ?S2
    string_without//1,     % +EndCs
    sum_pos/2,             % +N:nonneg, -Ds:list(between(0,9))
    sum_pos/3,             % +N:nonneg, +Base:positive_integer, -Ds:list(nonneg)
    tab//1,                % +Indent:nonneg
    tab//2,                % +Indent:nonneg, :Dcg_0
    tab_nl//2,             % +Indent:nonneg, :Dcg_0
    triple//3,             % :DcgX_0, :DcgY_0, :DcgZ_0
    tuple//1,              % +L
    tuple//2,              % :Dcg_1, +L
    ws//0
  ]
).
:- reexport(library(dcg/basics), except([digit//1,digits//1])).
:- reexport(library(uri/rfc1738), [
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
@version 2015/11-2017/03
*/

:- use_module(library(aggregate)).
:- use_module(library(atom_ext)).
:- use_module(library(code_ext)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(list_ext)).
:- use_module(library(math/math_ext)).
:- use_module(library(math/radconv)).
:- use_module(library(pair_ext)).
:- use_module(library(settings)).
:- use_module(library(string_ext)).
:- use_module(library(uri/rfc1738), [
     hialpha//1, % ?C
     lowalpha//1 % ?C
   ]).

:- meta_predicate
    ?(//, ?, ?),
    ?(3, -, ?, ?),
    ?(4, -, -, ?, ?),
    *(4, -, -, ?, ?),
    +(4, -, -, ?, ?),
    #(+, 4, -, -, ?, ?),
    '*n'(?, 4, -, -, ?, ?),
    'm*'(?, //, ?, ?),
    'm*'(?, 3, -, ?, ?),
    'm*'(?, 4, -, -, ?, ?),
    'm*n'(?, ?, 4, -, -, ?, ?),
    'm*n__g'(?, ?, +, 4, -, -, ?, ?),
    'm*n__p'(?, ?, +, 4, -, -, ?, ?),
    bracketed(//, ?, ?),
    bracketed(+, //, ?, ?),
    bracketed0(+, //, ?, ?),
    dcg(//),
    dcg_apply(//,+, ?, ?),
    dcg_apply_cp(//, +, ?, ?),
    dcg_atom(3, ?, ?, ?),
    dcg_between(//, //, ?, ?),
    dcg_between(//, //, //, ?, ?),
    dcg_call(4, ?, ?, ?, ?),
    dcg_call(5, ?, ?, ?, ?, ?),
    dcg_call(6, ?, ?, ?, ?, ?, ?),
    dcg_call(7, ?, ?, ?, ?, ?, ?, ?),
    dcg_call_bool(//, -, ?, ?),
    dcg_call_cp(//, ?, ?),
    dcg_call_cp(3, ?, ?, ?),
    dcg_call_cp(4, ?, ?, ?, ?),
    dcg_call_cp(5, ?, ?, ?, ?, ?),
    dcg_call_cp(6, ?, ?, ?, ?, ?, ?),
    dcg_call_cp(7, ?, ?, ?, ?, ?, ?, ?),
    dcg_dict(3, +, ?, ?),
    dcg_dict(3, +, +, ?, ?),
    dcg_dict0(3, +, +, ?, ?),
    dcg_goal(0, ?, ?),
    dcg_list(3, +, ?, ?),
    dcg_list(3, +, +, ?, ?),
    dcg_max_width(3, +, -),
    dcg_once(//, ?, ?),
    dcg_once(//, +, ?, ?),
    dcg_once(//, +, +, ?, ?),
    dcg_string(3, ?, ?, ?),
    dcg_width(//, -),
    def(3, -, +, ?, ?),
    dq(//, ?, ?),
    indent(+, //, ?, ?),
    indent_nl(+, //, ?, ?),
    opt(//, ?, ?),
    opt(3, ?, ?, ?),
    pair(//, //, ?, ?),
    quad(//, //, //, //, ?, ?),
    quoted(//, ?, ?),
    quoted(//, //, ?, ?),
    quoted(?, //, //, ?, ?),
    set(3, +, ?, ?),
    sq(//, ?, ?),
    string_atom_phrase(//, ?, ?),
    string_phrase(//, ?, ?),
    tab(+, //, ?, ?),
    tab_nl(+, //, ?, ?),
    triple(//, //, //, ?, ?),
    tuple(3, +, ?, ?).

:- multifile
    dcg:dcg_hook//1.

dcg:dcg_hook(perc(Term)) -->
  perc_fixed(Term).
dcg:dcg_hook(set(L)) -->
  set(L).
dcg:dcg_hook(string(Str)) -->
  atom(Str).
dcg:dcg_hook(thousands(N)) -->
  thousands(N).

:- setting(
     tab_size,
     integer,
     2,
     "The number of spaces that go into one tab."
   ).





%! ?(:Dcg_0)// is det.
%! ?(:Dcg_1, -Args1)// is det.
%! ?(:Dcg_2, -Args1, -Args2)// is det.

?(Dcg_0) -->
  'm*n'(0, 1, Dcg_0).

?(Dcg_1, L1) -->
  'm*n'(0, 1, Dcg_1, L1).

?(Dcg_2, L1, L2) -->
  'm*n'(0, 1, Dcg_2, L1, L2).



%! *(:Dcg_2, -Args1, -Args2)// is det.

*(Dcg_2, L1, L2) -->
  'm*n'(0, _, Dcg_2, L1, L2).



%! #(?Occurrences, :Dcg_2, -Args1, -Args2)// is det.

#(N, Dcg_2, L1, L2) -->
  'm*n'(N, N, Dcg_2, L1, L2).



+(Dcg_2, L1, L2) -->
  'm*n'(1, _, Dcg_2, L1, L2).



%! '*n'(?High, :Dcg_2, -Args1, -Args2)// is det.

'*n'(High, Dcg_2, L1, L2) -->
  'm*n'(_, High, Dcg_2, L1, L2).



%! 'm*'(?Low, :Dcg_0)// is det.
%! 'm*'(?Low, :Dcg_1, -Args1)// is det.
%! 'm*'(?Low, :Dcg_2, -Args1, -Args2)// is det.

'm*'(Low, Dcg_0) -->
  'm*n'(Low, _, Dcg_0).

'm*'(Low, Dcg_1, L1) -->
  'm*n'(Low, _, Dcg_1, L1).

'm*'(Low, Dcg_2, L1, L2) -->
  'm*n'(Low, _, Dcg_2, L1, L2).



%! 'm*n'(?Low, ?High, :Dcg_2, -Args1, -Args2)// is det.

'm*n'(Low, High, Dcg_2, L1, L2) -->
  parsing, !,
  'm*n__p'(Low, High, 0, Dcg_2, L1, L2).
'm*n'(Low, High, Dcg_2, L1, L2) -->
  'm*n__g'(Low, High, 0, Dcg_2, L1, L2).

'm*n__g'(Low, _, Count, _, [], []) -->
  {(var(Low) -> true ; Low =< Count)}.
'm*n__g'(Low, High, Count1, Dcg_2, [H1|T1], [H2|T2]) -->
  {(var(High) -> true ; Count1 < High)},
  dcg_call(Dcg_2, H1, H2),
  {Count2 is Count1 + 1},
  'm*n__g'(Low, High, Count2, Dcg_2, T1, T2).

'm*n__p'(Low, High, Count1, Dcg_2, [H1|T1], [H2|T2]) -->
  {(var(High) -> true ; Count1 < High)},
  dcg_call(Dcg_2, H1, H2),
  {Count2 is Count1 + 1},
  'm*n__p'(Low, High, Count2, Dcg_2, T1, T2).
'm*n__p'(Low, _, Count, _, [], []) -->
  {(var(Low) -> true ; Low =< Count)}.



%! atom_ci(?A)// .
%
% ```prolog
% ?- phrase(atom_ci(http), Cs).
% Cs = "HTTP" ;
% Cs = "HTTp" ;
% Cs = "HTtP" ;
% Cs = "HTtp" ;
% Cs = "HtTP" ;
% Cs = "HtTp" ;
% Cs = "HttP" ;
% Cs = "Http" ;
% Cs = "hTTP" ;
% Cs = "hTTp" ;
% Cs = "hTtP" ;
% Cs = "hTtp" ;
% Cs = "htTP" ;
% Cs = "htTp" ;
% Cs = "httP" ;
% Cs = "http" ;
% false.
% ```

atom_ci(A) -->
  {ground(A)}, !,
  {atom_codes(A, Cs)},
  *(code_ci, Cs).
atom_ci(A) -->
  *(code_ci, Cs),
  {atom_codes(A, Cs)}.



%! atom_lower(?A)// .
%
% Generate/parse a lower-case atom, i.e. one consisting of all
% characters except for uppercase letters.

atom_lower(A) -->
  {ground(A)}, !,
  {atom_codes(A, Cs)},
  *(code_lower, Cs).
atom_lower(A) -->
  *(code_lower, Cs),
  {atom_codes(A, Cs)}.



%! atom_title(?A) // .

atom_title(A) -->
  generating, !,
  {atom_codes(A, [C|Cs])},
  hialpha(C),
  *(lowalpha, Cs).
atom_title(A) -->
  hialpha(C),
  *(lowalpha, Cs),
  {atom_codes(A, [C|Cs])}.
atom_title('') --> !, "".



%! atom_upper(?A)// .

atom_upper(A) -->
  {ground(A)}, !,
  {atom_codes(A, Cs)},
  *(code_upper, Cs).
atom_upper(A) -->
  *(code_upper, Cs),
  {atom_codes(A, Cs)}.



%! atom_uppercase// is det.
%! atom_uppercase(+A)// is det.

atom_uppercase, [Up] -->
  [Low],
  {code_type(Up, to_upper(Low))}, !,
  rest.
atom_uppercase --> "".


atom_uppercase(A) -->
  {atom_codes(A, Cs)},
  atom_uppercase_codes(Cs).


atom_uppercase_codes([]) --> !, [].
atom_uppercase_codes([H|T]) -->
  code_upper(H),
  *(code, T).



%! between_code(+Low, +High)// .
%! between_code(+Low, +High, ?C)// .

between_code(Low, High) -->
  between_code(Low, High, _).


between_code(Low, High, C) -->
  [C],
  {between(Low, High, C)}.



%! between_code_rad(+Low, +High)// .
%! between_code_rad(+Low, +High, -C)// .
%
% Parses or generates a code between the given numbers.

between_code_rad(Low, High) -->
  between_code_rad(Low, High, _).


between_code_rad(dec(Low), dec(High), C) --> !,
  between_code(Low, High, C).
between_code_rad(Low1, High1, C) -->
  {
    radconv(Low1, dec(Low2)),
    radconv(High1, dec(High2))
  },
  between_code_rad(dec(Low2), dec(High2), C).



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

between_digit(Low, High) -->
  between_digit(Low, High, _).
between_digit(Low, High, W) -->
  between_digit(Low, High, W, _).
between_digit(Low, High, W, C) -->
  hexadecimal_digit(W, C),
  {between(Low, High, W)}.



%! between_int(+Low:integer, +High:integer)// .
%! between_int(+Low:integer, +High:integer, ?Value:integer)// .
%
% Consume integers between the given lower and higher bounds.

between_int(Low, High) -->
  between_int(Low, High, _).


between_int(Low, High, N) -->
  integer(N),
  {between(Low, High, N)}.



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



%! code(?C)// .
%
% Useful in meta-predicates.

code(C) -->
  [C].



%! code_ci(+C)// is multi.
%! code_ci(-C)// is nondet.
% Writes case-insensitive variants of the given code.
% Generates the upper- and lowercase variants of a given letter
% (in no particular order).
%
% ```prolog
% ?- phrase(code_ci(66), Cs).
% Cs = "b" ;
% Cs = "B".
% ?- phrase(code_ci(98), Cs).
% Cs = "B" ;
% Cs = "b".
%
% ```
%
% Parses letters returning the chracter codes of their
% lower- and upper-case variants (in no particular order).
%
% ```prolog
% ?- phrase(code_ci(X), `b`).
% X = 66 ;
% X = 98.
% ?- phrase(code_ci(X), `B`).
% X = 98 ;
% X = 66.
% ```
%
% This can be used to process all case-variants of a given string.
%
% ```prolog
% ?- phrase(*(code_ci, `http`, []), Cs).
% Cs = "HTTP" ;
% Cs = "HTTp" ;
% Cs = "HTtP" ;
% Cs = "HTtp" ;
% Cs = "HtTP" ;
% Cs = "HtTp" ;
% Cs = "HttP" ;
% Cs = "Http" ;
% Cs = "hTTP" ;
% Cs = "hTTp" ;
% Cs = "hTtP" ;
% Cs = "hTtp" ;
% Cs = "htTP" ;
% Cs = "htTp" ;
% Cs = "httP" ;
% Cs = "http" ;
% false.
% ```
%
% The latter comes close to using atom_ci//1.

code_ci(C) -->
  {var(C)}, !,
  [C0],
  {code_ci(C0, C)}.
code_ci(C) -->
  {code_ci(C, C0)},
  [C0].



%! code_lower(+C)// is det.
%! code_lower(-C)// is nondet.
%
% Parses letters and returns their lower-case character code.
%
% ```prolog
% ?- phrase(code_lower(X), `A`).
% X = 97.
% ?- phrase(code_lower(X), `a`).
% X = 97.
% ```
%
% Generates the lower-case letter that is identical to the given
% lower-case letter or that is the lower-case variant of the given
% upper-case letter.
%
% ```prolog
% ?- phrase(code_lower(65), Cs).
% Cs = "a".
% ?- phrase(code_lower(97), Cs).
% Cs = "a".
% ```

code_lower(C) -->
  {ground(C)}, !,
  {to_lower(C, Lower)},
  [Lower].
code_lower(Lower) -->
  [C],
  {code_type(C, upper(Lower))}.



%! code_rad(+RadC)// .
%! code_rad(+RadC, -C)// .
%
% Emits a single code and allows the code to be represented in one of
% the following bases:
%
%   * bin(+nonneg)
%
%   * dec(+nonneg)
%
%   * hex(+atom)
%
%   * oct(+nonneg)

code_rad(RadC) -->
  code_rad(RadC, _).


code_rad(RadC, C) -->
  {var(RadC)}, !,
  [C],
  {radconv(RadC, dec(C))}.
code_rad(RadC, C) -->
  {radconv(RadC, dec(C))},
  [C].



%! code_upper(+C)// is det.
%! code_upper(-C)// is nondet.
%
% Parses upper-case letters and returns their lower- and upper-case
% character code (in that order).
%
% ```prolog
% ?- phrase(code_upper(X), `A`).
% X = 65 ;
% ?- phrase(code_upper(X), `a`).
% X = 65.
% ```
%
% Generates the upper-case letter that is identical to the given
% upper-case letter or that is the upper-case version of the given
% lower-case letter.
%
% ```prolog
% ?- phrase(code_upper(65), Cs).
% Cs = "A".
% ?- phrase(code_upper(97), Cs).
% Cs = "A".
% ```

code_upper(C) -->
  {ground(C)}, !,
  {to_upper(C, Upper)},
  [Upper].
code_upper(Upper) -->
  [C],
  {code_type(C, upper(Upper))}.



%! dcg(:Dcg_0) is det.

dcg(Dcg_0) :-
  dcg_with_output_to(current_output, Dcg_0).



%! dcg_apply(:Dcg, +Args)// .
% Variant of apply/2 for DCGs.

dcg_apply(Dcg, Args1, X, Y) :-
  append(Args1, [X,Y], Args2),
  apply(Dcg, Args2).



%! dcg_apply_cp(:Dcg, +Args)// .
%
% Variant of dcg_apply/2 where copies of Dcg are called.

dcg_apply_cp(Dcg, Args1, X, Y) :-
  copy_term(Dcg, Dcg_),
  append(Args1, [X,Y], Args2),
  apply(Dcg_, Args2).



%! dcg_atom(:Dcg_1, ?A)// .
%
% This meta-DCG rule handles the translation between the word and the
% character level of parsing/generating.
%
% Typically, grammar *A* specifies how words can be formed out of
% characters.  A character is a code, and a word is a list of codes.
% Grammar *B* specifies how sentences can be built out of words.  Now
% the word is an atom, and the sentences in a list of atoms.
%
% This means that at some point, words in grammar *A*, i.e. lists of
% codes, need to be translated to words in grammar *B*, i.e. atoms.
%
% This is where dcg_atom//2 comes in.  We illustrate this with a
% schematic example:
%
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
%! dcg_between(:Begin_0, :Dcg_0, :End_0)// .

dcg_between(Between_0, Dcg_0) -->
  dcg_between(Between_0, Dcg_0, Between_0).


dcg_between(Begin_0, Dcg_0, End_0) -->
  Begin_0,
  Dcg_0,
  End_0.



%! dcg_call(:Dcg_2, ?Arg1, ?Arg2)// .
%! dcg_call(:Dcg_3, ?Arg1, ?Arg2, ?Arg3)// .
%! dcg_call(:Dcg_4, ?Arg1, ?Arg2, ?Arg3, ?Arg4)// .
%! dcg_call(:Dcg_5, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5)// .
%
% Dcg is called directly (i.e., not copied).  This means that multiple
% calls of the same Dcg may share variables.
%
% This is a DCG-based  variant of call//[1-5].

dcg_call(Dcg_2, Arg1, Arg2, X, Y) :-
  call(Dcg_2, Arg1, Arg2, X, Y).


dcg_call(Dcg_3, Arg1, Arg2, Arg3, X, Y) :-
  call(Dcg_3, Arg1, Arg2, Arg3, X, Y).


dcg_call(Dcg_4, Arg1, Arg2, Arg3, Arg4, X, Y) :-
  call(Dcg_4, Arg1, Arg2, Arg3, Arg4, X, Y).


dcg_call(Dcg_5, Arg1, Arg2, Arg3, Arg4, Arg5, X, Y) :-
  call(Dcg_5, Arg1, Arg2, Arg3, Arg4, Arg5, X, Y).



%! dcg_call_bool(:Dcg_0, -Bool)// is det.

dcg_call_bool(Dcg_0, true) -->
  dcg_once(Dcg_0), !.
dcg_call_bool(_, false) --> "".



%! dcg_call_cp(:Dcg_0)// .
%! dcg_call_cp(:Dcg_1, ?Arg1)// .
%! dcg_call_cp(:Dcg_2, ?Arg1, ?Arg2)// .
%! dcg_call_cp(:Dcg_3, ?Arg1, ?Arg2, ?Arg3)// .
%! dcg_call_cp(:Dcg_4, ?Arg1, ?Arg2, ?Arg3, ?Arg4)// .
%! dcg_call_cp(:Dcg_5, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5)// .
%
% Variant of dcg_call//[0-5] where copies of Dcg are called.
%
% dcg_call_cp//1 is included for consistency, even though it is
% operationally equivalent to dcg_call//1.

dcg_call_cp(Dcg_0, X, Y) :-
  copy_term(Dcg_0, Dcg__0),
  call(Dcg__0, X, Y).


dcg_call_cp(Dcg_1, A1, X, Y) :-
  copy_term(Dcg_1, Dcg__1),
  call(Dcg__1, A1, X, Y).


dcg_call_cp(Dcg_2, A1, A2, X, Y) :-
  copy_term(Dcg_2, Dcg__2),
  call(Dcg__2, A1, A2, X, Y).


dcg_call_cp(Dcg_3, A1, A2, A3, X, Y) :-
  copy_term(Dcg_3, Dcg__3),
  call(Dcg__3, A1, A2, A3, X, Y).


dcg_call_cp(Dcg_4, A1, A2, A3, A4, X, Y) :-
  copy_term(Dcg_4, Dcg__4),
  call(Dcg__4, A1, A2, A3, A4, X, Y).


dcg_call_cp(Dcg_5, A1, A2, A3, A4, A5, X, Y) :-
  copy_term(Dcg_5, Dcg__5),
  call(Dcg__5, A1, A2, A3, A4, A5, X, Y).



%! dcg_dict(+Dict)// is det.
%! dcg_dict(:Dcg_1, +Dict)// is det.
%! dcg_dict(:Dcg_1, +Dict, +I)// is det.

dcg_dict(Dict) -->
  dcg_dict(pl_term, Dict).


dcg_dict(Dcg_1, Dict) -->
  dcg_dict(Dcg_1, Dict, 1).


dcg_dict(Dcg_1, Dict, I1) -->
  {I2 is I1 + 1},
  dcg_dict0(Dcg_1, Dict, I2).


dcg_dict0(Dcg_1, Dict, I) -->
  {dict_pairs(Dict, Pairs)},
  (   % Empty terms do not use lucious spacing.
      {is_empty_term0(Dict)}
  ->  tab(I),
      "{}"
  ;   % Singleton term do not use lucious spacing if the member is
      % singleton.
      {is_singleton_term0(Dict)}
  ->  tab(I),
      "{ ",
      {Pairs = [Key-Val]},
      dcg_entry0(Dcg_1, Key-Val, 0),
      " }"
  ;   "{",
      nl,
      dcg_entries0(Dcg_1, Pairs, I),
      "}"
  ).

dcg_dict_or_list0(Dcg_1, Val, I) -->
  {is_dict(Val)}, !,
  dcg_dict0(Dcg_1, Val, I).
dcg_dict_or_list0(Dcg_1, Val, I) -->
  {is_list(Val)}, !,
  dcg_list(Dcg_1, Val, I).

dcg_entries0(Dcg_1, [H1,H2|T], I) --> !,
  dcg_entry0(Dcg_1, H1, I),
  ",", nl,
  dcg_entries0(Dcg_1, [H2|T], I).
dcg_entries0(Dcg_1, [H], I) --> !,
  dcg_entry0(Dcg_1, H, I),
  nl,
  dcg_entries0(Dcg_1, [], I).
dcg_entries0(_, [], I1) --> !,
  {I2 is I1 - 1},
  tab(I2).

dcg_entry0(Dcg_1, Key-Val, I1) --> !,
  tab(I1),
  dcg_call(Dcg_1, Key), ": ",
  (   {is_dict_or_list0(Val)}
  ->  % Newline and additional indentation before dictionary or list
      % values that are non-empty and non-singleton.
      ({(is_empty_or_singleton_term0(Val))} -> {I2 = 0} ; {I2 is I1 + 1}),
      dcg_dict_or_list0(Dcg_1, Val, I2)
  ;   dcg_call(Dcg_1, Val)
  ).
dcg_entry0(Dcg_1, Elem, I) -->
  tab(I),
  dcg_dict_or_list0(Dcg_1, Elem, I), !.
dcg_entry0(Dcg_1, Elem, I) -->
  tab(I),
  dcg_call(Dcg_1, Elem).

is_dict_or_list0(Term) :-
  is_dict(Term).
is_dict_or_list0(Term) :-
  is_list(Term).

is_empty_term0(Dict) :-
  is_dict(Dict), !,
  empty_dict(Dict).
is_empty_term0(L) :-
  is_list(L), !,
  empty_list(L).

is_empty_or_singleton_term0(Term) :-
  is_empty_term0(Term).
is_empty_or_singleton_term0(Term) :-
  is_singleton_term0(Term).

is_singleton_term0(Dict) :-
  is_dict(Dict), !,
  dict_pairs(Dict, Pairs),
  Pairs = [_-Val],
  is_singleton_term0(Val).
is_singleton_term0(L) :-
  is_list(L), !,
  singleton_list(Elem, L),
  is_singleton_term0(Elem).
is_singleton_term0(_).



%! dcg_goal(:Goal_0)// is det.

dcg_goal(Goal_0) -->
  {with_output_to(codes(Cs), Goal_0)},
  Cs.



%! dcg_list(+L)// is det.
%! dcg_list(:Dcg_1, +L)// is det.
%! dcg_list(:Dcg_1, +L, +I)// is det.

dcg_list(L) -->
  dcg_list(pl_term, L).


dcg_list(Dcg_1, L) -->
  dcg_list(Dcg_1, L, 0).


dcg_list(_, L, _) -->
  {empty_list(L)}, !,
  "[]".
dcg_list(Dcg_1, L, _) -->
  {
    singleton_list(Elem, L),
    is_singleton_term0(Elem)
  }, !,
  "[ ",
  dcg_entry0(Dcg_1, Elem, 0),
  " ]".
dcg_list(Dcg_1, L, I1) -->
  "[", nl,
  {I2 is I1 + 1},
  dcg_entries0(Dcg_1, L, I2),
  tab(I1), "]".



%! dcg_max_width(:Dcg_1, +Args, -MaxWidth:nonneg) is det.
%
% MaxWidth is the number of longest string that is the result of
% printing all Args with the DCG_1 rule.

dcg_max_width(Dcg_1, Args, MaxW) :-
  aggregate_all(
    max(W),
    (
      member(Arg, Args),
      dcg_width(dcg_call_cp(Dcg_1, Arg), W)
    ),
    MaxW
  ).



%! dcg_once(:Dcg_0)// is det.
%! dcg_once(:Dcg_1, +Arg1)// is det.
%! dcg_once(:Dcg_2, +Arg1, +Arg2)// is det.
%
% Calls the given DCG at most one time.

dcg_once(Dcg_0, X, Y) :-
  once(dcg_call(Dcg_0, X, Y)).


dcg_once(Dcg_1, Arg1, X, Y) :-
  once(dcg_call(Dcg_1, Arg1, X, Y)).


dcg_once(Dcg_2, Arg1, Arg2, X, Y) :-
  once(dcg_call(Dcg_2, Arg1, Arg2, X, Y)).



%! dcg_string(:Dcg_1, ?S)// .
%
% @see Variants of dcg_atom//2 that supports SWI7 strings.

dcg_string(Dcg_1, S) -->
  {var(S)}, !,
  dcg_call(Dcg_1, Cs),
  {string_codes(S, Cs)}.
dcg_string(Dcg_1, S) -->
  {must_be(string, S)}, !,
  {string_codes(S, Cs)},
  dcg_call(Dcg_1, Cs).



%! dcg_strip// is det.
%! dcg_strip(+StripCs)// is det.

dcg_strip -->
  dcg_strip_begin([9,10,32]).


dcg_strip(StripCs) -->
  dcg_strip_begin(StripCs).

dcg_strip_begin(StripCs) -->
  [C],
  {memberchk(C, StripCs)}, !,
  dcg_strip_begin(StripCs).
dcg_strip_begin(StripCs) -->
  dcg_strip_middle(StripCs).

dcg_strip_middle(StripCs) -->
  dcg_strip_end(StripCs), !.
dcg_strip_middle(StripCs), [C] -->
  [C],
  dcg_strip_middle(StripCs).

dcg_strip_end(StripCs) -->
  [C],
  {memberchk(C, StripCs)}, !,
  dcg_strip_end(StripCs).
dcg_strip_end(_) -->
  eos.



%! dcg_tab// is det.
% This is not named tab//0 since that would conflict with the built-in tab/2.

dcg_tab -->
  tab(1).



%! dcg_width(:Dcg_0, -Width:nonneg) is det.

dcg_width(Dcg_0, W) :-
  dcg_with_output_to(codes(Cs), Dcg_0),
  length(Cs, W).



%! def(:Dcg_1, -Arg, +Def)// .

def(Dcg_1, Arg, _) --> dcg_call(Dcg_1, Arg), !.
def(_, Def, Def) --> "".



%! digit_code(?C)// .
% Wrapper around digit//2.

digit_code(C) --> digit(C, _).



%! done// .

done(_, _).



%! dq(:Dcg_0)// is det.

dq(Dcg_0) -->
  "‘",
  Dcg_0,
  "’".



%! eol// .

eol --> "\n".
eol --> "\r\n".



%! frac_pos(+Fractional:between(0.0,1.0), -Ds:list(between(0,9))) is det.

frac_pos(Frac, Ds) :-
  fractional_integer(Frac, I),
  sum_pos(I, Ds).



%! generate_as_digits(+N:nonneg, +NoDs)// is det.

generate_as_digits(N, M) -->
  generate_as_digits(N, 10, M).


%! generate_as_digits(+N:nonneg, +Base:positive_integer, +NoDs)// is det.

generate_as_digits(_, _, 0) --> !, "".
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

generating(X, Y) :-
  \+ parsing(X, Y).



%! indent(+Indent:nonneg, :Dcg_0)// is det.

indent(I, Dcg_0) --> indent(I), Dcg_0.


%! indent_nl(+Indent:nonneg, :Dcg_0)// is det.

indent_nl(I, Dcg_0) --> indent(I, Dcg_0), nl.



%! lowercase// .

lowercase, [Low] -->
  [Up],
  {code_type(Low, to_lower(Up))}, !,
  rest.
lowercase --> "".



%! number// .
% Wrapper around number//1 from library(dcg/basics).

number --> number(_).



%! opt(:Dcg_0)// .

opt(Dcg_0) --> Dcg_0, !.
opt(_) --> "".


%! opt(:Dcg_1, ?Arg)// .

opt(Dcg_1, Arg) --> dcg_call(Dcg_1, Arg), !.
opt(_, _) --> "".



%! ordinal(+N)// .

ordinal(N) -->
  integer(N),
  ordinal_suffix(N).

ordinal_suffix(N) -->
  {N0 is N mod 10},
  (   {N0 =:= 1}
  ->  "st"
  ;   {N0 =:= 2}
  ->  "nd"
  ;   {N0 =:= 3}
  ->  "rd"
  ;   "th"
  ).



%! pair(:DcgX_0, :DcgY_0)// is det.

pair(DcgX_0, DcgY_0) -->
  "〈", DcgX_0, ",", DcgY_0, "〉".



%! perc(+Perc:between(0.0,1.0))// is det.
%
% Generates a human-readable representation of a percentage.

perc(Perc0, Head, Tail) :-
  Perc is floor(Perc0 * 100),
  format(codes(Head, Tail), "~d%", [Perc]).



%! perc_fixed(+Perc:between(0.0,1.0))// is det.
%
% Fixed-width variant of perc//1.

perc_fixed(Perc) -->
  ({Perc < 0.1} -> "  " ; {Perc < 1.0} -> " " ; ""),
  perc(Perc).


%! pos(+I:nonneg, -Ds:list(between(0,9))) is det.
% Wrapper around pois/2 with decimal base.

pos(I, Ds) :-
  pos(I, 10, Ds).


%! pos(+I:nonneg, +Base:positive_integer, -Ds:list(between(0,9))) is det.

pos(I, Base, Ds) :-
  pos_rev(I, Base, Ds0),
  reverse(Ds0, Ds).

pos_rev(I, Base, [I]) :-
  I < Base, !.
pos_rev(I1, Base, [H|T]) :-
  H is I1 mod Base,
  I2 is I1 // Base,
  pos_rev(I2, Base, T).



%! pos_frac(+Ds:list(between(0,9)), -FractionalPart:rational) is det.
% Positional fractional.

pos_frac(Ds, Frac) :-
  aggregate_all(sum(Rat), (nth1(I, Ds, D), Rat is D rdiv (10 ^ I)), Frac).



%! progress_bar(+Processed, +All)// is det.

progress_bar(M, N) -->
  progress_bar(M, N, 10),
  ({M =:= N} -> " [done]" ; "").


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



%! quad(:DcgX_0, :DcgY_0, :DcgZ_0, :Q_0)// is det.

quad(DcgX_0, DcgY_0, DcgZ_0, Q_0) -->
  "〈", DcgX_0, ",", DcgY_0, ",", DcgZ_0, ",", Q_0, "〉".



%! quoted(:Content_0)// .

quoted(Content_0) -->
  quoted(str("\""), Content_0).


%! quoted(:Quote_0, :Content_0)// .

quoted(Quote_0, Content_0) -->
  quoted(1, Quote_0, Content_0).


%! quoted(?Length:positive_integer, :Quote_0, :Content_0)// .
%
% Typical values for Quote_0 are:
%
%   * str("\"")
%
%   * str("'")

quoted(N, Quote_0, Content_0) -->
  dcg_between(#(N, Quote_0), Content_0).



%! rest// is det.
%! rest(-Rest:list(code))// is det.
%
% Same as `rest --> "".'

rest(X, X).


rest(X, X, []).



%! set(+L)// is det.
%! set(:Dcg_1, +L)// is det.

set(L) -->
  set(pl_term, L).


set(Dcg_1, L) -->
  "{",
  seplist(Dcg_1, ", ", L),
  "}".



%! skip_line// is det.

skip_line -->
  ...,
  eol, !.



%! sq(:Dcg_0)// is det.

sq(Dcg_0) -->
  "‘",
  Dcg_0,
  "’".



%! str_ci(?Str)// .

str_ci(Str) -->
  {ground(Str)}, !,
  {string_codes(Str, Cs)},
  *(code_ci, Cs).
str_ci(Str) -->
  *(code_ci, Cs),
  {string_codes(Str, Cs)}.



%! str_ellipsis(+Str, +MaxLen)// is det.

str_ellipsis(Str, MaxLen) -->
  {string_ellipsis(Str, MaxLen, Ellipsis)},
  atom(Ellipsis).



%! string// .
% Wrapper around string//1.

string -->
  string(_).



%! string_atom_phrase(:Dcg_0, ?S, ?A) is nondet.

string_atom_phrase(Dcg_0, S, A) :-
  string_codes(S, Cs1),
  phrase(Dcg_0, Cs1, Cs2),
  atom_codes(A, Cs2).



%! string_phrase(:Dcg_0, +S1, ?S2) is nondet.

string_phrase(Dcg_0, S1, S2) :-
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

sum_pos(I, Ds) :-
  sum_pos(I, 10, Ds).


sum_pos(I, Base, Ds) :-
  sum_pos0(I, Base, Ds0),
  reverse(Ds0, Ds).

sum_pos0(0, _, []) :- !.
sum_pos0(I1, Base, [H|T]) :-
  H is I1 mod Base,
  I2 is I1 // Base,
  sum_pos0(I2, Base, T).



%! tab(+Indent:nonneg)// is det.
%! tab(+Indent:nonneg, :Dcg_0)// is det.

tab(I) -->
  {
    setting(tab_size, N0),
    N is I * N0
  },
  indent(N).


tab(I, Dcg_0) -->
  tab(I),
  Dcg_0.



%! tab_nl(+Indent:nonneg, :Dcg_0)// is det.

tab_nl(I, Dcg_0) -->
  tab(I, Dcg_0),
  nl.



%! triple(:DcgX_0, :DcgY_0, :DcgZ_0)// is det.
% Wrapper around triple//4 using the default writer.

triple(DcgX_0, DcgY_0, DcgZ_0) -->
  "〈", DcgX_0, ",", DcgY_0, ",", DcgZ_0, "〉".



%! tuple(+L)// is det.
%! tuple(:Dcg_1, +L)// is det.
% Prints a tuple.

tuple(L) -->
  tuple(pl_term, L).


tuple(Dcg_1, L) -->
  "〈", seplist(Dcg_1, L), "〉".



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
