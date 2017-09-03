:- module(
  dcg_ext,
  [
    atom_lower//1,         % ?Atom
    atom_title//1,         % ?Atom
    atom_upper//1,         % ?Atom
    atom_uppercase//0,
    atom_uppercase//1,     % +Atom
    between_code_rad//2,   % +RadLow, +RadHigh
    between_code_rad//3,   % +RadLow, +RadHigh, -Code
    between_digit//2,      % +Low, +High
    between_digit//3,      % +Low, +High, -Weight
    between_digit//4,      % +Low, +High, -Weight, -Code
    between_int//2,        % +Low, +High
    between_int//3,        % +Low, +High, ?Value
    between_radix//2,      % +Low, +High
    between_radix//3,      % +Low, +High, ?Value
    bit//1,                % ?Integer
    bracketed//1,          % :Dcg_0
    bracketed//2,          % ?Type, :Dcg_0
    bs//0,
    code//1,               % ?Code
    code_ci//1,            % ?Code
    code_lower//1,         % ?Code
    code_rad//1,           % ?RadCode
    code_rad//2,           % ?RadCode, -Code
    code_upper//1,         % ?Code
    dcg/1,                 % :Dcg_0
    dcg_apply//2,          % :Dcg_1, +Args
    dcg_apply_cp//2,       % :Dcg_1, +Args
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
    dcg_strip//0,
    dcg_strip//1,          % +StripCodes
    dcg_width/2,           % :Dcg_0, -Width
    digit_code//1,         % ?Code
    done//0,
    dq//1,                 % :Dcg_0
    eol//0,
    generating//0,
    indent//2,             % +Indent:nonneg, :Dcg_0
    indent_nl//2,          % +Indent:nonneg, :Dcg_0
    lowercase//0,
    number//0,
    ordinal//1,            % +M
    pair//2,               % :Dcg_0, :Dcg_0
    perc//1,               % +Perc
    perc_fixed//1,         % +Perc
    progress_bar//2,       % +Processed, +All
    quad//4,               % :DcgX_0, :DcgY_0, :DcgZ_0, :DcgQ_0
    quoted//1,             % :Content_0
    quoted//2,             % :Quote_0, :Content_0
    quoted//3,             % ?Length, :Quote_0, :Content_0
    set//1,                % +L
    set//2,                % :Dcg_1, +L
    skip_line//0,
    sq//1,                 % :Dcg_0
    str_ci//1,             % ?Str
    str_ellipsis//2,       % +S, +Max
    string//0,
    string_atom_phrase/3,  % :Dcg_0, ?String, ?Atom
    string_without//1,     % +EndCodes
    triple//3,             % :DcgX_0, :DcgY_0, :DcgZ_0
    tuple//1,              % +L
    tuple//2,              % :Dcg_1, +L
    ws//0
  ]
).
:- reexport(library(dcg/basics), except([digit//1,digits//1])).
:- reexport(library(uri/rfc1738), [
     alpha//1,      % ?Code
     alphadigit//1, % ?Code
     digit//1,      % ?Weight
     digit//2,      % ?Weight, ?Code
     escape//1 as percent_escape, % ?Code
     hex//1,        % ?Weigth
     hex//2,        % ?Weigth, ?Code
     hialpha//1,    % ?Code
     lowalpha//1    % ?Code
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
     hialpha//1, % ?Code
     lowalpha//1 % ?Code
   ]).

:- meta_predicate
    bracketed(//, ?, ?),
    bracketed(+, //, ?, ?),
    bracketed0(+, //, ?, ?),
    dcg(//),
    dcg_apply(//,+, ?, ?),
    dcg_apply_cp(//, +, ?, ?),
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
    dcg_width(//, -),
    dq(//, ?, ?),
    indent(+, //, ?, ?),
    indent_nl(+, //, ?, ?),
    pair(//, //, ?, ?),
    quad(//, //, //, //, ?, ?),
    quoted(//, ?, ?),
    quoted(//, //, ?, ?),
    quoted(?, //, //, ?, ?),
    set(3, +, ?, ?),
    sq(//, ?, ?),
    string_atom_phrase(//, ?, ?),
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



%! between_code_rad(+Low, +High)// .
%! between_code_rad(+Low, +High, -C)// .
%
% Parses or generates a code between the given numbers.

between_code_rad(Low, High) -->
  between_code_rad(Low, High, _).


between_code_rad(dec(Low), dec(High), C) --> !,
  between(Low, High, C).
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
% # Example
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
  ->  dcg_tab(I),
      "{}"
  ;   % Singleton term do not use lucious spacing if the member is
      % singleton.
      {is_singleton_term0(Dict)}
  ->  dcg_tab(I),
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
  dcg_tab(I2).

dcg_entry0(Dcg_1, Key-Val, I1) --> !,
  dcg_tab(I1),
  dcg_call(Dcg_1, Key), ": ",
  (   {is_dict_or_list0(Val)}
  ->  % Newline and additional indentation before dictionary or list
      % values that are non-empty and non-singleton.
      ({(is_empty_or_singleton_term0(Val))} -> {I2 = 0} ; {I2 is I1 + 1}),
      dcg_dict_or_list0(Dcg_1, Val, I2)
  ;   dcg_call(Dcg_1, Val)
  ).
dcg_entry0(Dcg_1, Elem, I) -->
  dcg_tab(I),
  dcg_dict_or_list0(Dcg_1, Elem, I), !.
dcg_entry0(Dcg_1, Elem, I) -->
  dcg_tab(I),
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
  dcg_tab(I1), "]".



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



%! dcg_width(:Dcg_0, -Width:nonneg) is det.

dcg_width(Dcg_0, W) :-
  dcg_with_output_to(codes(Cs), Dcg_0),
  length(Cs, W).



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



%! set(+L)// is det.
%! set(:Dcg_1, +L)// is det.

set(L) -->
  set(pl_term, L).


set(Dcg_1, L) -->
  "{",
  *&(Dcg_1, ", ", L),
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



%! string_without(+EndCodes:list(code))// .
% Wrapper around string_without//2.

string_without(End) -->
  string_without(End, _).



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
  "〈", *&(Dcg_1, L), "〉".



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
