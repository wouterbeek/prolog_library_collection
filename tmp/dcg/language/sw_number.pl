:- module(
  sw_number,
  [
    'DECIMAL'//2, % +Language:oneof([sparql,turtle])
                  % ?Decimal:float
    decimalLiteral//1, % ?Decimal:float
    'DECIMAL_NEGATIVE'//1, % ?Decimal:float
    'DECIMAL_POSITIVE'//1, % ?Decimal:float
    'DOUBLE'//2, % +Language:oneof([sparql,turtle])
                 % ?Double:float
    'DOUBLE_NEGATIVE'//1, % ?Double:float
    'DOUBLE_POSITIVE'//1, % ?Double:float
    'EXPONENT'//1, % ?Value:integer
    floatingPointLiteral//1, % ?Float:float
    'INTEGER'//2, % +Language:oneof([sparql,turtle])
                  % ?Integer:integer
    integerLiteral//1, % ?Integer:integer
    'INTEGER_NEGATIVE'//1, % ?Integer:integer
    'INTEGER_POSITIVE'//1 % ?Integer:integer
  ]
).

/** <module> SW grammar: Numbers

Grammar rules for numbers as defined by Semantic Web standards.

Changes to Manchester grammar
=============================

All occurrences of:
  - `digits` are replaced by `[0-9]+`.
  - `{ digit }` are replaced by `[0-9]*`.
  - `digit` are replaced by `[0-9]`.
  - `nonZero` are replaced by `[1-9]`.
  - `zero` are replaced by `'0'`.
  - `('e' | 'E')` are replaced by `[eE]`.
  - `('f' | 'F')` are replaced by `[fF]`.
  - `['+' | '-']` are replaced by `[+-]?`.
  - `[exponent]` are replaced by `exponent?`.
  - `['.'digits]` are replaced by `('.' [0-9]+)?`.

```
[1]   digits ::= digit { digit }
[2]   digit ::= zero | nonZero
[3]   nonZero := '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
[4]   zero ::= '0'
```

---

@author Wouter Beek
@compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)
@compat SPARQL 1.0
@compat SPARQL 1.1 Query
@compat Turtle 1.1
@version 2014-2015
*/

:- use_module(plc(dcg/dcg_abnf_common)).
:- use_module(plc(dcg/dcg_cardinal)).
:- use_module(plc(dcg/dcg_char)).
:- use_module(plc(math/math_ext)).
:- use_module(plc(math/positional)).
:- use_module(plc(math/radix)).
:- use_module(plc(math/rational_ext)).





%! 'DECIMAL'(+Language:oneof([sparql,turtle]), ?Decimal:compound)// .
% ```ebnf
% [SPARQL]   DECIMAL ::= [0-9]* '.' [0-9]+
% [Turtle]   DECIMAL ::= [+-]? [0-9]* '.' [0-9]+
% ```
%
% @compat SPARQL 1.0 [78]
% @compat SPARQL 1.1 Query [147]
% @compat Turtle 1.1 [20]

'DECIMAL'(sparql, N) -->
  (   {ground(N)}
  ->  {rational_parts_weights(N, IW, FW)},
      '[0-9]*'(IW),
      ".",
      '[0-9]+'(FW)
  ;   '[0-9]*'(IW),
      ".",
      '[0-9]+'(FW),
      {rational_parts_weights(N, IW, FW)}
  ).
'DECIMAL'(turtle, N) -->
  (   {ground(N)}
  ->  '[+-]?'(N),
      {N0 is abs(N)},
      'DECIMAL'(sparql, N0)
  ;   '[+-]?'(Sg),
      'DECIMAL'(sparql, N0),
      {N is copysign(N0, Sg)}
  ).



%! decimalLiteral(?Decimal:float)// .
% ```bnf
% decimalLiteral ::= [+-]? [0-9]+ '.' [0-9]+
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)
% @compat Different from 'DECIMAL'//2 (SPARQL, Turtle) in that
%         (1) at least one digit must appear before the dot,
%         (2) at least one digit must appear after the dot.

decimalLiteral(N) -->
  (   {ground(N)}
  ->  '[+-]?'(N),
      {N0 is abs(N)},
      {rational_parts_weights(N0, IW, FW)},
      '[0-9]+'(IW),
      ".",
      '[0-9]+'(FW)
  ;   '[+-]?'(Sg),
      '[0-9]+'(IW),
      ".",
      '[0-9]+'(FW),
      {rational_parts_weights(N0, IW, FW)},
      {N is copysign(N0, Sg)}
  ).



%! 'DECIMAL_NEGATIVE'(?Decimal:compound)// .
% ```bnf
% DECIMAL_NEGATIVE ::= '-' DECIMAL
% ```
%
% @compat SPARQL 1.0 [84]
% @compat SPARQL 1.1 Query [153]

'DECIMAL_NEGATIVE'(N) -->
  "-",
  (   {ground(N)}
  ->  {N < 0.0},
      {N0 is copysign(N, 1)},
      'DECIMAL'(sparql, N0)
  ;   'DECIMAL'(sparql, N0),
      {N is copysign(N0, -1)}
  ).



%! 'DECIMAL_POSITIVE'(?Decimal:float)// .
% ```bnf
% DECIMAL_POSITIVE ::= '+' INTEGER
% ```
%
% @compat SPARQL 1.0 [81]
% @compat SPARQL 1.1 Query [149]

'DECIMAL_POSITIVE'(N) -->
  "+",
  'INTEGER'(sparql, N).



%! 'DOUBLE'(+Language:oneof([sparql,turtle]), ?Double:float)// .
% ```ebnf
% [SPARQL]   DOUBLE ::=   [0-9]+ '.' [0-9]* EXPONENT
%                       | '.' [0-9]+ EXPONENT
%                       | [0-9]+ EXPONENT
% [Turtle]   DOUBLE ::= [+-]?
%                       ( [0-9]+ '.' [0-9]* EXPONENT
%                       | '.' [0-9]+ EXPONENT
%                        | [0-9]+ EXPONENT
%                       )
% ```
%
% @compat SPARQL 1.0 [79]
% @compat SPARQL 1.1 Query [148]
% @compat Turtle 1.1 [21]
% @tbd How to write the exponent in the generative case?

'DOUBLE'(sparql, N) -->
  (   {ground(N)}
  ->  {normalized_number(N, Norm, Exp)},
      {I is float_integer_part(Norm)},
      {F is float_fractional_part(Norm)},
      {positional_fraction(FW, F)},
      (   {positional(I, IW)},
          '[0-9]+'(IW),
          ".",
          '[0-9]*'(FW),
          'EXPONENT'(Exp)
      ;   {I =:= 0},
          ".",
          '[0-9]*'(FW),
          'EXPONENT'(Exp)
      ;   {I =:= 0},
          '[0-9]+'(FW),
          'EXPONENT'(Exp)
      )
  ;   (   '[0-9]+'(IW),
          ".",
          '[0-9]*'(FW),
          'EXPONENT'(Exp)
      ;   ".",
          '[0-9]*'(FW),
          'EXPONENT'(Exp)
      ;   '[0-9]+'(FW),
          'EXPONENT'(Exp)
      ),
      {positional_fraction(FW, F)},
      {positional(I, IW)},
      {N is I + F * 10 ^ Exp}
  ).
'DOUBLE'(turtle, N) -->
  (   {ground(N)}
  ->  '[+-]?'(N),
      {N0 is abs(N)},
      'DOUBLE'(sparql, N0)
  ;   '[+-]?'(Sg),
      'DOUBLE'(sparql, N0),
      {N is copysign(N0, Sg)}
  ).



%! 'DOUBLE_POSITIVE'(?Double:float)// .
% ```bnf
% DOUBLE_POSITIVE ::= '+' DOUBLE
% ```
%
% @compat SPARQL 1.0 [82]
% @compat SPARQL 1.1 Query [151]

'DOUBLE_POSITIVE'(N) -->
  "+",
  'DOUBLE'(sparql, N).



%! 'DOUBLE_NEGATIVE'(?Double:float)// .
% ```bnf
% DOUBLE_NEGATIVE ::= '-' DOUBLE
% ```
%
% @compat SPARQL 1.0 [85]
% @compat SPARQL 1.1 Query [154]

'DOUBLE_NEGATIVE'(N) -->
  "-",
  (   {ground(N)}
  ->  {N < 0.0},
      {N0 is abs(N)},
      'DOUBLE'(sparql, N0)
  ;   'DOUBLE'(sparql, N0),
      {N is copysign(N0, -1)}
  ).



%! 'EXPONENT'(?Value:float)// .
% ```ebnf
% EXPONENT ::= [eE] [+-]? [0-9]+
% ```
%
% @compat SPARQL 1.0 [86]
% @compat SPARQL 1.1 Query [155]
% @compat Turtle 1.1 [154s]

'EXPONENT'(N) -->
  exponent(N).



%! exponent(?Value:float)// .
% ```bnf
% exponent ::= [eE] [+-]? [0-9]+
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

exponent(Exp) -->
  char_ci(e),
  (   {ground(Exp)}
  ->  {N is log10(Exp)},
      '[+-]?'(Exp),
      {positional(N, N0)},
      '[0-9]+'(N0)
  ;   '[+-]?'(Sg),
      '[0-9]+'(N0),
      {positional(N, N0)},
      {Exp is Sg * 10 ^ N}
  ).



%! floatingPointLiteral(?Float:float)// .
% ```bnf
% floatingPointLiteral ::= [+-]?
%                          ( [0-9]+ ('.' [0-9]+)? exponent?
%                          | '.' [0-9]+ exponent?)
%                          [fF]
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)
% @compat This is different from 'DOUBLE'//2 (SPARQL, Turtle) where
%         (1) the exponent is required,
%         (2) where the dot is allowed even if it is not followed by
%             any digit.

floatingPointLiteral(N) -->
  (   {ground(N)}
  ->  '[+-]?'(N),
      {N0 is abs(N)},
      {normalized_number(N0, Norm, Exp)},
      {rational_parts_weights(Norm, IW, FW)},
      (   '[0-9]+'(IW),
          (   ".",
              '[0-9]+'(FW)
          ;   ""
          ),
          (   exponent(Exp)
          ;   ""
          )
      ;   ".",
          '[0-9]+'(FW),
          (   exponent(Exp)
          ;   ""
          )
      )
  ;   '[+-]?'(Sg),
      (   '[0-9]+'(IW),
          (   ".",
              '[0-9]+'(FW)
          ;   ""
          ),
          (   exponent(Exp)
          ;   ""
          )
      ;   ".",
          '[0-9]+'(FW),
          (   exponent(Exp)
          ;   ""
          )
      ),
      {rational_parts_weights(Norm, IW, FW)},
      {N is Sg * Norm * 10 ^ Exp}
  ),
  char_ci(f).



%! 'INTEGER'(?Language:oneof([sparql,turtle]), ?Integer:integer)// .
% ```ebnf
% [SPARQL]   INTEGER ::= [0-9]+
% [Turtle]   INTEGER ::= [+-]? [0-9]+
% ```
%
% @compat SPARQL 1.0 [77]
% @compat SPARQL 1.1 Query [146]
% @compat Turtle 1.1 [19]

'INTEGER'(sparql, N) -->
  (   {ground(N)}
  ->  {positional(N, NW)},
      '[0-9]+'(NW)
  ;   '[0-9]+'(NW),
      {positional(N, NW)}
  ).
'INTEGER'(turtle, N) -->
  (   {ground(N)}
  ->  '[+-]?'(N),
      {N0 is abs(N)},
      {positional(N0, NW)},
      '[0-9]+'(NW)
  ;   '[+-]?'(Sg),
      '[0-9]+'(NW),
      {positional(N0, NW)},
      {N is copysign(N0, Sg)}
  ).



%! integerLiteral(?Integer:integer)// .
% ```bnf
% integerLiteral ::= [+-]? [0-9]+
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

integerLiteral(N) -->
  (   {ground(N)}
  ->  '[+-]?'(N),
      {N0 is abs(N)},
      {positional(N0, NW)},
      '[0-9]+'(NW)
  ;   '[+-]?'(Sg),
      '[0-9]+'(NW),
      {positional(N0, NW)},
      {N is copysign(N0, Sg)}
  ).



%! 'INTEGER_POSITIVE'(?Integer:integer)// .
% ```bnf
% INTEGER_POSITIVE ::= '+' INTEGER
% ```
%
% @compat SPARQL 1.0 [80]
% @compat SPARQL 1.1 Query [149]

'INTEGER_POSITIVE'(N) -->
  "+",
  'INTEGER'(sparql, N).



%! 'INTEGER_NEGATIVE'(?Integer:integer)// .
% ```bnf
% INTEGER_NEGATIVE ::= '-' INTEGER
% ```
%
% @compat SPARQL 1.0 [83]
% @compat SPARQL 1.1 Query [152]

'INTEGER_NEGATIVE'(N) -->
  "-",
  (   {ground(N)}
  ->  {N < 0},
      {N0 is abs(N)},
      'INTEGER'(sparql, N0)
  ;   'INTEGER'(sparql, N0),
      {N is copysign(N0, -1)}
  ).



%! nonNegativeInteger(?Integer:nonneg)// .
% ```bnf
% nonNegativeInteger ::= '0' | positiveInteger
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

nonNegativeInteger(N) -->
  (   "0",
      {N = 0}
  ;   positiveInteger(N)
  ).



%! positiveInteger(?Integer:positive_integer)// .
% ```bnf
% positiveInteger ::= [1-9] [0-9]*
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

positiveInteger(N) -->
  (   {ground(N)}
  ->  {positional(N, [H|T])},
      '[1-9]'(H),
      '[0-9]*'(T)
  ;   '[1-9]'(H),
      '[0-9]*'(T),
      {positional(N, [H|T])}
  ).



%! '[1-9]'(?Digit:between(1,9))// .
% ```bnf
% nonZero := '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

'[1-9]'(N) -->
  between_digit(1, 9, N).
