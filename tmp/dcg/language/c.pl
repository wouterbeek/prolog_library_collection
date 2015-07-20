:- module(
  c,
  [
    c_convert//0,
    c_double//1,
    c_name//0
  ]
).

/** <module> DCG rules for the C programming language.

@author Wouter Beek
@version 2013/02, 2013/06, 2014/01-2014/02, 2014/12-2015/01
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_char)).
:- use_module(library(dcg/dcg_generics)).
:- use_module(library(dcg/dcg_meta)).
:- use_module(library(dcg/dcg_replace)).
:- use_module(library(dcg/language/sw_number)).





%! c_convert// is det.
% Replace the bell character for `\b`.
% Replace the line feed character for `\n`.
% Replace the horizontal tab character for `\t`.
%
% ### Example
%
% ```prolog
% ?- use_module(plc(generics/code_ext)).
% ?- phrase(c_convert, `aaa\bbbb\nccc\tddd`, X), put_codes(current_output, X).
% aaabbb
% cccddd
% ```

c_convert -->
  '*'(dcg_replace, [`\b`,`\n`,`\t`], [bell,line_feed,horizontal_tab], []).



%! c_double(?Float:float)// .

c_double(N) -->
  'DOUBLE'(sparql, N),
  (char_ci(f) ; char_ci(l) ; "").



%! c_name// .
% ### Example
%
% ```prolog
% ?- once(phrase(c_name, `appe- lenSappP$`, CName)).
% CName = "appe__lensappp_" .
% ```

c_name -->
  eos.
c_name, [C] -->
  ascii_letter_lowercase(C),
  c_name.
c_name, [C] -->
  decimal_digit(C),
  c_name.
c_name, [C2] -->
  ascii_letter_uppercase(C1),
  {to_lower(C1, C2)},
  c_name.
c_name, "_" -->
  [_],
  c_name.

