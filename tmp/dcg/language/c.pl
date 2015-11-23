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
@version 2015/11
*/

:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_char)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_generics)).
:- use_module(library(dcg/dcg_meta)).
:- use_module(library(dcg/dcg_replace)).





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

c_convert, "\b" --> "\\b", !, c_convert.
c_convert, "\n" --> "\\n", !, c_convert.
c_convert, "\t" --> "\\t", !, c_convert.
c_convert, [C] --> [C],    !, c_convert.
c_convert --> "".



%! c_double(?Float:float)// .

c_double(N) --> 'DOUBLE'(N), ("f" ; "F" ; "l" ; "L").



%! c_name// .
% ### Example
%
% ```prolog
% ?- once(phrase(c_name, `appe- lenSappP$`, CName)).
% CName = "appe__lensappp_" .
% ```

c_name, [C]  --> lowalpha(C), !, c_name.
c_name, [C]  --> digit(C),    !, c_name.
c_name, [C2] --> upalpha(C1), !, {to_lower(C1, C2)}, c_name.
c_name, "_"  --> [_],         !, c_name.
c_name       --> eos.
