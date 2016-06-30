:- module(
  dcg_cli,
  [
    ansi_str//2, % +Modifiers, +Str
    bold//1      % +Str
  ]
).

/** <module> DCG CLI

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(dcg/dcg_ext)).





%! ansi_str(+Modifiers, +Str)// is det.
%
% # Text attributes
%
%   | **Modifier**       | **Code** |
%   |:-------------------|:---------|
%   | All attributes off | 0        |
%   | Bold on            | 1        |
%   | Dim on             | 2        |
%   | Standout on        | 3        |
%   | Underscore on      | 4        |
%   | Blink on           | 5        |
%   | Reverse video on   | 7        |
%   | Concealed on       | 8        |
%
% # Foreground colors
%
%   | **Modifier** | **Code** |
%   |:-------------|:---------|
%   | Black        | 30       |
%   | Red          | 31       |
%   | Green        | 32       |
%   | Yellow       | 33       |
%   | Blue         | 34       |
%   | Magenta      | 35       |
%   | Cyan         | 36       |
%   | White        | 37       |
%   | Default      | 39       |
%
% # Background colors
%
%   | **Modifier** | **Code** |
%   |:-------------|:---------|
%   | Black        | 40       |
%   | Red          | 41       |
%   | Green        | 42       |
%   | Yellow       | 43       |
%   | Blue         | 44       |
%   | Magenta      | 45       |
%   | Cyan         | 46       |
%   | White        | 47       |
%   | Default      | 49       |

ansi_str(Modifiers, Str) -->
  "\e[",
  seplist(integer, ";", Modifiers),
  "m",
  atom(Str),
  "\e[0m", !.



%! bold(+Str)// is det.

bold(Str) -->
  ansi_str([1], Str).
