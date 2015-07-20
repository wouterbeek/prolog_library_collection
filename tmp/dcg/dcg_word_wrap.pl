:- module(
  dcg_word_wrap,
  [
    dcg_word_wrap//1 % +Options:list(nvpair)
  ]
).

/** <module> DCG: Word wrap

Grammar for wrapping lines of text.

There are various uses of wrapping text:
  1. Wrapping text with newlines and padding them with spaces.
     This is e.g. used for the speech bubble in cowspeak.
     The newline suffices for terminal output.
     The padding with spaces is needed in order to have
     the `|` appear at the right horizontal position,
      forming the right hand side of the speech bubble.
  2. Wrapping text into separate lists without padding.
     This is useful if another predicate needs to perform
      arbitrary operations on the splitted lines of text.
     Since the display device may not be a terminal,
      the padding with spaces may not be necessary.
  3. Wrap text with HTML linebreak tags, i.e. `<br/>`,
      since HTML does not display newlines.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_peek)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(settings)).

:- setting(wrap_margin, integer, 80, 'The default wrap margin.').

:- meta_predicate(dcg_word_wrap(:,?,?)).
:- meta_predicate(dcg_word_wrap_hard(+,//,+,+,?,?)).
:- meta_predicate(dcg_word_wrap_soft(+,//,+,+,?,?)).

is_meta(separator).

:- predicate_options(dcg_word_wrap//1, 1, [
     padding(+boolean),
     separator(+callable),
     wrap_margin(+positive_integer),
     wrap_mode(+oneof([hard,none,soft]))
   ]).





%! dcg_word_wrap(+Options:list(nvpair))// is det.
% The following options are supported:
%   1. `padding(+boolean)`
%      Whether padding occurs at the right hand side of the last line
%      Spaces are used for padding.
%      Default: `false`.
%   2. `separator(:Dcg)`
%      The separator that is emitted between the wrapped lines
%      Default: `newline`.
%   3. `wrap_margin(+positive_integer)`
%      The maxmim width of a line of characters (default `80`).
%      This is the length at which line wrapping occurs.
%   4. `wrap_mode(+oneof([hard,none,soft]))`
%      Soft word wrap inserts newlines after words (default).
%      Hard wrod wrapping inserts newlines after characters.
%      Mode `none` does not insert any newlines.

dcg_word_wrap(Options1) -->
  {
    meta_options(is_meta, Options1, Options2),
    select_option(word_wrap(WordWrap), Options2, Options3, word),
    option(padding(Padding), Options3, false),
    option(separator(Separator), Options3, line_feed),
    setting(wrap_margin, DefaultWrapMargin),
    option(wrap_margin(WrapMargin), Options3, DefaultWrapMargin)
  },
  (   {WordWrap == hard}
  ->  dcg_word_wrap_hard(Padding, Separator, WrapMargin, WrapMargin)
  ;   {WordWrap == soft}
  ->  dcg_word_wrap_soft(Padding, Separator, WrapMargin, WrapMargin),
      % Prevent backtracking on codes/1 that appears in the head.
      !
  ;   dcg_cp
  ).



%! dcg_word_wrap_hard(
%!   +Padding:boolean,
%!   :Separator,
%!   +RemainingMargin:nonneg,
%!   +WrapMargin:nonneg
%! )// is det.
% Emits the parsed codes list with interspersed separators using
%  hard word wrapping.
%
% Hard word wrapping ends a line after the given number of characters
%  has been parsed, or once there are no more characters left.

% The wrap margin has been reached, so it is time for a separator.
% Reset the character count and start parsing the next line.
dcg_word_wrap_hard(Padding, Separator, 0, WrapMargin), Separator --> !,
  dcg_word_wrap_hard(Padding, Separator, WrapMargin, WrapMargin).
% In the midst of parsing a line.
% Process the next character and decrease the counter.
dcg_word_wrap_hard(Padding, Separator, Remaining1, WrapMargin), [Code] -->
  [Code],
  {Remaining2 is Remaining1 - 1}, !,
  dcg_word_wrap_hard(Padding, Separator, Remaining2, WrapMargin).
% The last character was consumed.
% Depending on the Padding option, space padding is applied or not.
dcg_word_wrap_hard(Padding, _, Remaining, _),
    apply_padding(Padding, Remaining)
-->
  eos.



%! dcg_word_wrap_soft(
%!   +Padding:boolean,
%!   :Separator,
%!   +RemainingMargin:nonneg,
%!   +WrapMargin:nonneg
%! )// is det.
% Returns the parsed codes list with newlines using soft word wrap.
%
% Soft word wrap means that a line split never occurs within a word.
%
% @tbd Use a natural language dictionary and a language tag
%      in order to wrap at word boundaries properly.

% Process another character. Notice that there are several options here.
dcg_word_wrap_soft(Padding, Separator, Remaining, WrapMargin),
    '*'(code, Word2, []),
    Postfix
-->
  % The behavior of word wrapping depends on properties of
  %  the upcoming word in the parsed string.
  % We therefore peek at this upcoming string.
  % A word is simplistically defined as a sequence of graphic characters.
  dcg_peek('+'(graphic, Word1, [count(WordLength)])), !,
  
  (   % Case 1: The word is too long to ever occur on a single line.
      % Therefore, we might as well split it now.
      % Insert the word prefix that fits in the current line.
      % Insert a newline directly afterwards (i.e. no space).
      % Consume the placed word prefix, but not the rest of the word (yet).
      {WordLength > WrapMargin}
  ->  {
        length(Word2, Remaining),
        append(Word2, _, Word1),
        Postfix = Separator,
        NewRemaining = WrapMargin
      },
      '*'(code, Word2, [])
  ;   % Case 2: What a nice accident! The word happens to fit exactly
      % into the remaining positions of the current line.
      % Place the word, and insert the split dirrectly after the word.
      % Also, skip any directly following white characters from the stream
      % (since they would otherwise start the next line).
      {WordLength == Remaining}
  ->  {
        Word2 = Word1,
        Postfix = Separator,
        NewRemaining = WrapMargin
      },
      '*'(code, Word1, []),
      whites
  ;   % Case 3: The word is too long to fit on the current line,
      % but it would fit on a new line.
      % Fill the rest of the line with spaces (depending on
      % the `padding` option) and insert the separator after that.
      % Process the entire word later.
      {WordLength > Remaining}
  ->  {
        (   Padding == true
        ->  repeating_list(32, Remaining, Word2)
        ;   Word2 = []
        ),
        Postfix = Separator,
        NewRemaining = WrapMargin
      }
  ;   % Case 4: The 'normal' case.
      % The word fits in the current line, and on the current line such that
      %  there will be at least one character position left after it.
      % Place the word, and consume it.
      {Word2 = Word1},
      '*'(code, Word1, []),

      % Whether a space should be inserted after the word, depends on
      %  whether a whitespace character appears in the processed string.
      % This is not always the case, e.g. when the word occurs
      %  at the end of the processed string.
      % We need to do some bookkeeping in order to get this right.
      (   dcg_peek(u_white(Code))
      ->  [Code],
          {
            Postfix = space,
            SpaceLength = 1
          }
      ;   {
            Postfix = dcg_void,
            SpaceLength = 0
          }
      ),
      {NewRemaining is Remaining - WordLength - SpaceLength}
  ),

  % No regrets.
  % Actually, this does not prevent Prolog from backtracking
  % on `'*'(code, Codes, [])` in the head!
  % The calling context must enforce determinism.
  !,
  dcg_word_wrap_soft(Padding, Separator, NewRemaining, WrapMargin).
% Insert a newline while processing non-graphic characters.
dcg_word_wrap_soft(Padding, Separator, 0, WrapMargin), Separator --> !,
  dcg_word_wrap_soft(Padding, Separator, WrapMargin, WrapMargin).
% Process a non-graphic character.
dcg_word_wrap_soft(Padding, Separator, Remaining1, WrapMargin), [Code] -->
  [Code], !,
  {Remaining2 is Remaining1 - 1},
  dcg_word_wrap_soft(Padding, Separator, Remaining2, WrapMargin).
% The last character was consumed.
% Depending on the Padding option, space padding is applied or not.
dcg_word_wrap_soft(Padding, _, Remaining, _),
    apply_padding(Padding, Remaining)
-->
  eos.





% HELPERS

apply_padding(false, _) --> "".
apply_padding(true, Remaining) --> '#'(Remaining, space, []).
