This is a collection of Prolog libraries that augment the functionality
available in the [SWI-Prolog](www.swi-prolog.org) standard libraries.


Abbreviations
=============

We use the following abbreviations for often occurring variable names:

| **Variable name** | **Expansion** |
|:-----------------:|:-------------:|
| A                 | Atom          |
| Abs               | Absolute      |
| Arg               | Argument      |
| Attr              | Attribute     |
| C                 | Code          |
| Char              | Character     |
| Comp              | Component     |
| D                 | Digit         |
| Def               | Default       |
| E                 | Error         |
| Exts              | Extensions    |
| Frac              | Fractional    |
| H                 | Head          |
| I                 | Integer       |
| L                 | List          |
| Lang              | Language      |
| Max               | Maximum       |
| Min               | Minimum       |
| N                 | Number        |
| Num$X$s           | NumberOf$X$s  |
| Opt               | Options       |
| Param             | Parameter     |
| Perc              | Percentage    |
| Req               | Request       |
| S                 | String        |
| Sep               | Separator     |
| T                 | Tail          |


`/dcg` Definite Clause Grammars
===============================

In directory `dcg` you will find a collection of Definite Clause Grammar (DCG) modules:

  - [`dcg_abnf`](https://github.com/wouterbeek/Prolog_Library_Collection#dcg_abnf):
    Bauckus-Naur Form (BNF) constructs in DCGs.
  - [`dcg_arrow`](https://github.com/wouterbeek/Prolog_Library_Collection#dcg_arrow):
    Simple ASCII arrows and horizontal lines.
  - [`dcg_ascii`](https://github.com/wouterbeek/Prolog_Library_Collection#dcg_ascii)
    Provides support for all ASCII characters
    and common ASCII character groups.
  - [`dcg_cardinal`](https://github.com/wouterbeek/Prolog_Library_Collection#dcg_cardinal)
    DCG rules for the cardinal numbers.
  - `dcg_content` DCG rules for often-occurring content that does
    not belong to a specific module.
  - `dcg_generics` Generic extensions for the use of DCGs
    (e.g., meta-calls for DCGs, peeking, REs, replacements).
  - `dcg_unicode` DCG rules for Unicode support.
  - `dcg_word_wrap` DCG rules for word wrapping:
    *soft word wrap* at word boundaries,
    or *hard word wrap* at character boundaries.
  - `emoticons` DCG rules for emoticons, contributed by Anne Ogborn :-)

In the code examples we expect that code lists are portrayed in the Prolog top level.
An easy way of setting this is by loading the following module:

```prolog
:- [library(deb_ext)].
```

---

`/dcg/dcg_abnf.pl`: Advanced Bauckus-Naur Form (BNF)
====================================================

While DCGs are nice, the use of Backus Naur Form-notation (BNF) sometimes results in simpler code.

#### A simple example

While DCGs are nice, the use of Backus Naur Form-notation (BNF) sometimes results in simpler code.
For example, the following DCG:

```prolog
word(Word) -->
  letters(Codes),
  atom_codes(Word, Codes).

letters([H|T]) -->
  letter(H),
  letters(T).
letters([]) --> "".
```

This can be written as follows by using the Kleene star (`*`):

```prolog
word(Word) -->
  '*'(letter, Codes ,[]),
  atom_codes(Word, Codes).
```

Inspired by [RFC 5234: Advanced Backus Naur Form (ABNF)](https://tools.ietf.org/html/rfc5234), library `dcg_abnf` introduces the following ABNF-like operators:

| **Expression**            | **Meaning**                       |
| ------------------------- | --------------------------------- |
| `'#'(?N, :Dcg, [])`       | Process `Dcg` exactly `N` times.  |
| `'*'(:Dcg, [])`           | Process `Dcg` 0 or more times.    |
| `'*n'(?N, :Dcg, [])`      | Process `Dcg` at most `N` times.  |
| `'+'(:Dcg, [])`           | Process `Dcg` 1 or more times.    |
| `'?'(:Dcg, [])`           | Process `Dcg` 0 or 1 times. Alternatively: `Dcg` is optional. |
| `'m*'(?M, :Dcg, [])`      | Process `Dcg` at least `M` times. |
| `'m*n'(?M, ?N, :Dcg, [])` | Process `Dcg` at least `M` and at most `N` times.            |

In the previous table the last argument is the empty list.
This list can contain any of the following options:

| **Option**             | **Meaning** |
| ---------------------- | ----------- |
| `copy_term(+boolean)`  | Whether or not `Dcg` should first be copied before being processed. |
| `count(-nonneg)`       | The exact number of times `Dcg` is processed.  For `'#'//[3-8]` this is `N`.  For all other ABNF operators this returns a non-trivial result that may be informative to the calling context. |
| `separator(+callable)` | If `Dcg` is processed more than once, the separator DCG rule is processed in between any two productions of `Dcg`. |


#### Example using option `count` and `separator`

In the following example we state that a sentence consists of one or more words that are separated by whitespace.
We also want to keep track of the number of words:

```prolog
sentence(N1, [H|T]) -->
  word(H),
  white,
  {succ(N1, N2)},
  words(N2, T).

words(N1, [H|T]) -->
  word(H),
  white,
  {succ(N1, N2)},
  words(N2, T).
words(0, []) --> "".
```

Using library `dcg_abnf` we write this as follows:

```prolog
sentence(N, Words) -->
  '+'(word, Words, [count(N),separator(white)]).
```


#### Example solutions with option `count`

The predicates defined in this module allow the number of DCG productions to be returned through the `count` option:

```prolog
?- [library(dcg/dcg_abnf)].
?- [library(dcg/dcg_arrow)].
?- phrase('*'(arrow(Head, Length), [count(Count),copy_term(true)]), `<--->`).
Count = 1 ;   % `<--->`
Count = 2 ;   % `<---` and `>`
Count = 2 ;   % `<--` and `->`
Count = 2 ;   % `<-` and `-->`
Count = 2 ;   % `<` and `--->`
false.
```

#### Uninstantiated variables: shared or not?

The DCG rules defined in this module allow the DCG goal to be either copied or not using the `copy_term` option.

For `copy_term(true)` a new copy of the DCG rule is called each time.
For `copy_term(false)` the uninstantiated variables are shared between all productions of DCG.
We illustrate this distinction with an example.

The following generates all sequences of at most 2 arrows surrounded by triple quotes, with uninstantiated variables shared between successive calls of `Dcg`.
The output shows that the single and double quote characters do not occur in the same string.

```prolog
?- [library(dcg/dcg_abnf)].
?- [library(dcg/dcg_arrow)].
?- phrase('*n'(2, quoted(3, Quote, arrow(right, 8)), [copy_term(false)]), X).
X = "" ;
Quote = double_quote,
X = """"------->"""" ;
Quote = double_quote,
X = """"------->""""""------->"""" ;
Quote = single_quote,
X = "'''------->'''" ;
Quote = single_quote,
X = "'''------->''''''------->'''" ;
false.
```

The following generates all sequences of at most 2 arrows surrounded by triple quotes, without sharing variables between successive calls of `Dcg`.
The output shows that the two arrows are now allowed to usiong different quotation characters (single and double):

```prolog
?- [library(dcg/dcg_abnf)].
?- [library(dcg/dcg_arrow)].
?- phrase('*n'(2, quoted(3, Quote, arrow(right, 8)), [copy_term(true)]), X).
X = "" ;
X = """"------->"""" ;
X = """"------->""""""------->"""" ;
X = """"------->"""'''------->'''" ;
X = "'''------->'''" ;
X = "'''------->'''"""------->"""" ;
X = "'''------->''''''------->'''" ;
false.
```

---

`dcg_arrow`: ASCII arrows and horizontal lines
==============================================

This library consists of the following two DCGs:

  * `arrow(?Head:oneof([both,left,right]), ?Length:nonneg)//`
  * `horizontal_line(?Length:nonneg)//`

For example the string `<--->` can be parsed as a sequence of two ASCII arrows in four ways:

```prolog
?- [library(dcg/dcg_arrow)].
?- phrase((arrow(Head1, Length1), arrow(Head2, Length2)), `<--->`).
Head1 = left,   % <--- and >
Length1 = 4,
Head2 = right,
Length2 = 1 ;
Head1 = left,   % <-- and ->
Length1 = 3,
Head2 = right,
Length2 = 2 ;
Head1 = left,   % <- and -->
Length1 = 2,
Head2 = right,
Length2 = 3 ;
Head1 = left,   % < and --->
Length1 = 1,
Head2 = right,
Length2 = 4 ;
false.
```

---

`/dcg/dcg_ascii`: ASCII characters
==================================

Provides support for all individual [ASCII characters](http://en.wikipedia.org/wiki/ASCII) by name.
Meaningful groups of ASCII characters are defined as well: brackets, lowercase letters, control characters, hexadecimal digits, whites, etc.

Every character and characer group comes with a variant DCG that has the decimal character code as additional argument.

As an example, we can define the fragment of a URI (per RFC 3986) without having to look up the numeric ASCII codes (resulting in code that is better documented):

```prolog
fragment(Fragment) -->
  '*'(fragment_code, Codes, []),
  {atom_codes(Fragment, Codes)}.

fragment_code(Code) --> pchar(Code).
fragment_code(Code) --> forward_slash(Code).
fragment_code(Code) --> question_mark(Code).
```

This is especially useful for characers that are non-graphic or that look similar to other characers on some displays (e.g., carriage return and line-feed).

Numeric characters (i.e., `0-9A-Za-z`) also have a variant DCG with a weight argument representing the decimal value of the character.

Bracket characters and bracket character groups have a variant with an additional `Type` argument which is either `angular`, `curly`, `round` or `square`.
A handy special case for this occurs with content that is (consistently) surrounded by some bracket type, possibly returning the bracket type to the calling context:

```prolog
bracketed_content(Type, Dcg) -->
  opening_bracket(Type, _),
  Dcg,
  closing_bracket(Type, _).
```

The above can e.g. be use to parse [Markdown URL notation](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet#links):

```prolog
markdown_url(Label, Url) -->
  bracketed_content(square, Label),
  bracketed_content(round, Url).
```

---

`/dcg/dcg_bracketed`: Surrounding brackets
==========================================

The following DCG rules can be used to put brackets around arbitrary content:

  * `bracketed(:Content)//`
  * `bracketed(+Type:oneof([angular,curly,langular,round,square]), :Content)//`
  Surround content by brackets of the given type.


```prolog
?- [library(dcg/dcg_bracketed)].
?- phrase(bracketed(Type, `monkey`), Cs).
Type = angular,
Cs = "<monkey>" ;
Type = curly,
Cs = "{monkey}" ;
Type = round,
Cs = "(monkey)" ;
Type = square,
Cs = "[monkey]" ;
Type = langular,
Cs = [12296, 109, 111, 110, 107, 101, 121, 12297].
```
---

`/dcg/dcg_cadinal`: Cardinal numbers
====================================

The following DCG rules process integers between a lower and an unpper bound:

  - `between(+Low:integer, +High:integer)`
  - `between(+Low:integer, +High:integer, ?Value:integer)`
    Process integers between the given lower and higher bounds.
  - `between_digit(+Low:hex, +High:hex, ?Value:hex)`
    Process digits between the given lower and higher bounds.
    `hex` is defined as `or([between(0,9),oneof([a,b,c,d,e,f])])`.
  - `between_radix(+Low:compound, +High:compound[, ?Value:compound])`
    Radix values are compound terms of the following forms: `bin/1`, `oct/1`, `dec/1`, `hex/1`.
    ```prolog
    ?- [library(dcg/dcg_cardinal)].
    ?- phrase(between_radix(bin(1001), hex(f), oct(X)), Codes).
    X = 11,
    Codes = "9" ;
    X = 12,
    Codes = "10" ;
    X = 13,
    Codes = "11" ;
    X = 14,
    Codes = "12" ;
    X = 15,
    Codes = "13" ;
    X = 16,
    Codes = "14" ;
    X = 17,
    Codes = "15".
    ```

---

`/dcg/dcg_code`: Codes
======================

This module provides the following DCGs for processing character codes:

  * `between_code(+Low:code, +High:code)//`
  * `between_code(+Low:code, +High:code, ?Code:code)//`
  * `between_code_radix(+Low:compound, +High:compound)//`
  * `between_code_radix(+Low:compound, +High:compound, ?Code:code)//`
  Radix values are compound terms of the following forms: `bin/1`, `oct/1`, `dec/1`, `hex/1`.
  * `code(?Code:code)//`
  The same as `[Code]`.
  * `code_ci(?Code:code)//`
  Case-insensitive version of `code//1`.
  * `code_lower(?Code:code)//`

#### Simple generation example

Generate the upper- and lowercase variants of a given letter.

```prolog
?- phrase(code_ci(oct(142)), [X]), string_codes(S, [X]).
X = 66,
S = "B" ;
X = 98,
S = "b".
```

#### Simple parsing example

Parse a letter in its lower- and uppercase variants.

```prolog
?- phrase(code_ci(hex(X)), `b`).
X = 66 ;
X = 98.
```

#### Real-world example

It often occurs that keywords or other reserved words in a grammar are allowed to appear in any case variant.
The following generate all case-variants of the string `http`:

```prolog
?- [library(dcg/dcg_abnf)].
?- [library(dcg/dcg_code)].
?- phrase('*'(code_ci, `http`, []), X).
X = "HTTP" ;
X = "HTTp" ;
X = "HTtP" ;
X = "HTtp" ;
X = "HtTP" ;
X = "HtTp" ;
X = "HttP" ;
X = "Http" ;
X = "hTTP" ;
X = "hTTp" ;
X = "hTtP" ;
X = "hTtp" ;
X = "htTP" ;
X = "htTp" ;
X = "httP" ;
X = "http" ;
false.
```

`/dcg/dcg_quoted`: Quoting
==========================

The following DCG rules allow arbitrary content to be quoted:

  * `quoted(:Content)//`
  Quote content using single occurrences of double quotes.
  * `quoted(:Quote, :Content)//`
  Quote content using single occurrences of an arbitrary quote.
  * `quoted(?Length:positive_integer, :Quote, :Content)//`
  Quote content using given number of occurrences of an arbitrary quote.
  If no quote is given then `double_quote//0` and `single_quote//0` (in that order) are used.

---

This library was developed by [Wouter Beek](http://www.wouterbeek.com) in 2013-2015.
