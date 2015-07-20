plDcg
=====

A collection of Descriptive Clause Grammar modules for (SWI-)Prolog.



Contents
--------

Library plDcg consists of the following modules:

  - [`dcg_abnf`](https://github.com/wouterbeek/plDcg#dcg_abnf):
    Bauckus-Naur Form (BNF) constructs in DCGs.
  - [`dcg_arrow`](https://github.com/wouterbeek/plDcg#dcg_arrow):
    Simple ASCII arrows and horizontal lines.
  - [`dcg_ascii`](https://github.com/wouterbeek/plDcg#dcg_ascii)
    Provides support for all ASCII characters
    and common ASCII character groups.
  - [`dcg_cardinal`](https://github.com/wouterbeek/plDcg#dcg_cardinal)
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



`dcg_abnf`: Bauckus-Naur Form (BNF) constructs in DCGs
======================================================

While DCGs are nice, the use of Backus Naur Form-notation (BNF)
sometimes results in simpler code.

A simple example
----------------

The following is quite common in DCGs: define a word as a (possibly empty) sequence of letters. Letters are represented by codes (= numeric character codes) but words are represented by atoms (e.g., allowing sentences to be represented by lists of atoms). Since we want the same grammar to both parse and generate, we have to put the conversion between atoms and codes either first (for generating) or last (for parsing). In code:

```prolog
word(Word) -->
  {var(Word)}, !,
  letters(Codes),
  {atom_codes(Word, Codes)}.
word(Word) -->
  {atom_codes(Word, Codes)},
  letters(Codes).

letters([H|T]) -->
  letter(H),
  letters(T).
letters([]) --> "".
```

Now we write the same thing more concisely by using the Kleene star (`*`):

```prolog
word(Word) -->
  '*'(letter, Word ,[convert1(codes_atom)]).
```

Specification
-------------

Different BNFs have been defined.
This module was inspired by
[RFC 5234: Advanced Backus Naur Form (ABNF)](https://tools.ietf.org/html/rfc5234), which is used in many RFC documents, and by EBNF, which is used in many W3C documents.

In the example above we were using the Kleene star (`*`) which says: "the following rule must be processed (i.e., generated/parsed) 0 or more times". In other words, the Kleene star restricts the number of applications of a rule to the range `[0,inf)`.
Similarly, the plus sign (`+`) says: "the following rules must be processed at least 1 time" (i.e., range: `[1,inf)`).

The most generic ABNF construct is `m*n` which says: "the following rule must be processed at least `m` and at most `n` times". Notice that the Kleene start and the plus sign can both be defined in terms of this generic construct.

The following constructs are defined by library `dcg_abnf`:

| **Construct** | **Generic expression** | **Meaning**                   |
|:-------------:| ---------------------- | ----------------------------- |
| `#`   | `'#'(?N, :Dcg, [])`       | Process `Dcg` exactly `N` times.   |
| `*`   | `'*'(:Dcg, [])`           | Process `Dcg` 0 or more times.     |
| `*n`  | `'*n'(?N, :Dcg, [])`      | Process `Dcg` at most `N` times.   |
| `+`   | `'+'(:Dcg, [])`           | Process `Dcg` 1 or more times.     |
| `?`   | `'?'(:Dcg, [])`           | Process `Dcg` 0 or 1 times. Alternatively: `Dcg` is optional. |
| `m*`  | `'m*'(?M, :Dcg, [])`      | Process `Dcg` at least `M` times.  |
| `m*n` | `'m*n'(?M, ?N, :Dcg, [])` | Process `Dcg` at least `M` and at most `N` times. |

Every ABNF construct takes a list of options as its last argument.
The following options are defined:

| **Option**             | **Meaning** |
|:----------------------:|:----------- |
| `copy_term(+boolean)`  | Whether or not `Dcg` should first be copied before being processed. |
| `convert1`(+callable)  | An arbitrary conversion applied to the first argument. `convert2` allows an arbitrary conversion to be applied to the second argument, etc. |
| `count(-nonneg)`       | The exact number of times `Dcg` is processed. For `'#'//[3-8]` this is the same as `N`. For all other ABNF operators this returns a non-trivial results that may be informative to the calling context. |
| `empty1(+term)`        | The Prolog term that is returned as the first argument in the case of zero productions of `Dcg`. |
| `separator(+callable)` | If `Dcg` is processed more than once, the separator DCG rule is processed in between any two productions of `Dcg`. |

Another example
---------------

Continuing our previous example, we now state that a sentence consists of one or more
 words that are separated by whitespace.
In addition, we want to keep track of the number of words in the sentence:

```prolog
sentence(Count1, [H|T]) -->
  word(H),
  white,
  {succ(Count1, Count2)},
  words(Count2, T).

words(Count1, [H|T]) -->
  word(H),
  white,
  {succ(Count1, Count2)},
  words(Count2, T).
words(0, []) --> "".
```

Using library `dcg_abnf` we can write this in a more concise way
 using the plsu sign (`+`) construct together with options `count` and `separator`:

```prolog
sentence(N, Words) -->
  '+'(word, Words, [count(N),separator(white)]).
```

---

`dcg_arrow`: Simple ASCII arrows and horizontal lines
=====================================================

The following generates all sequences of at most 2 arrows
surrounded by triple quotes, *with uninstantiated variables shared*
between successive calls.
The output shows that the single and double quote characters
do not occur in the same string.

```prolog
?- phrase('*n'(2, quoted(triple_quote(_), arrow(right, 8)), [copy_term(false)]), Codes),
   atom_codes(Atom, Codes).
Codes = [],
Atom = '' ;
Codes = """"------->"""",
Atom = '"""------->"""' ;
Codes = """"------->""""""------->"""",
Atom = '"""------->""""""------->"""' ;
Codes = "'''------->'''",
Atom = '\'\'\'------->\'\'\'' ;
Codes = "'''------->''''''------->'''",
Atom = '\'\'\'------->\'\'\'\'\'\'------->\'\'\'' ;
false.
```

The following generates all sequences of at most 2 arrows
surrounded by triple quotes, *without sharing variables*
between successive calls.
The output shows that the single and double quote characters
do not occur in the same string.

```prolog
?- phrase('*n'(2, quoted(triple_quote(_), arrow(right, 8)), [copy_term(true)]), Codes),
   atom_codes(Atom, Codes).
Codes = [],
Atom = '' ;
Codes = """"------->"""",
Atom = '"""------->"""' ;
Codes = """"------->""""""------->"""",
Atom = '"""------->""""""------->"""' ;
Codes = """"------->"""'''------->'''",
Atom = '"""------->"""\'\'\'------->\'\'\'' ;
Codes = "'''------->'''",
Atom = '\'\'\'------->\'\'\'' ;
Codes = "'''------->'''"""------->"""",
Atom = '\'\'\'------->\'\'\'"""------->"""' ;
Codes = "'''------->''''''------->'''",
Atom = '\'\'\'------->\'\'\'\'\'\'------->\'\'\'' ;
false.
```



`dcg_ascii`
===========

Provides support for all individual
[ASCII characters](http://en.wikipedia.org/wiki/ASCII)
by name.
On top of that defined meaningful groups of ASCII characters,
e.g., brackets, lowercase letters, control characters,
hexadecimal digits, whites.

Every character and characer group comes with a variant DCG
that has the decimal character code as additional argument.

As an example, we can define the fragment of a URI (per RFC 3986)
without having to look up the numeric ASCII codes
(also better documented this way!):

```prolog
fragment(Fragment) -->
  '*'(fragment_code, Codes, []),
  {atom_codes(Fragment, Codes)}.

fragment_code(Code) --> pchar(Code).
fragment_code(Code) --> forward_slash(Code).
fragment_code(Code) --> question_mark(Code).
```

This is especially useful for characers that are non-graphic
or that look similar to other characers on some displays
(e.g., carriage return and line-feed).

Numeric characters (i.e., `0-9A-Za-z`) also have a variant DCG
with a weight argument representing the decimal value of the character.

Bracket characters and bracket character groups have a variant
with an additional `Type` argument which is either `angular`, `curly`,
`round`, or `square`.
A handy special case for this occurs with content that is
(consistently) surrounded by some bracket type,
possibly returning the bracket type to the calling context:

```prolog
bracketed_content(Type, Dcg) -->
  opening_bracket(Type, _),
  Dcg,
  closing_bracket(Type, _).
```

The above can e.g. be use to parse
[Markdown URL notation](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet#links):

```prolog
markdown_url(Label, Url) -->
  bracketed_content(square, Label),
  bracketed_content(round, Url).
```



`dcg_cadinal`
-------------

The following DCG rules process integers between a lower and an unpper bound:

  - `between(+Low:integer, +High:integer[, ?Value:integer])`
    Processes integers between the given lower and higher bounds.
  - `between_digit(+Low:hex, +High:hex[, ?Value:hex])`
    Processes digits between the given lower and higher bounds.
    `hex` is defined as `or([between(0,9),oneof([a,b,c,d,e,f])])`.
  - `between_radix(+Low:compound, +High:compound[, ?Value:compound])`
    The values are either integers (in decimal base) or compound terms
    (`bin/1`, `oct/1`, `dec/1`, `hex/1`)
    representing numbers in different bases
    (binary, octal, hexadecimal).
    ```prolog
    ?- phrase(between_radix(bin(1001), hex(f), oct(X)), Codes).
    X = 11,
    Codes = [57] ;
    X = 12,
    Codes = [49, 48] ;
    X = 13,
    Codes = [49, 49] ;
    X = 14,
    Codes = [49, 50] ;
    X = 15,
    Codes = [49, 51] ;
    X = 16,
    Codes = [49, 52] ;
    X = 17,
    Codes = [49, 53].
    ```

--

Authors:

  - [Wouter Beek](http://www.wouterbeek.com)
  - [Anne Ogborn](https://github.com/Anniepoo)

Developed during: 2013-2014
