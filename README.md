# Prolog-Library-Collection (PLC)

A collection of Prolog libraries that have proven useful in various
projects.  These libraries are intended to extend the functionality
that is already available in the
[SWI-Prolog](http://www.swi-prolog.org) standard libraries.

## Installation

  1. Install [SWI-Prolog](http://www.swi-prolog.org).

  2. Run Prolog with the following command:

```sh
swipl
```

  3. Make the following call inside Prolog:

```pl
:- pack_install('Prolog-Library-Collection').
```

That it!  You can now load modules from this library in the following way:

```pl
:- [library(atom_ext)].
```

## Module overview

## Module `archive_ext`

This module extends the standard library `archive`:

  - `archive_extension(?Extension:atom)`

    Succeeds if `Extension` is a default file name extension for an
    archive filter or format, as declared in library [[media_type]].

  - `archive_media_type(?MediaType:media)`

    Succeeds if `MediaType` is the Media Type of an archive filter or
    format.

  - `archive_open(+In:stream, -Archive:blob)`

    Opens an archive over all supported and sensible archive filters
    and formats.  This specifically excludes format `mtree`, which is
    a plain text format that is almost never used yet leads to many
    false positives in practice.

## Module `assoc_ext`

This module extends the standard library `assoc`:

  - `merge_assoc(+New:assoc, +Old:assoc, -Merge:assoc)`

    Merges two assocs into a new one.  If the same key exists in `New`
    and `Old`, the format replaces the latter in `Merge`.  These
    semantics are inspired by those of the standard library predicate
    `merge_options/3` in library `option`.

  - `transpose_assoc(+Assoc:assoc, -Transposed:assoc)`

    Turns an assoc of (key,value) pairs into one with(value,key)
    pairs.

## Module `atom_ext`

This module provides additional support for working with atoms:

  - `atom_capitalize(+Original:atom, ?Capitalized:atom)`

    Succeeds if `Capitalized` is a copt of `Orginal` where the first
    character is in upper case (if applicable).

  - `atom_ellipsis(+Original:atom, ?MaxLength:between(2,inf), ?Ellipsed:atom)`

    Succeeds if `Ellipsed` is like `Original`, but has ellipsis
    applied in order to have `MaxLength`.

  - `atom_postfix(+Original:atom, ?PostFix:atom)`
  - `atom_postfix(+Original:atom, ?Length:nonneg, ?PostFix:atom)`

    Succeeds if `Postfix` is a postfix of `Original` consisting of
    `Length` characters.


  - `atom_prefix(+Original:atom, ?PostFix:atom)`
  - `atom_prefix(+Original:atom, ?Length:nonneg, ?PostFix:atom)`

    Succeeds if `Prefix` is a prefix of `Original` consisting of
    `Length` characters.

  - `atom_strip(+Original:atom, ?Stripped:atom)`
  - `atom_strip(+Original:atom, +Strip:list(char), ?Stripped:atom)`

    Succeeds if `Stripped` is a copy of `Original` where leading and
    trailing characters in `Strip` have been removed.

  - `atom_terminator(+Original:atom, +Terminator:atom, ?Terminated:atom)`

    Succeeds if `Terminated` is a copy of `Original` which is ensured
    to end with the `Terminator` character.

  - `atom_truncate(+Original:atom, +MaxLenhgt:noneng, ?Truncated:atom)`

    Like `atom_prefix/3`, but the `Truncated` atom is the `Original`
    atom in case `MaxLength` exceeds the `Original` atom length.

## `call_ext`

meta-predicates

## `closure`

## `code_ext`

This module extends support for working with character-denoting
numeric codes:

   - `put_codes(+Codes:list(code))`
   - `put_codes(+Out:stream, +Codes:list(code))`

## `conf_ext`

This module introduces a generic way for dealing with external
configuration files:

   - `cli_arguments(-Args:list(opt)) is det.`
   - `conf_json(-Conf:dict) is det.`

## `counter`

## `csv_ext`

Streamed processing of CSV files.

## `date_time`

## `dcg`

Definite Clause Grammars

In directory `/dcg` you will find a collection of Definite Clause
Grammar (DCG) modules.

## `dcg/dcg_abnf`

Advanced Bauckus-Naur Form (ABNF)

While DCGs are nice, they can be a bit verbose for expressing common
repetition patterns.  To make DCGs that include repetitions less
verbose, this module implements *variable repetition* as defined in
[[https://tools.ietf.org/html/rfc5234][RFC 5234: Augmented BNF for
Syntax Specifications: ABNF]].

### A simple example

Suppose we want to parse sentences, which are non-empty sequences of
words:

```pl
sentence1([H|T]) -->
  word(H),
  sentece2(T).

sentence2([H|T]) -->
  word(H),
  sentence2(T)
sentence2([]) --> "".
```

When this module is loaded, the same can be written as follows:

```pl
sentence(L) -->
  +(word, L).
```

### definition

#### variable repetition

Variable repetition is a metasyntactic construct which states that at
least `M` and at most `N` occurrences of `:Dcg_0` must be processed:

```pl
'm*n'(?M:nonneg, ?N:nonneg, :Dcg_0)//
```

#### specific repetition

Specific repetition is a metasyntactic construct which states that
exactly `N` occurrences of `Dcg_0` must be processed:

```pl
'#'(?N:nonneg, :Dcg_0)//
```

Specific repetition is a special case of [[variable repetition]],
because `#(N, Dcg_0)` is the same as `'m*n'(N, N, Dcg_0)`.

#### Kleene

Kleene star is a metasyntactic construct which states that zero or
more occurrences of `Dcg_0` must be processed:

```pl
*(?N:nonneg, :Dcg_0)//
```

Kleene star is a special case of [[variable repetition]], because
`*(N, Dcg_0)` is the same as `'m*n'(_, _, Dcg_0)`.

#### Kleene sum

Kleene sum is a metasyntactic construct which states that one or more
occurrences of `Dcg_0` must be processed:

```pl
+(?N:nonneg, :Dcg_0)//
```

Kleene sum is a special case of [[variable repetition]], because `+(N,
Dcg_0)` is the same as `'m*n'(1, _, Dcg_0)`.

#### optional sequence

Optional sequence is a metasyntactic construct which states that
`Dcg_0` should either be processed once or not at all:

```pl
?(:Dcg_0)//
```

Optional sequence is a special case of [[variable repetition]],
because `?(Dcg_0)` is the same as `'m*n'(0, 1, Dcg_0)`.

  | *DCG*                     | *Meaning*                                           | *Name*              |
  |---------------------------+-----------------------------------------------------+---------------------|
  | `#(?N, :Dcg_0)//`         | Process `Dcg_0` exactly `N` times.                  | specific repetition |
  | `*(:Dcg_0)//`             | Process `Dcg_0` 0 or more times.                    | Kleene star         |
  | `'*n'(?N, :Dcg_0)//`      | Process `Dcg_0` at most `N` times.                  |                     |
  | `+(:Dcg_0)//`             | Process `Dcg_0` 1 or more times.                    | Kleene sum          |
  | `?(:Dcg_0)//`             | Process `Dcg_0` 0 or 1 times.                       | optional sequence   |
  | `'m*'(?M, :Dcg_0)//`      | Process `Dcg_0` at least `M` times.                 |                     |
  | `'m*n'(?M, ?N, :Dcg_0)//` | Process `Dcg_0` at least `M` and at most `N` times. | variable repetition |

It contains the following modules:

  | *Type*  | *Definition*                                                                                                       |
  |---------+--------------------------------------------------------------------------------------------------------------------|
  | `media` | A compound term of the form `media(Super:atom/Sub:atom,Parameters:list(opt))`                                      |
  | `opt`   | A unary compound term whose predicate letter is an option name and whose argument is a corresponding option value. |

## `dcg/dcg_ext`

## `debug_ext`

## `default`

## `dict`

Dictionaries.

## `dlist`

Difference lists.

## `file_ext`

Handling files and directories.

## `graph/gml`

## `graph/graph_ext`

## `graph/jgf`

## `hash_ext`

## `http/http_client2`

## `http/http_generic`

## `http/http_pagination`

## `http/http_resource`

## `http/http_server`

## `json_ext`

This module provides extended JSON support on top of the standard
library `http/json`:

   - `json_load(+File:atom, -Structure:dict) is det.`

   - `json_save(+File:atom, +Structure:dict) is det.`

## `list_ext`

## `math_ext`

## `media_type`

## `nlp/nlp_lang`

## `os_ext`

Running external processes, streaming to/from external processes.

## `pagination`

## `pair_ext`

## `pp`

## `pure`

## `sort_ext`

## `stream_ext`

Support for recoding, unpacking, sorting, and hasing streams.

## `string_ext`

## `task`

## `term_ext`

## `thread_ext`

## `uri_ext`

Constructing/decomposing URIs.

## `xml_ext`

This module allows Prolog goals to be called on a stream that encodes
an XML DOM:

   - `call_on_xml(+In:stream, +Names:list(atom), :Goal_1) is det.`

The following predicates allow the encoding of an XML file or stream
to be determined:

   - `xml_encoding(+In:stream, -Encoding:atom) is semidet.`
   - `xml_file_encoding(+File:atom, -Encoding:atom) is semidet.`
