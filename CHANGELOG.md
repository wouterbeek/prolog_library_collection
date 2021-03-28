# CHANGELOG

## 1.0.13 (2021-03-28)

This release introduces support for creating simple command-line
interfaces.  The SWI-Prolog standard library includes a module called
`optparse`, but is relatively complex and performs many exponential
checks.  Also, the here included support library is intended to be
decomposed into reusable parts (e.g., calculating width-delimited text
is part of `string_ext`).  The command-line interface support is only
an initial version currently, but the intention is to extended this
over time.

Here follows the full list of changes:

### call_ext

- Renamed `call_bool/2` to `call_boolean/2`.  This is more consistent
  with the name of the Prolog type `boolean`.

### cli, cli\_arguments, cli\_help, cli\_version

- Added a new module that makes it easy to write CLI tools.

### dcg

- Added `dcg_char//1` for parsing/generating one single character.

- Renamed `dcg_bool//1` to `dcg_pp_boolean//1`.  This is more
  consistent with the name of the Prolog type `boolean`, and indicates
  the pretty-print purpose better.

- Added `dcg_boolean//1` for parsing/generating Boolean values.

- Added `dcg_peek//1` which can be useful for simple debugging.

### dict

- Fixed instantiation `(-, +, ?)` of predicate `dict_get/3`.

### print_ext

- Renamed `call_print_bool/1` to `call_print_boolean/1` and renamed
  `print_bool/1` to `print_boolean/1`.  These are more consistent with
  the name of the Prolog type `boolean`.

### rest-server

- Removed `http_server_init/1`.  It is better to use the SWI-Prolog
  standard library predicates for initialization an HTTP server
  instead.

### string_ext

- Added `max_string_length/2` for calculating the length of the
  longest given string.

- Added `message_lines/3` and `words_lines/[3,4]` for generating
  width-delimited displays of text.

## 1.0.12 (2021-03-20)

This release introduces many improvements to the `media_type` library,
together with several smaller improvements and some bug fixes (see
below).

### jgf

= Fixed the use of an outdated standard library predicate in predicate
  `jgf_reply/2`.

### media_type

- A big rewrite of how Media Types are stored.

- A new Prolog type called `media_type` that is used to indicate Media
  Type compound terms.

- Better support for Media Types with identical file name extensions
  (e.g., `gml` is used for Geography Markup Language and for Graph
  Markup Language).

- Removal of the following less used predicates: `extension_label/2`,
  `media_type_comps/4`.

- Changed the API for retrieving Media Type parameters: parameters are
  now represented as pairs of atoms.

- A new API for accessing 'families' of grouped Media Types:
  `media_type_family/2`.

### pagination

- Fixed the use of an outdated predicate from the `dict` module in
  predicate `pagination_options/5`.

### string_ext

- Added support for `inf` as the maxmimum length that is used in
  `string_ellipsis/3`.  This makes it easier to specify that no
  ellipsis should be applied in some contexts.

### term_ext

- New predicates for generating simple, ASCII-based IDs:
  `ascii_id/[1,2]`.  These IDs are supported by many external
  languages and tools (e.g., C names, DOT IDs).

- Added a Prolog type declarations for optional types: `maybe(T)`.

### tree

- New module, based on a generalization of the old `proof` module.
  This module current includes the following predicates: `depth/2` and
  `shortest/2`.

## 1.0.10 (2021-03-17)

This release fixes one bug in module `http_client2` and changes the
behavior of the CLI argument handling predicates (module `conf`).  It
also introduces several small additions in other modules (see below).

### conf

- Changed the predicates for handling CLI arguments
  (`cli_arguments/[1-3]`).

### debug_gui

- Added `gtrace_failure/1`.

### dict

- Added `dict_change_keys/3`.

### http_client2

- Fixed retrieving the metadata from HTTP requests.

### pair_ext

- Added `change_keys/3`.
- Added `group_values_by_key/2`.

### stream_ext

- Support encoding `iso-8859-1` (an alias of `iso_latin_1`).

### term_ext

- Define a term `options` for dictionaries with tag 'options' and used
  it everywhere options are used.
