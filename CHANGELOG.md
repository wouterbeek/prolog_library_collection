# CHANGELOG

## 1.0.11 (2021-03-20)

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
