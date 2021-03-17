# CHANGELOG

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
