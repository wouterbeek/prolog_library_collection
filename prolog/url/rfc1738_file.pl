:- module(
  rfc1738_file,
  [
    fileurl//2 % ?Host:or([list(nonneg),list(string)])
               % ?Path:list(string)
  ]
).

/** <module> RFC 1738: Uniform Resource Locators (URL)

@author Wouter Beek
@compat RFC 1738
@deprecated Use module `rfc????` instead.
@version 2015/11
*/

:- use_module(library(url/rfc1738_component)).
:- use_module(library(url/rfc1738_ftp)).





%! fileurl(?Host:or([list(nonneg),list(string)]), ?Path:list(string))// .
% ```abnf
% fileurl = "file://" [ host | "localhost" ] "/" fpath
% ```

fileurl(Host, Path) -->
  "file://",
  (host(Host), ! ; "localhost", {Host = localhost}),
  "/",
  fpath(Path).
