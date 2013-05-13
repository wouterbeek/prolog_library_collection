:- module(rfc_1808, []).

/** <module> RFC 1808

Defines the syntax and semantics for such Relative Uniform Resource Locators.

---+ Purpose

---++ Conciseness

Absolute URLs contain a great deal of information which may already
be known from the context of the base document's retrieval, including
the scheme, network location, and parts of the URL path.
In situations where the base URL is well-defined and known, it is useful
to be able to embed a URL reference which inherits that context
rather than re-specifying it within each instance.

---++ Portability

Relative addressing of URLs allows document trees to be partially
independent of their location and access scheme.

---+ Generic-RL syntax

==
<url> = <scheme>://[<net_loc>][/<path>][;<params>][?<query>][#<fragment>]
==

  * =|<scheme>|=
    Scheme name.
  * =|<net_loc>|=
    Network location and login information.
  * =|/<path>|=
    URL path.
  * =|;<params>|=
    Object parameters.
  * =|<query>|=
    Query information.
  * =|#<fragment>|=
    Fragment identifier.
    Strictly speaking not part of the URL.

---++ BNF

This is _not_ the same as the RFC 1738 BNF! For instance, this allows
login information to be part of an HTTP-scheme URL! This BNF is only used
for _parsing_ purposes.

==
URL         = ( absoluteURL | relativeURL ) [ "#" fragment ]
absoluteURL = generic-RL | ( scheme ":" *( uchar | reserved ) )
generic-RL  = scheme ":" relativeURL
relativeURL = net_path | abs_path | rel_path

net_path    = "//" net_loc [ abs_path ]
abs_path    = "/"  rel_path
rel_path    = [ path ] [ ";" params ] [ "?" query ]

path        = fsegment *( "/" segment )
fsegment    = 1*pchar
segment     =  *pchar

params      = param *( ";" param )
param       = *( pchar | "/" )

scheme      = 1*( alpha | digit | "+" | "-" | "." )
net_loc     =  *( pchar | ";" | "?" )
query       =  *( uchar | reserved )
fragment    =  *( uchar | reserved )

pchar       = uchar | ":" | "@" | "&" | "="
uchar       = unreserved | escape
unreserved  = alpha | digit | safe | extra

escape      = "%" hex hex
hex         = digit | "A" | "B" | "C" | "D" | "E" | "F" |
                      "a" | "b" | "c" | "d" | "e" | "f"
alpha       = lowalpha | hialpha
lowalpha    = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" |
              "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" |
              "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
hialpha     = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" |
              "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" |
              "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
digit       = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" |
              "8" | "9"
safe        = "$" | "-" | "_" | "." | "+"
extra       = "!" | "*" | "'" | "(" | ")" | ","
national    = "{" | "}" | "|" | "\" | "^" | "~" | "[" | "]" | "`"
reserved    = ";" | "/" | "?" | ":" | "@" | "&" | "="
punctuation = "<" | ">" | "#" | "%" | <">
==

---+++ Error in RFC 1738

RFC 1738 specifies that the question-mark character is allowed in an FTP or
FILE path segment. However, this is not true in practice and is believed to
be an error in the RFC.

---+++ Extension of RFC 1738

RFC 1738 allows the reserved character semicolon within an HTTP path segment,
but does not define its semantics; the correct semantics are as defined
by this document for =|<params>|=.

---+++ Relative URLs in different schemes

Schemes that never use relative URLs:
  * *mailto*
    Electronic Mail
  * *news*
    USENET news
  * *telnet*
    TELNET Protocol for Interactive Sessions

Schemes that allow the use of reserved characters for purposes outside the
generic-RL syntax given above. Relative URLs can be used with these schemes
whenever the applicable base URL follows the generic-RL syntax.
  * *gopher*
    Gopher and Gopher+ Protocols
  * *prospero*
    Prospero Directory Service
  * *wais*
    Wide Area Information Servers Protocol

Schemes that can always be parsed using the generic-RL syntax:
  * *file*
    Host-specific Files
  * *ftp*
    File Transfer Protocol
  * *http*
    Hypertext Transfer Protocol
  * *nntp*
    USENET news using NNTP access

---+ URL parser

  1. The substring after the first crosshatch character ('#') until the end
     of the string is the *|fragment identifier|*.
  1. If the string contains a color (':') after the first character and
     before any disallowed scheme name characters (anything but alphanumeric,
     plus, period, hyphen), then the substring from the beginning up to but
     not including that colon is the scheme name.
  1. If the string begins with double slash, then the substring after the
     double slash and up to but not including the next slash, or else until
     the end of the string, is the network location/login.
  1. The substring starting after the leftmost question mark and up to the
     end of the string is the query information. If there is not question
     mark then the query information is the empty string.
  1. The substring starting after the first semicolon and up to the end of
     the string is the parameters substring. If there is no semicolon, then
     the parameters substring is the empty string.
  1. What remains is the path substring.
     If this starts with a slash, then the path is absolute; it is relative
     otherwise.

---+ Establishing the base URL of a document

Ordered by presedence (higher to lower):

---++ Embedded in the document's content.

Part of the media type specification.

Messages can specify the base URL in their header; recommended format:
==
base-header  = "Base" ":" "<URL:" absoluteURL ">"
==

  * =Base= is case-insensitive.
  * Whitespace inside the angle brackets is ignored.

---++ From the encapsulating entity

If a document has no embedded base URL and it is enclosed within another
entity (e.g. media type =|message/*|=, media type =|multipart/*|=, then the
base URL of the enclosed document is the base URL of the enclosing entity.

---++ From the retrieval URL

The URL that was used to retrieve the document (i.e. the last URL in a
redirect chain) is consisdered the base URL.

---++ Default base URL

The empty string.

---+ Resolving relative URLs

This algorithm cannot guarantee that the resulting URL will equal that
intended by the original author.

This algorithm does guarantee that any valid URL (relative or absolute)
can be consistently transformed to an absolute form given a valid base URL.

Algorithm:
  1. If the base URL is empty, then the URL is absolute; return it.
  2. Parse base URL and embedded URL into their component parts (see above).
    * If the embedded URL is empty, then it is set equal to the base URL.
    * If the embedded URL starts with a scheme name, then it is absolute;
      return it.
    * Otherwise, the embedded URL inherits the scheme from the base URL.
  3. If the embbedded URL's =|<net_lov>|= is non-empty, then goto 7.
     Otherwise inherit it.
  4. If the embedded URL path is preceded by a slash, then goto 7.
  5. If the embedded path is empty, then inherit it.
     If the embedded parameters component is non-empty, then goto 7.
     Otherwise, inherit it.
     If the embedded query string is non-empty, then goto 7.
     Otherwise, inherit it.
  6. Remove the last segment of the base URL's path (i.e. if the path
     substring has a rightmost slash, then the substring starting after it;
     otherwise, the entire path string).
     Append the embedded path.
     * For all complete path segments "." with subsequent slash, "./" is
       removed.
     * If the path ends with complete segment".", then remove it.
     * Iteratively remove the leftmost occurrence of "<segment>/../",
       where <segement> is a complete path segment not equal to "..".
     * If the path ends with "<segment>/..", where segment is a complete
       path segment, remove it.
  7. Recombine components into an absolute URI; return it.

Notes:
  * The base fragment identifier is only inherited if the entire base URL is
    inherited.
  * If a relative URL contains a colon in its first path component, then
    this may be parsed incorrectly as a scheme name. Such colons must
    therefore either be encoded, or the part should be preceded by "./".
  * "There is an ambiguity in the semantics for the ftp URL scheme
    regarding the use of a trailing slash ("/") character and/or a
    parameter ";type=d" to indicate a resource that is an ftp directory.
    If the result of retrieving that directory includes embedded relative
    URLs, it is necessary that the base URL path for that result include
    a trailing slash.  For this reason, we recommend that the ";type=d"
    parameter value not be used within contexts that allow relative URLs."
    [???]

---+ Examples

Base URL:
==
<URL:http://a/b/c/d;p?q#f>
==

Relative URLs and their absolute URLs:
==
g:h     = <URL:g:h>
g       = <URL:http://a/b/c/g>
./g     = <URL:http://a/b/c/g>
g/      = <URL:http://a/b/c/g/>
/g      = <URL:http://a/g>
//g     = <URL:http://g>
?y      = <URL:http://a/b/c/d;p?y>
g?y     = <URL:http://a/b/c/g?y>
g?y/./x = <URL:http://a/b/c/g?y/./x>
#s      = <URL:http://a/b/c/d;p?q#s>
g#s     = <URL:http://a/b/c/g#s>
g#s/./x = <URL:http://a/b/c/g#s/./x>
g?y#s   = <URL:http://a/b/c/g?y#s>
;x      = <URL:http://a/b/c/d;x>
g;x     = <URL:http://a/b/c/g;x>
g;x?y#s = <URL:http://a/b/c/g;x?y#s>
.       = <URL:http://a/b/c/>
./      = <URL:http://a/b/c/>
..      = <URL:http://a/b/>
../     = <URL:http://a/b/>
../g    = <URL:http://a/b/g>
../..   = <URL:http://a/>
../../  = <URL:http://a/>
../../g = <URL:http://a/g>
==

The empty relative URL:
==

[~SWIPL] <>      = <URL:http://a/b/c/d;p?q#f>
==

More relative path ".." segments than there are hierarchical levels in the
base URL's path.

==
[~SWIPL] ../../../g    = <URL:http://a/../g>
[~SWIPL] ../../../../g = <URL:http://a/../../g>
==

"." and ".." can occur as part of complete components of a relative path:
==
[~SWIPL] /./g          = <URL:http://a/./g>
[~SWIPL] /../g         = <URL:http://a/../g>
g.            = <URL:http://a/b/c/g.>
.g            = <URL:http://a/b/c/.g>
g..           = <URL:http://a/b/c/g..>
..g           = <URL:http://a/b/c/..g>
==

Unnecessary or nonsensical uses of the "." and ".." complete path segments:
==
./../g        = <URL:http://a/b/g>
./g/.         = <URL:http://a/b/c/g/>
g/./h         = <URL:http://a/b/c/g/h>
g/../h        = <URL:http://a/b/c/h>
==

Some older parsers allow the scheme name to be present in a relative URL if
it is the same as the base URL scheme:

==
[~SWIPL] http:g        = <URL:http:g>
[~SWIPL] http:         = <URL:http:>
==

---+ References

[1] Berners-Lee, T., "Universal Resource Identifiers in WWW: A
       Unifying Syntax for the Expression of Names and Addresses of
       Objects on the Network as used in the World-Wide Web", RFC 1630,
       CERN, June 1994.

   [2] Berners-Lee, T., Masinter, L., and M. McCahill, Editors, "Uniform
       Resource Locators (URL)", RFC 1738, CERN, Xerox Corporation,
       University of Minnesota, December 1994.

   [3] Berners-Lee T., and D. Connolly, "HyperText Markup Language
       Specification -- 2.0", Work in Progress, MIT, HaL Computer
       Systems, February 1995.
       <URL:http://www.ics.uci.edu/pub/ietf/html/>

   [4] Borenstein, N., and N. Freed, "MIME (Multipurpose Internet Mail
       Extensions): Mechanisms for Specifying and Describing the Format
       of Internet Message Bodies", RFC 1521, Bellcore, Innosoft,
       September 1993.

   [5] Crocker, D., "Standard for the Format of ARPA Internet Text
       Messages", STD 11, RFC 822, UDEL, August 1982.

   [6] Kunze, J., "Functional Recommendations for Internet Resource
       Locators", RFC 1736, IS&T, UC Berkeley, February 1995.

@author Wouter Beek
@compat RFC 1808
@see http://www.ietf.org/rfc/rfc1808.txt
@version 2013/05
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(rfc, 'http://www.ietf.org/rfc/').



init:-
  Graph = rfc,
  rdf_global_id(rfc:'1808', This),
  rdf_assert_datatype(This, rfc:year, gYear, 1995, Graph),
  rdf_assert_literal(This, rfc:title, en, 'Relative Uniform Resource Locators', Graph),
  rdf_assert_literal(This, rfc:author, en, 'R. Fielding', Graph),
  rdf_assert(This, foaf:homepage, 'http://www.ietf.org/rfc/rfc1808.txt', Graph),
  rdf_assert(This, rfc:mentions, rfc:'822', Graph), % BNF
  rdf_assert(This, rfc:mentions, rfc:'1521', Graph), % MIME
  rdf_assert(This, rfc:mentions, rfc:'1630', Graph), % Partial URLs
  rdf_assert(This, rfc:mentions, rfc:'1738', Graph). % URL
:- init.

