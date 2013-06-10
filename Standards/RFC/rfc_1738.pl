:- module(rfc_1738, []).

/** <module> RFC 1738

Specifies the syntax and semantics for localizing and accessing resources
via the Internet (URLs).

# General URL form

~~~{.txt}
<url> = <scheme>:<scheme-specific-part>
~~~

The former part is the name of the scheme used.

The latter part is interpreted relative to the scheme used.

# Encoding of URLs

  1. Character sequences in parts of a URL represent sequences of octets.
     The mapping uses the US-ASCII code character set.
  1. Octets may be encoded by a character triplet consisting
     of the character "%" followed by the two hexadecimal digits.
  1. Safe characters: uppercase and lowercase letters, decimal digits,
     and =|$-_.*!*'(),|=.
  1. Letters are interpreted case-insensitively.

Octets that must be encoded using %-escaping:
  1. Octets with no corresponding graphic: 80-ff (unused) and
     00-1f, 7f (control).
  1. *|Unsafe characters|*:
     * _Spaces_
       Transcribing/typesetting URLs may remove/add spaces.
     * Characters =|<>|=
       URL delimiters in free text.
     * _|Double quote mark|_
       URL delimiter in free text.
     * _|Hash character|_
       URL fragment delimiter
     * _|Percent sign|_
       Used for character encoding.
     * Characters =|{}|\^~[]`|=,
       Characters that are sometimes modified by gateways and other
       transport agents.
  1. *|Reserved characters|* that may have a special meaning according to a
     scheme: =|;/?:@=&|= (this is a closed collection).

# Scheme-specific syntax

URL schemes that involve the direct use of an IP-based protocol to a
specified host on the Internet use a common syntax for the scheme-specific
data (=|[]|= means optional part):

~~~{.txt}
<scheme-specific-part> = //[<user>[:<password>]@]<host>[:<port>][/<url-path>]
~~~

## Host

  * Fully qualified domain name of a network host.
    Dot-separated sequence of domain labels, starting and ending with an
    alphanumeric character, possibly containing dashes, the rightmost
    domain label does not start with a digit.
  * IP address of a network host.
    Four dot-separated decimal digit groups.

# Schemes

## FTP

Default password: the Internet e-mail address of the end user. [???]
Default port: 21.
Default user: anonymous.

~~~{.txt}
<url-path> = <cwd1>/<cwd2>/.../<cwdN>/<name>[;type=<typecode>]
~~~

CWD (Change Working Directory) commands <cwdi>

<typecode> is one of:
  * =a=
    TYPE command with argument =a=.
  * =d=
    NLST (name list) command with argument =|<name>|=, returning a file
    directory listing.
  * =i=
    TYPE command with argument =i=.

<cwdi> and <name> are (possibly encoded) strings with '/' and ';' encoded.

### Examples

~~~{.txt}
ftp://myname@host.dom/%2Fetc/motd
"CWD /etc", "RETR motd"
~~~

~~~{.txt}
ftp://myname@host.dom//etc/motd
"CWD ", "CWD etc", "RETR motd"
~~~

## HTTP

~~~{.txt}
<url> = http://<host>[:<port>]/[<path>][?][<searchpart>]
~~~

  * Default port: 80.
  * No user name, no password.
  * =|<path>|= is a HTTP selector.
  * =|<searchpart>|= is a query string.
  * If =|<path>|= and =|<searchpart>|= are both omitted, then the last '/'
    may be omitted as well.
  * '/' _may_ be used to designate a hierarchical structure.
  * Reserved chaarcters in =|<path>|= and =|<searchpart>|=: =|/;?|=

## GOPHER

~~~{.txt}
<url> = gopher://<host>[:<port>]/[<gopher-path>]
~~~

=|<gopher-path>|= is one of:
  * =|<gophertype>[<selector>]|=
  * =|<gophertype>[<selector>]%09<search>|=
  * =|<gophertype>[<selector>]%09[<search>]%09<gopher+_string>|=

  * Default port: 70.
  * =|<gophertype>|= is a single-character field.
  * Default =|<gophertype>|=: 1.
  * =|<selector>|= is a sequence of octets, excluding 09 (horizontal tab),
    0A (line feed), 0D (carriage return).
  * If =|<gopher-path>|= is missing, then the last '/' is optional.
  * The empty =|<selector>|= refers to the top-level directory.
  * =|%09|= is an encoded horizontal tab.

@tbd

## MAILTO

~~~{.txt}
<url> = mailto:<rfc822-addr-spec>
~~~

"Unlike many URLs, the mailto scheme does not represent a data object
to be accessed directly; there is no sense in which it designates an object.
It has a different use than the message/external-body type in MIME." [???]

## NEWS

~~~{.txt}
<url> = news:<newsgroup-name>
<url> = news:<message-id>
~~~

  * =|<newsgroup-name> is a period-delimited hierarchical name.
  * =|<newsgroup-name> = *|= refers to all available news groups.
  * =@= is a reserved character in =|<newsgroup-name>|= and =|<message-id>|=.

~~~{.txt}
<message-id> = <unique>@<full_domain_name>
~~~

"The news URLs are unusual in that by themselves, they do not contain
sufficient information to locate a single resource, but, rather, are
location-independent." [???]

## NNTP

~~~{.txt}
<url> = nntp://<host>[:<port>]/<newsgroup-name>/<article-number>
~~~

  * Default port: 119.

## TELNET

Designates a service, not a data object.

~~~{.txt}
<url> = telnet://[<user>[:<password>]]@<host>[:<port>]/
~~~

  * If =|<port>|= is omitted, then the last '/' may be omitted.
  * Default port: 23.

## WAIS

Designate a WAIS database:
~~~{.txt}
<url> = wais://<host>[:<port>]/<database>
~~~

Designates a particular search:
~~~{.txt}
<url> = wais://<host>[:<port>]/<database>?<search>
~~~

Designates a particular document:
~~~{.txt}
<url> = wais://<host>[:<port>]/<database>/<wtype>/<wpath>
~~~

  * Default port: 210.

# FILES

Designate files accessible on a particular host computer, i.e. _not_
universally accessible files.

~~~{.txt}
<url> = file://[<host>/]<path>
~~~

  * =|<host>|= is a fully qualified domain name or =localhost=.
  * Default host: =localhost=.
  * =|<path>|= is a hierarchical directory path
    =|<directory>/<directory>/.../<name>|=.

## PROSPERO

~~~{.txt}
<url> = prospero://<host>[:<port>]/<hsoname>;<field>=<value>
~~~

  * Default port: 1525.

@tbd

# Registration of new schemes

IANA (Internet Assigned Numbers Authority) will maintain a registry
of URL schemes.

The submission of a new scheme must include a definition of an algorithm
for accessing resources and the syntax for representing locations.

  * Prefix =|x-|=
    Reserved for experimental purposes.

## Proposed schemes

  * *afs*
    Andrew File System global file names.
  * *mid*
    Message identifiers for electronic mail.
  * *cid*
    Content identifiers for MIME body parts.
  * *nfs*
    Network File System (NFS) file names.
  * *tn3270*
    Interactive 3270 emulation sessions.
  * *mailserver*
    Access to data available from mail servers.
  * *z39.50*
    Access to ANSI Z39.50 services.

# BNF

~~~{.txt}
; The generic form of a URL is:
genericurl     = scheme ":" schemepart

; Specific predefined schemes are defined here; new schemes
; may be registered with IANA
url            = httpurl | ftpurl | newsurl |
                 nntpurl | telneturl | gopherurl |
                 waisurl | mailtourl | fileurl |
                 prosperourl | otherurl

; new schemes follow the general syntax
otherurl       = genericurl

; the scheme is in lower case; interpreters should use case-ignore
scheme         = 1*[ lowalpha | digit | "+" | "-" | "." ]
schemepart     = *xchar | ip-schemepart

; URL schemeparts for ip based protocols:
ip-schemepart  = "//" login [ "/" urlpath ]

login          = [ user [ ":" password ] "@" ] hostport
hostport       = host [ ":" port ]
host           = hostname | hostnumber
hostname       = *[ domainlabel "." ] toplabel
domainlabel    = alphadigit | alphadigit *[ alphadigit | "-" ] alphadigit
toplabel       = alpha | alpha *[ alphadigit | "-" ] alphadigit
alphadigit     = alpha | digit
hostnumber     = digits "." digits "." digits "." digits
port           = digits
user           = *[ uchar | ";" | "?" | "&" | "=" ]
password       = *[ uchar | ";" | "?" | "&" | "=" ]
urlpath        = *xchar    ; depends on protocol see section 3.1

; The predefined schemes:

; FTP (see also RFC959)
ftpurl         = "ftp://" login [ "/" fpath [ ";type=" ftptype ]]
fpath          = fsegment *[ "/" fsegment ]
fsegment       = *[ uchar | "?" | ":" | "@" | "&" | "=" ]
ftptype        = "A" | "I" | "D" | "a" | "i" | "d"

; FILE
fileurl        = "file://" [ host | "localhost" ] "/" fpath

; HTTP
httpurl        = "http://" hostport [ "/" hpath [ "?" search ]]
hpath          = hsegment *[ "/" hsegment ]
hsegment       = *[ uchar | ";" | ":" | "@" | "&" | "=" ]
search         = *[ uchar | ";" | ":" | "@" | "&" | "=" ]

; GOPHER (see also RFC1436)
gopherurl      = "gopher://" hostport [ / [ gtype [ selector
                 [ "%09" search [ "%09" gopher+_string ] ] ] ] ]
gtype          = xchar
selector       = *xchar
gopher+_string = *xchar

; MAILTO (see also RFC822)
mailtourl      = "mailto:" encoded822addr
encoded822addr = 1*xchar               ; further defined in RFC822

; NEWS (see also RFC1036)
newsurl        = "news:" grouppart
grouppart      = "*" | group | article
group          = alpha *[ alpha | digit | "-" | "." | "+" | "_" ]
article        = 1*[ uchar | ";" | "/" | "?" | ":" | "&" | "=" ] "@" host

; NNTP (see also RFC977)
nntpurl        = "nntp://" hostport "/" group [ "/" digits ]

; TELNET
telneturl      = "telnet://" login [ "/" ]

; WAIS (see also RFC1625)
waisurl        = waisdatabase | waisindex | waisdoc
waisdatabase   = "wais://" hostport "/" database
waisindex      = "wais://" hostport "/" database "?" search
waisdoc        = "wais://" hostport "/" database "/" wtype "/" wpath
database       = *uchar
wtype          = *uchar
wpath          = *uchar

; PROSPERO
prosperourl    = "prospero://" hostport "/" ppath *[ fieldspec ]
ppath          = psegment *[ "/" psegment ]
psegment       = *[ uchar | "?" | ":" | "@" | "&" | "=" ]
fieldspec      = ";" fieldname "=" fieldvalue
fieldname      = *[ uchar | "?" | ":" | "@" | "&" ]
fieldvalue     = *[ uchar | "?" | ":" | "@" | "&" ]

; Miscellaneous definitions
lowalpha       = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" |
                 "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" |
                 "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" |
                 "y" | "z"
hialpha        = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" |
                 "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" |
                 "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
alpha          = lowalpha | hialpha
digit          = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" |
                 "8" | "9"
safe           = "$" | "-" | "_" | "." | "+"
extra          = "!" | "*" | "'" | "(" | ")" | ","
national       = "{" | "}" | "|" | "\" | "^" | "~" | "[" | "]" | "`"
punctuation    = "<" | ">" | "#" | "%" | <">
reserved       = ";" | "/" | "?" | ":" | "@" | "&" | "="
hex            = digit | "A" | "B" | "C" | "D" | "E" | "F" |
                 "a" | "b" | "c" | "d" | "e" | "f"
escape         = "%" hex hex
unreserved     = alpha | digit | safe | extra
uchar          = unreserved | escape
xchar          = unreserved | reserved | escape
digits         = 1*digit
~~~

# URLs in context

To distinguish URLs from other data structures (e.g. URI), use prefix
=|URL:|=.

To distinguish URLs from other text (e.g. natural language punctuation),
use enclosing angle brackets along with the prefix =|URL:|=.
  * Whitespace inside the enclosing brackets is ignored.
  * No whitespace should follow a hyphen, since typesetters and printers
    may (erroneously) introduce a hyphen at the end of a line when breaking
    a line.

# References

[1] Anklesaria, F., McCahill, M., Lindner, P., Johnson, D.,
       Torrey, D., and B. Alberti, "The Internet Gopher Protocol
       (a distributed document search and retrieval protocol)",
       RFC 1436, University of Minnesota, March 1993.
       <URL:ftp://ds.internic.net/rfc/rfc1436.txt;type=a>

   [2] Anklesaria, F., Lindner, P., McCahill, M., Torrey, D.,
       Johnson, D., and B. Alberti, "Gopher+: Upward compatible
       enhancements to the Internet Gopher protocol",
       University of Minnesota, July 1993.
       <URL:ftp://boombox.micro.umn.edu/pub/gopher/gopher_protocol
       /Gopher+/Gopher+.txt>

   [3] Berners-Lee, T., "Universal Resource Identifiers in WWW: A
       Unifying Syntax for the Expression of Names and Addresses of
       Objects on the Network as used in the World-Wide Web", RFC
       1630, CERN, June 1994.
       <URL:ftp://ds.internic.net/rfc/rfc1630.txt>

   [4] Berners-Lee, T., "Hypertext Transfer Protocol (HTTP)",
       CERN, November 1993.
       <URL:ftp://info.cern.ch/pub/www/doc/http-spec.txt.Z>

   [5] Braden, R., Editor, "Requirements for Internet Hosts --
       Application and Support", STD 3, RFC 1123, IETF, October 1989.
       <URL:ftp://ds.internic.net/rfc/rfc1123.txt>

   [6] Crocker, D. "Standard for the Format of ARPA Internet Text
       Messages", STD 11, RFC 822, UDEL, April 1982.
       <URL:ftp://ds.internic.net/rfc/rfc822.txt>

   [7] Davis, F., Kahle, B., Morris, H., Salem, J., Shen, T., Wang, R.,
       Sui, J., and M. Grinbaum, "WAIS Interface Protocol Prototype
       Functional Specification", (v1.5), Thinking Machines
       Corporation, April 1990.
       <URL:ftp://quake.think.com/pub/wais/doc/protspec.txt>

   [8] Horton, M. and R. Adams, "Standard For Interchange of USENET
       Messages", RFC 1036, AT&T Bell Laboratories, Center for Seismic
       Studies, December 1987.
       <URL:ftp://ds.internic.net/rfc/rfc1036.txt>

   [9] Huitema, C., "Naming: Strategies and Techniques", Computer
       Networks and ISDN Systems 23 (1991) 107-110.

  [10] Kahle, B., "Document Identifiers, or International Standard
       Book Numbers for the Electronic Age", 1991.
       <URL:ftp://quake.think.com/pub/wais/doc/doc-ids.txt>

  [11] Kantor, B. and P. Lapsley, "Network News Transfer Protocol:
       A Proposed Standard for the Stream-Based Transmission of News",
       RFC 977, UC San Diego & UC Berkeley, February 1986.
       <URL:ftp://ds.internic.net/rfc/rfc977.txt>

  [12] Kunze, J., "Functional Requirements for Internet Resource
       Locators", Work in Progress, December 1994.
       <URL:ftp://ds.internic.net/internet-drafts
       /draft-ietf-uri-irl-fun-req-02.txt>

  [13] Mockapetris, P., "Domain Names - Concepts and Facilities",
       STD 13, RFC 1034, USC/Information Sciences Institute,
       November 1987.
       <URL:ftp://ds.internic.net/rfc/rfc1034.txt>

  [14] Neuman, B., and S. Augart, "The Prospero Protocol",
       USC/Information Sciences Institute, June 1993.
       <URL:ftp://prospero.isi.edu/pub/prospero/doc
       /prospero-protocol.PS.Z>

  [15] Postel, J. and J. Reynolds, "File Transfer Protocol (FTP)",
       STD 9, RFC 959, USC/Information Sciences Institute,
       October 1985.
       <URL:ftp://ds.internic.net/rfc/rfc959.txt>

  [16] Sollins, K. and L. Masinter, "Functional Requirements for
       Uniform Resource Names", RFC 1737, MIT/LCS, Xerox Corporation,
       December 1994.
       <URL:ftp://ds.internic.net/rfc/rfc1737.txt>

  [17] St. Pierre, M, Fullton, J., Gamiel, K., Goldman, J., Kahle, B.,
       Kunze, J., Morris, H., and F. Schiettecatte, "WAIS over
       Z39.50-1988", RFC 1625, WAIS, Inc., CNIDR, Thinking Machines
       Corp., UC Berkeley, FS Consulting, June 1994.
       <URL:ftp://ds.internic.net/rfc/rfc1625.txt>

  [18] Yeong, W. "Towards Networked Information Retrieval", Technical
       report 91-06-25-01, Performance Systems International, Inc.
       <URL:ftp://uu.psi.com/wp/nir.txt>, June 1991.

  [19] Yeong, W., "Representing Public Archives in the Directory",
       Work in Progress, November 1991.

  [20] "Coded Character Set -- 7-bit American Standard Code for
       Information Interchange", ANSI X3.4-1986.

@author Wouter Beek
@compat RFC 1738
@see http://www.ietf.org/rfc/rfc1738.txt
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
  rdf_global_id(rfc:'1738', This),
  rdf_assert_datatype(This, rfc:year, gYear, 1994, Graph),
  rdf_assert_literal(This, rfc:title, en, 'Uniform Resource Locators (URL)', Graph),
  rdf_assert_literal(This, rfc:author, en, 'Tim Berners-Lee', Graph),
  rdf_assert_literal(This, rfc:author, en, 'L. Masinter', Graph),
  rdf_assert_literal(This, rfc:author, en, 'M. McCahill', Graph),
  rdf_assert(This, foaf:homepage, 'http://www.ietf.org/rfc/rfc1738.txt', Graph),
  rdf_assert(This, rfc:mentions, rfc:'822', Graph), % MAILTO, BNF
  rdf_assert(This, rfc:mentions, rfc:'959', Graph), % FTP
  rdf_assert(This, rfc:mentions, rfc:'977', Graph), % NNTP
  rdf_assert(This, rfc:mentions, rfc:'1036', Graph), % NEWS
  rdf_assert(This, rfc:mentions, rfc:'1436', Graph), % GOPHER
  rdf_assert(This, rfc:mentions, rfc:'1625', Graph), % WAIS
  rdf_assert(This, rfc:mentions, rfc:'1630', Graph). % URIs in WWW
:- init.

