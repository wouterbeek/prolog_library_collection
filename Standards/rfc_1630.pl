:- module(rfc_1630, []).

/** <module> RFC 1630

Documenting existing practice as of 1994/06 and a reference point for URL
and URN discussions.

---+ Definitions

    * URI, Uniform Resource Identifier
      A member of the universal set.
      A name encapsulated in a registered name space and labeled with that
      name space.
    * URL, Uniform Resource Location
      A form of URI which expresses an address which maps onto an access
      algorithm using network protocols.
    * URN, Uniform Resource Name
      A name space (and presumably resolution protocols) for persistent
      object names (more persistent than URLs).

---+ Citations

"There is one area, however, in which it is impractical to make conversions,
and that is in the names and addresses used to identify objects.
This is because names and addresses of objects are passed on in so
many ways, from the backs of envelopes to hypertext objects, and may
have a long life."

"A common feature of almost all the data models of past and proposed
systems is something which can be mapped onto a concept of "object"
and some kind of name, address, or identifier for that object.  One
can therefore define a set of name spaces in which these objects can
be said to exist."

"Practical systems need to access and mix objects which are part of
different existing and proposed systems.  Therefore, the concept of
the universal set of all objects, and hence the universal set of
names and addresses, in all name spaces, becomes important."

---+ Design criteria

    * Complete
      Any naming scheme can be encoded.
    * Extensible
      New naming schemes may be added.
    * Printable
      "It is possible to express any URI using 7-bit ASCII characters so
      that URIs may, if necessary, be passed using pen and ink."

---+ Universal syntax

    * Order
      Prefixed by an arbitrary, registered string defining the decoding of
      the rest of the string (extensibility). A prefix identifies a scheme.
      Prefixes and schemes are registered by the registration authority.
    * Punctuation of elements
      Colon separator betweeen prefix and rest.
    * Acceptable characters
      Binary names encoded in base 16 or 64 (completeness). [???]
    * Escaping rules
      The percent sign is used a the escape charcter for non-safe characters
      (printability).
      Hexadecimal escaping method.
      All white space is encoded.

---+ URI syntax

---++ Scheme

The first element in a URI, separated from the rest by a colon.

---++ Path

The rest of the URI following the colon. The encoding format depends on the
scheme.

Slashes imply a hierarchical structure.

---++ Reserved characters

    1. The *percent* sign is the escape character in the encoding scheme.
    1. The *slash* character delimits substrings whose relationship is
       hierarchical.
    1. The *hash* character separates the URI from a fragment identifier.
    1. The *|question mark|* separates the URI of a queryable object from
       a set of words used to express a query on that object.
       The combined URI stands for the object that results from the query.
    1. The *plus* sign is a shorthand notation for spaces in the query words
       segment.
    1. The *asterisk* and *|exclamation mark|* are reserved.
    1. Substrings consisting of one or two dots are reserved.
    1. Substrings that start with a percent and that are not followed by two
       hexadecimal digits.

---+ Unsafe characters

    1. Control characters
    1. Spaces
    1. Characters whose ASCII code is used differently in different national
       character variant 7bit sets. [???]
    1. 8bit characters beyond DEL (0x7f)

Unsafe characters are %-escaped (percent sign, followed by two hexadecimal
digits giving the ISO Latin 1 code).

A URI may be made 'safer' by encoding additional characters using %-encoding,
provided these do not include one of the reserved characters.

The percent sign, intended as such, must always be encoded.

---+ Comparing URIs

Before comparison, URIs must be brought to the same encoding level.

---+ Examples

---++ I

Identical; %2D encodes a hyphen character.

==
http://info.cern.ch/albert/bertram/marie-claude
http://info.cern.ch/albert/bertram/marie%2Dclaude
==

---++ II

Not identical, as in the second case the encoded slash does not
have hierarchical significance.

==
http://info.cern.ch/albert/bertram/marie-claude
http://info.cern.ch/albert/bertram%2Fmarie-claude
==

---++ III

Illegal, as all % characters imply encodings, and there is no
decoding defined for "%*"  or "%as" in this recommendation.

==
fxqn:/us/va/reston/cnri/ietf/24/asdf%*.fred
news:12345667123%asdghfh@info.cern.ch
==

---+ Partial (relative) form

Purpose: terseness, robustness (information hiding).

Using the reserved characters expressing hierarhy: slash, single and double
dot substrings.

---++ Variant I [???]

"If the partial URI starts with a non-zero number of consecutive
slashes, then everything from the context URI up to (but not
including) the first occurrence of exactly the same number of
consecutive slashes which has no greater number of consecutive
slashes anywhere to the right of it is taken to be the same and
so prepended to the partial URL to form the full URL."

---++ Variant II [???]

"The last part of the path of the context URI (anything following
the rightmost slash) is removed, and the given partial URI
appended in its place, and then:

Within the result, all occurrences of "xxx/../" or "/." are
recursively removed, where xxx, ".." and "." are complete path
elements."

---++ Note: Trailing slashes [???]

"If a path of the context locator ends in slash, partial URIs are
treated differently to the URI with the same path but without a
trailing slash. The trailing slash indicates a void segment of the
path."

---++ Examples

In the context of URIs

==
magic://a/b/c//d/e/f
magic://a/b/c//d/e/
==

the partial URIs would expand as follows:

==
g      magic://a/b/c//d/e/g
/g     magic://a/g
//g    magic://g
../g   magic://a/b/c//d/g
g:h    g:h
==

---+ Fragment-id

A fragment of/ sub-function within / part of an object.

Follows the hash sign after the object URI.

In the case of an empty fragment-id the URI refers to the whole object.

"There is no implication that a fragment identifier refers to anything
which can be extracted as an object in its own right."

---+ Schemes

---++ Implemented

    * file
      Local file access
    * ftp
      File Transfer protocol
    * gopher
      Gopher protocol
    * http
      Hypertext Transfer Protocol (examples)
    * mailto
      Electronic mail address
    * news
      Usenet news
    * telnet, rlogin and tn3270
      Reference to interactive sessions
    * wais
      Wide Area Information Servers

---++ Unimplemented

    * cid
      Content identifiers for MIME body part
    * mid
      Message identifiers for electronic mail
    * network management database
    * Whois++
    * X.500

---++ Reserved

    * Starting with "x-"
      Experimental prefixes
    * urn
      Uniform Resource Name

---+ BNF

---++ URI

==
fragmentaddress uri [ # fragmentid ]
uri             scheme : path [ ? search ]
scheme          ialpha
path            void | xpalphas [ / path ]
search          xalphas [ + search ]
fragmentid      xalphas
xalpha          alpha | digit | safe | extra | escape
xalphas         xalpha [ xalphas ]
xpalpha         xalpha | +
xpalphas        xpalpha [ xpalpha ]
ialpha          alpha [ xalphas ]
alpha           a | b | c | d | e | f | g | h | i | j | k |
                l | m | n | o  | p | q | r | s | t | u | v |
                w | x | y | z | A | B | C  | D | E | F | G |
                H | I | J | K | L | M | N | O | P |  Q | R |
                S | T | U | V | W | X | Y | Z
digit           0 |1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
safe            $ | - | _ | @ | . | &
extra           ! | * | " |  ' | ( | ) | ,
reserved        = | ; | / | # | ? | : | space
escape          % hex hex
hex             digit | a | b | c | d | e | f | A | B | C |
                D | E | F
national        { | } | vline | [ | ] | \ | ^ | ~
punctuation     < | >
void
==

---++ URL

==
  prefixedurl            u r l : url

  url                    httpaddress | ftpaddress | newsaddress |
                         nntpaddress | prosperoaddress | telnetaddress
                         | gopheraddress | waisaddress |
                         mailtoaddress  | midaddress | cidaddress

  scheme                 ialpha

  httpaddress            h t t p :   / / hostport [  / path ] [ ?
                         search ]

  ftpaddress             f t p : / / login / path [  ftptype ]

  afsaddress             a f s : / / cellname / path

  newsaddress            n e w s : groupart

  nntpaddress            n n t p : group /  digits

  midaddress             m i d  :  addr-spec

  cidaddress             c i d : content-identifier

  mailtoaddress          m a i l t o : xalphas @ hostname

  waisaddress            waisindex | waisdoc

  waisindex              w a i s : / / hostport / database [ ? search
                         ]

  waisdoc                w a i s : / / hostport / database / wtype  /
                         wpath

  wpath                  digits = path ;  [ wpath ]

  groupart               * | group | article

  group                  ialpha [ . group ]

  article                xalphas @ host

  database               xalphas

  wtype                  xalphas

  prosperoaddress        prosperolink

  prosperolink           p r o s p e r o : / / hostport / hsoname [ %
                         0 0 version [ attributes ] ]

  hsoname                path

  version                digits

  attributes             attribute [ attributes ]

  attribute              alphanums

  telnetaddress          t e l n e t : / / login

  gopheraddress          g o p h e r : / / hostport [/ gtype  [
                         gcommand ] ]

  login                  [ user [ : password ] @ ] hostport

  hostport               host [ : port ]

  host                   hostname | hostnumber

  ftptype                A formcode | E formcode | I | L digits

  formcode               N | T | C

  cellname               hostname

  hostname               ialpha [  .  hostname ]

  hostnumber             digits . digits . digits . digits

  port                   digits

  gcommand               path

  path                   void |  segment  [  / path ]

  segment                xpalphas

  search                 xalphas [ + search ]

  user                   alphanum2 [ user ]

  password               alphanum2 [ password ]

  fragmentid             xalphas

  gtype                  xalpha

  alphanum2              alpha | digit | - | _ | . | +

  xalpha                 alpha | digit | safe | extra | escape

  xalphas                xalpha [ xalphas ]

  xpalpha                xalpha | +

  xpalphas               xpalpha [ xpalphas ]

  ialpha                 alpha [ xalphas ]

  alpha                  a | b | c | d | e | f | g | h | i | j | k |
                         l | m | n | o  | p | q | r | s | t | u | v |
                         w | x | y | z | A | B | C  | D | E | F | G |
                         H | I | J | K | L | M | N | O | P |  Q | R |
                         S | T | U | V | W | X | Y | Z

  digit                  0 |1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

  safe                   $ | - | _ | @ | . | &  | + | -

  extra                  ! | * |  " |  ' | ( | )  | ,

  reserved               =  |  ;  |  /  |  #  | ? |  : | space

  escape                 % hex hex

  hex                    digit | a | b | c | d | e | f | A | B | C |
                         D | E | F

  national               { | } | vline | [ | ] | \ | ^ | ~

  punctuation            < | >

  digits                 digit [ digits ]

  alphanum               alpha | digit

  alphanums              alphanum [ alphanums ]

  void
==

@author Wouter Beek
@version 2013/05
*/



