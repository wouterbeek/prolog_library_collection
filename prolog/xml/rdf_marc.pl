:- module(
  rdf_marc,
  [
  ]
).

/** <module> RDF MARC

@author Wouter Beek
@version 2017/02
*/

%! marc_subfield(+Tag, +A, +B, +Code, +Val, -Pair) is det.

% 035: System Control Number (R)
%
%      Control number of a system other than the one whose control
%      number is contained in field 001 (Control Number), field 010
%      (Library of Congress Control Number) or field 016 (National
%      Bibliographic Agency Control Number).
%
% a:   System control number (NR)
%
%      MARC code (enclosed in parentheses) of the organization
%      originating the system control number, followed immediately by
%      the number.  See Appendix I: Organization Code Sources for a
%      listing of sources used in MARC 21 records.
%
% Example: (IISG)IISGb10126980
marc_subfield(field('035','','',a), Val, Key-Val) :- !,
  rdf_equal(marc:systemControlNumber, Key).
% 040: Cataloging Source (NR)
%
%      MARC code for or the name of the organization(s) that created
%      the original bibliographic record, assigned MARC content
%      designation and transcribed the record into machine-readable
%      form, or modified (except for the addition of holdings symbols)
%      an existing MARC record.  These data and the code in 008/39
%      (Cataloging source) specify the parties responsible for the
%      bibliographic record.  See: MARC Code List for Organizations
%      for a listing of sources used in MARC 21 records.
%
% a:   Original cataloging agency (NR)
%
% Example: NL-AmISG
marc_subfield('040', '', '', a, Val, Key-Val) :- !,
  rdf_equal(marc:originalCatalogingAgency, Key).
% 100: Main Entry-Personal Name (NR)
%
%      Personal name used as a main entry in a bibliographic record.
%
% 1:   Surname
%
%      Single or multiple surname formatted in inverted order or a
%      single name without forenames that is known to be a surname.
%
% a:   Personal name (NR)
%
%      Surname and/or forename; letters, initials, abbreviations,
%      phrases, or numbers used in place of a name; or a family name.
%
% Example: Hajnal, Henri.
marc_subfield('100', '1', '', a, Val, Key-Val) :- !,
  rdf_equal(marc:mainEntrySurname, Key).
% c:   Titles and words associated with a name (R)
%
% Example: of the Socialist Workers Party.
marc_subfield('100', '1', '', c, Val, Key-Val) :- !,
  rdf_equal(marc:'100-1-c', Key).
% 0:   Authority record control number or standard number (R)
%
%      Subfield $0 contains the system control number of the related
%      authority record, or a standard identifier such as an
%      International Standard Name Identifier (ISNI).  The control
%      number or identifier is preceded by the appropriate MARC
%      Organization code (for a related authority record) or the
%      Standard Identifier source code (for a standard identifier
%      scheme), enclosed in parentheses.  In the latter case, the
%      parenthetical "(uri)" is redundant and should not be included
%      if the identifier is given in the form of a Web retrieval
%      protocol, e.g., HTTP URI, which is self-identifying.
%
%      See MARC Code List for Organizations for a listing of
%      organization codes and Standard Identifier Source Codes for
%      code systems for standard identifiers.  Subfield $0 is
%      repeatable for different control numbers or identifiers.
%
%      Subfield $0 is to be used for recording URIs which represent
%      objects in RDF triple statements.
%
% Example: (NL-AMISG)136532
marc_subfield('100', '1', '', '0', Val, Key-Val) :- !,
  rdf_equal(marc:'10010', Key).
% 245: Title Statement (NR)
%
%      Title and statement of responsibility area of the bibliographic
%      description of a work
%
% 1:   Added entry
%
%      Desired title added entry is the same as the title in field
%      245.
%
% 0-9: Number of non-filling characters.
%
% a:   Title (NR)
%
% Example (0): Finsterwolde... verdedigt de democratie en het levenspeil der werkers.
% Example (3): Le droit du Danube international. Avec une préface de C. de Visscher.
% Example (4): The revolutions of Latin America.
marc_subfield('245', '1', _, a, Lex, P-O) :- !,
  rdf_equal(dc:title, P),
  nlp_guess(Lex, LTag),
  rdf_literal(O, rdf:langString, Lex, LTag).
% b:   Remainder of title (NR)
%
% Example: ein metaphysischer Versuch /
marc_subfield('245', '1', _, b, Lex, P-O) :- !,
  rdf_equal(marc:titleRemainder, P),
  nlp_guess(Lex, LTag),
  rdf_literal(O, rdf:langString, Lex, LTag).
% c:   Statement of responsibility, etc. (NR)
%
% Example: Hans Driesch.
marc_subfield('245', '1', _, c, Val, P-Val) :- !,
  rdf_equal(marc:'245-1-0-c', P).
% 250: Edition Statement (R)
%
%      Information relating to the edition of a work as determined by
%      applicable cataloging rules.
%
% a:   Edition statement (NR)
%
% Example: New impression.
marc_subfield('250', _, _, a, Val, P-Val) :- !,
  rdf_equal(marc:editionStatement, P).
% 260: Publication, Distribution, etc. (Imprint) (R)
%
%      Information relating to the publication, printing,
%      distribution, issue, release, or production of a work.
%
% a:   Place of publication, distribution, etc. (R)
%
%      May contain the abbreviation [S.l.] when the place is unknown.
%
% Example: La Haye,
marc_subfield('260', '', '', a, Val, Key-Val) :- !,
  rdf_equal(bibframe:place, Key).
% b:   Name of publisher, distributor, etc. (R)
%
%      May contain the abbreviation [s.n.] when the name is unknown.
marc_subfield('260', '', '', b, Val, Key-Val) :- !,
  rdf_equal(dc:publisher, Key).
% c:   Date of publication, distribution, etc. (R)
%
%      May contain multiple dates (e.g., dates of publication and
%      copyright).
%
% Example: 1929.
marc_subfield('260', '', '', c, Val, Key-Val) :- !,
  rdf_equal(dc:date, Key).
% 300: Physical Description (R)
%
%      Physical description of the described item, including its
%      extent, dimensions, and such other physical details as a
%      description of any accompanying materials and unit type and
%      size.
%
% a:   Extent (R)
%
%      Number of physical pages, volumes, cassettes, total playing
%      time, etc., of of each type of unit.
marc_subfield('300', '', '', a, Val, Key-Val) :- !,
  rdf_equal(marc:extent, Key).
% 500:  General Note (R)
%
%       General information for which a specialized 5XX note field has
%       not been defined.
%
% a:    General note (NR)
marc_subfield('500', '', '', a, Val, P-Val) :- !,
  rdf_equal(marc:generalNote, P).
% 600: Subject Added Entry-Personal Name (R)
%
%      Subject added entry in which the entry element is a personal
%      name.
%
% 1:   Surname
%
% 4:   Source not specified
%
% a:   Personal name (NR)
%
% Example: Humboldt, Wilhelm von.
marc_subfield('600', '1', '4', a, Val, P-Val) :- !,
  rdf_equal(marc:'600-1-4-a', P).
% c:   Titles and other words associated with a name (R)
%
% Example: 7th earl of.
marc_subfield('600', '1', '4', c, Val, P-Val) :- !,
  rdf_equal(marc:'600-1-4-c', P).
% 0:   Authority record control number or standard number (R)
%
% Example: (NL-AMISG)149188
marc_subfield('600', '1', '4', '0', Val, P-Val) :- !,
  rdf_equal(marc:'600-1-4-0', P).
% 690: Local Subject Access Fields (R)
%
%      Fields 690-699 are reserved for local subject use and local
%      definition.  For interchange purposes, documentation of the
%      structure of the 69X fields and input conventions should be
%      provided to exchange partners by the organization initiating
%      the exchange.
%
% a:   @tbd
%
% Example: O 25
marc_subfield('690', '', '', a, Val, Key-Val) :- !,
  rdf_equal(marc:'690a', Key).
% b:   @tbd
%
% Example: Austria; political history; foreign relations
marc_subfield('690', '', '', b, Val, Key-Val) :- !,
  rdf_equal(marc:'690b', Key).
% c:   @tbd
%
% Eample: Oostenrijk; politieke geschiedenis; buitenlandse politiek.
marc_subfield('690', '', '', c, Val, Key-Val) :- !,
  rdf_equal(marc:'690c', Key).
% 694: @tbd
%
% a:   @tbd
%
% Example: Harich, W.
marc_subfield('694', '', '', a, Val, P-Val) :- !,
  rdf_equal(marc:'694-a', P).
% b:   @tbd
%
% Example: A 170/11
marc_subfield('694', '', '', b, Val, P-Val) :- !,
  rdf_equal(marc:'694-b', P).
% 700: Added Entry-Personal Name (R)
%
%      Added entry in which the entry element is a personal name.
%
% 1:   Surname
%
% a:   Personal name (NR)
%
% Example: Visscher, C. de.
marc_subfield('700', '1', '', a, Val, Key-Val) :- !,
  rdf_equal(marc:addedEntrySurname, Key).
% e:   Relator term (R)
%
% Example: collector.
marc_subfield('700', '1', '', e, Val, Key-Val) :- !,
  rdf_equal(marc:relator, Key).
% 0:   Authority record control number or standard number (R)
%
% Example: (NL-AMISG)388835
marc_subfield('700', '1', '', '0', Val, Key-Val) :- !,
  rdf_equal(marc:'700-1-0', Key).
% 710: Added Entry-Corporate Name (R)
%
%      Added entry in which the entry element is a corporate name.
%
% 2:   Name in direct order
%
% a:   Corporate name or jurisdiction name as entry element (NR)
%
% Example: Communist Party of India (Marxist)
marc_subfield('710', '2', _, a, Lex, P-O) :- !,
  rdf_equal(marc:corporateName, P),
  nlp_guess(Lex, LTag),
  rdf_literal(O, rdf:langString, Lex, LTag).
% 830: Series Added Entry-Uniform Title (R)
%
%      Series added entry consisting of a series title alone.
%
% 0-9: Number of non-filing characters.
%
% a:   Uniform title (NR)
%
% Example: The great revolutions.
marc_subfield('830', '', _, a, Lex, P-O) :- !,
  rdf_equal(marc:seriesTitle, P),
  nlp_guess(Lex, LTag),
  rdf_literal(O, rdf:langString, Lex, LTag).
% v:   Volume/sequential designation (NR)
%
% Example: II, 2.
marc_subfield('830', '', _, v, Val, P-Val) :- !,
  rdf_equal(marc:volume, P).
% 0:   Authority record control number or standard number (R)
%
% Example: (NL-AMISG)125750
marc_subfield('830', '', _, '0', Val, P-Val) :- !,
  rdf_equal(marc:'8300', P).
% 852: Location (R)
%
%      Identifies the organization holding the item or from which it
%      is available.  May also contain detailed information about how
%      to locate the item in a collection.
%
% 4:   Shelving control number
%
% a:   Location (NR)
%
%      Institution or person holding the item or from which access is
%      given.  Contains a MARC code of the holding institution or the
%      name of the institution or person.
%
% Example: IISG
marc_subfield('852', '4', '', a, Val, Key-Val) :- !,
  rdf_equal(marc:shelvingLocation, Key).
% b:   Sublocation or collection (R)
%
%      Specific department, library, etc., within the holding
%      organization in which the item is located or from which it is
%      available.
%
% Example: IISG
marc_subfield('852', '4', '', b, Val, Key-Val) :- !,
  rdf_equal(marc:shelvingSublocation, Key).
% c:   Shelving location (R)
%
% Example: IISG
marc_subfield('852', '4', '', c, Val, Key-Val) :- !,
  rdf_equal(marc:shelvingLocation2, Key).
% j:   Shelving control number (NR)
%
% Example: O 25/17
marc_subfield('852', '4', '', j, Val, Key-Val) :- !,
  rdf_equal(marc:shelvingControlNumber, Key).
% n:   Country code (NR)
%
%      Two- or three-character MARC code for the principal location
%      contained in subfield $a (Location).  Code from: MARC Code List
%      for Countries.
%
% Example: Available
marc_subfield('852', '4', '', n, Val, Key-Val) :- !,
  rdf_equal(marc:shelvingCountry, Key).
% p:   Piece designation (NR)
%
%      Identification of a single piece when the holdings information
%      does not contain an 863-865 (Enumeration and Chronology) or
%      876-878 (Item Information) field that contains a subfield $p
%      (Piece designation).
%
% Example: N10124962
marc_subfield('852', '4', '', p, Val, Key-Val) :- !,
  rdf_equal(marc:pieceDesignation, Key).
% 901: Local Data Element
%
% a:   ???
%
% Example: 8
marc_subfield('901', '', '', a, Val, Key-Val) :- !,
  rdf_equal(marc:'901a', Key).
% b:   ???
%
% Example: Unknown
marc_subfield('901', '', '', b, Val, Key-Val) :- !,
  rdf_equal(marc:'901b', Key).
% c:   ???
%
% Example: 8
marc_subfield('901', '', '', c, Val, Key-Val) :- !,
  rdf_equal(marc:'901c', Key).
% t:   ???
%
% Example: biblio
marc_subfield('901', '', '', t, Val, Key-Val) :- !,
  rdf_equal(marc:'901t', Key).
% 902: Local Data Element
%
% a:   ???
%
% Example: 10622/8ADEFA52-2EE7-48C3-9C1A-ADF75CF9E87A
marc_subfield('902', '', '', a, Val, Key-Val) :- !,
  rdf_equal(marc:'902a', Key).
% DEB
marc_subfield(Tag, A, B, Code, Val, Key-Val) :-
  format(user_output, "~a-~a-~a-~a: ~a~n", [Tag,A,B,Code,Val]),
  gtrace,
  marc_subfield(Tag, A, B, Code, Val, Key-Val).
