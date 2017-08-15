:- module(
  rfc5322,
  [
    mailbox//1 % -Mailbox
  ]
).

/** <module> RFC 5322: Internet Message Format

@author Wouter Beek
@compat RFC 5322
@see https://tools.ietf.org/html/rfc5322
@version 2017/05-2017/08
*/

:- use_module(library(clpfd)).
:- use_module(library(dcg/dcg_ext), except([
     atom//1
   ])).
:- use_module(library(dcg/rfc5234), [
     'ALPHA'//1,  % ?Code
     'CR'//0,
     'CR'//1,     % ?Code
     'CRLF'//0,
     'CRLF'//1,   % ?Code
     'DIGIT'//1,  % ?Weight
     'DQUOTE'//0,
     'HTAB'//0,
     'LF'//0,
     'LF'//1,     % ?Code
     'SP'//0,
     'VCHAR'//1,  % ?Code
     'WSP'//0,
     'WSP'//1     % ?Code
   ]).
:- use_module(library(lists)).
:- use_module(library(math_ext)).





%! 'addr-spec'(-LocalPart:atom, -Domain:atom)// .
%
% ```abnf
% addr-spec = local-part "@" domain
% ```

'addr-spec'(LocalPart, Domain) -->
  'local-part'(LocalPart),
  "@",
  domain(Domain).



%! address(-Address:pair(list(atom),list(compound)))// .
%
% ```abnf
% address = mailbox / group
% ```

address([]-Mailboxes) -->
  mailbox(Mailboxes).
address(Name-Mailboxes) -->
  group(Name, Mailboxes).



%! 'angle-addr'(-Route:list(atom), -LocalPart:atom, -Domain:atom)// .
%
% ```abnf
% angle-addr = [CFWS] "<" addr-spec ">" [CFWS] / obs-angle-addr
% ```

'angle-addr'([], LocalPart, Domain) -->
  ?('CFWS'),
  "<",
  'addr-spec'(LocalPart, Domain),
  ">",
  ?('CFWS').
'angle-addr'(Route, LocalPart, Domain) -->
  'obs-angle-addr'(Route, LocalPart, Domain).



%! atext(?Code:code)// .
%
% ```abnf
% atext = ALPHA / DIGIT   ; Printable US-ASCII
%       / "!" / "#"       ;  characters not including
%       / "$" / "%"       ;  specials.  Used for atoms.
%       / "&" / "'"
%       / "*" / "+"
%       / "-" / "/"
%       / "=" / "?"
%       / "^" / "_"
%       / "`" / "{"
%       / "|" / "}"
%       / "~"
% ```

atext(Code)   --> 'ALPHA'(Code).
atext(Code)   --> 'DIGIT'(Code).
atext(0'!) --> "!".
atext(0'#) --> "#".
atext(0'$) --> "$".
atext(0'%) --> "%".
atext(0'&) --> "&".
atext(0'') --> "'".
atext(0'*) --> "*".
atext(0'+) --> "+".
atext(0'-) --> "-".
atext(0'/) --> "/".
atext(0'=) --> "=".
atext(0'?) --> "?".
atext(0'^) --> "^".
atext(0'_) --> "_".
atext(0'`) --> "`".
atext(0'{) --> "{".
atext(0'|) --> "|".
atext(0'}) --> "}".
atext(0'~) --> "~".



%! atom(?Atom:atom)// .
%
% ```abnf
% atom = [CFWS] 1*atext [CFWS]
% ```

atom(Atom) -->
  ?('CFWS'),
  dcg_atom(+(atext), Atom),
  ?('CFWS').



%! ccontent// .
%
% ```abnf
% ccontent = ctext / quoted-pair / comment
% ```

ccontent --> ctext(_).
ccontent --> 'quoted-pair'(_).
ccontent --> comment.



%! 'CFWS'// .
%
% ```abnf
% CFWS = (1*([FWS] comment) [FWS]) / FWS
% ```

'CFWS' -->
  +(sep_comment_),
  ?('FWS').
'CFWS' -->
  'FWS'.

sep_comment_ -->
  ?('FWS'),
  comment.



%! comment// .
%
% ```abnf
% comment = "(" *([FWS] ccontent) [FWS] ")"
% ```

comment -->
  "(",
  *(sep_ccontent_),
  ?('FWS'),
  ")".

sep_ccontent_ -->
  ?('FWS'),
  ccontent.



%! ctext(?Code:code)// .
%
% ```abnf
% ctext = %d33-39     ; Printable US-ASCII
%       / %d42-91     ;  characters not including
%       / %d93-126    ;  "(", ")", or "\"
%       / obs-ctext
% ```

ctext(Code) --> between(33, 39, Code).
ctext(Code) --> between(42, 91, Code).
ctext(Code) --> between(93, 126, Code).
ctext(Code) --> 'obs-ctext'(Code).



%! date(?Year:nonneg, ?Month:between(0,99), ?Day:between(0,99))// .
%
% ```abnf
% date = day month year
% ```

date(Year, Month, Day) -->
  day(Day),
  month(Month),
  year(Year).



%! 'date-time'(?Datetime:dt)// .
%
% ```abnf
% date-time = [ day-of-week "," ] date time [CFWS]
% ```

'date-time'(dt(Year,Month,Day,Hour,Minute,Second,Offset)) -->
  ('day-of-week'(Day) -> "," ; ""),
  date(Year, Month, Day),
  time(Hour, Minute, Second, Offset),
  ?('CFWS').



%! day(?Day:between(0,99))// .
%
% ```abnf
% day = ([FWS] 1*2DIGIT FWS) / obs-day
% ```

day(Day) -->
  ?('FWS'),
  dcg_integer('m*n'(1, 2, 'DIGIT'), Day),
  'FWS'.
day(Day) -->
  'obs-day'(Day).



%! 'day-name'(?Day:between(1,7))// .
%
% ```abnf
% day-name = "Mon" / "Tue" / "Wed" / "Thu" / "Fri" / "Sat" / "Sun"
% ```

'day-name'(1) --> "Mon".
'day-name'(2) --> "Tue".
'day-name'(3) --> "Wed".
'day-name'(4) --> "Thu".
'day-name'(5) --> "Fri".
'day-name'(6) --> "Sat".
'day-name'(7) --> "Sun".



%! 'day-of-week'(?Day:between(1,7))// .
%
% ```abnf
% day-of-week = ([FWS] day-name) / obs-day-of-week
% ```

'day-of-week'(Day) -->
  ?('FWS'),
  'day-name'(Day).
'day-of-week'(Day) -->
  'obs-day-of-week'(Day).



%! 'display-name'(?Name:list(atom))// .
%
% ```abnf
% display-name = phrase
% ```

'display-name'(Name) -->
  phrase0(Name).



%! domain(?Domain:atom)// .
%
% ```abnf
% domain = dot-atom / domain-literal / obs-domain
% ```

domain(Domain) --> 'dot-atom'(Domain).
domain(Domain) --> 'domain-literal'(Domain).
domain(Domain) --> 'obs-domain'(Domain).



%! 'domain-literal'(?DomainLiteral:atom)// .
%
% ```abnf
% domain-literal = [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
% ```

'domain-literal'(DomainLiteral) -->
  ?('CFWS'),
  "[",
  dcg_atom(*('domain-literal_'), DomainLiteral),
  ?('FWS'),
  "]",
  ?('CFWS').

'domain-literal_'(Code) -->
  ?('FWS'),
  dtext(Code).



%! 'dot-atom'(?Atom:atom)// .
%
% ```abnf
% dot-atom = [CFWS] dot-atom-text [CFWS]
% ```

'dot-atom'(Atom) -->
  ?('CFWS'),
  'dot-atom-text'(Atom),
  ?('CFWS').



%! 'dot-atom-text'(?Atom:atom)// .
%
% ```abnf
% dot-atom-text = 1*atext *("." 1*atext)
% ```

'dot-atom-text'(Atom) -->
  atext(H),
  *('dot-atom-text_', T),
  {atom_codes(Atom, [H|T])}.

'dot-atom-text_'(0'.) --> ".".
'dot-atom-text_'(Code) --> atext(Code).



%! dtext(?Code:code)// .
%
% ```abnf
% dtext = %d33-90     ; Printable US-ASCII
%       / %d94-126    ;  characters not including
%       / obs-dtext   ;  "[", "]", or "\"
% ```

dtext(Code) --> between(33, 90, Code).
dtext(Code) --> between(94, 126, Code).
dtext(Code) --> 'obs-dtext'(Code).



%! 'FWS'// .
%
% Folding white space.
%
% ```abnf
% FWS = ([*WSP CRLF] 1*WSP) / obs-FWS   ; Folding white space
% ```

'FWS' -->
  ?(fws_prefix),
  +('WSP').
'FWS' -->
  'obs-FWS'.

fws_prefix -->
  *('WSP'),
  'CRLF'.



%! group(-Name:list(atom), -Mailboxes:list(compound))// .
%
% ```abnf
% group = display-name ":" [group-list] ";" [CFWS]
% ```

group(Name, Mailboxes) -->
  'display-name'(Name),
  ":",
  ('group-list'(Mailboxes) -> "" ; {Mailboxes = []}),
  ";",
  ?('CFWS').



%! 'group-list'(-Mailboxes:list(compound))// .
%
% ```abnf
% group-list = mailbox-list / CFWS / obs-group-list
% ```

'group-list'(Mailboxes) --> 'mailbox-list'(Mailboxes).
'group-list'([]) --> 'CFWS'.
'group-list'([]) --> 'obs-group-list'.



%! hour(?Hour:between(0,99))// .
%
% ```abnf
% hour = 2DIGIT / obs-hour
% ```

hour(Hour) -->
  dcg_integer(#(2, 'DIGIT'), Hour).
hour(Hour) -->
  'obs-hour'(Hour).



%! 'local-part'(-LocalPart:atom)// .
%
% ```abnf
% local-part = dot-atom / quoted-atom / obs-local-part
% ```

'local-part'(LocalPart) -->
  'dot-atom'(LocalPart).
'local-part'(LocalPart) -->
  'quoted-atom'(LocalPart).
'local-part'(LocalPart) -->
  'obs-local-part'(LocalPart).



%! mailbox(-Mailbox:compound)// .
%
% ```abnf
% mailbox = name-addr / addr-spec
% ```

mailbox(mailbox(Name,Route,LocalPart,Domain)) -->
  'name-addr'(Name, Route, LocalPart, Domain).
mailbox(mailbox([],[],LocalPart,Domain)) -->
  'addr-spec'(LocalPart, Domain).



%! 'mailbox-list'(-Mailboxes:list(compound))// .
%
% ```abnf
% mailbox-list = (mailbox *("," mailbox)) / obs-mbox-list
% ```

'mailbox-list'([H|T]) -->
  mailbox(H),
  *(sep_mailbox, T), !.
'mailbox-list'(L) -->
  'obs-mbox-list'(L).

sep_mailbox(Mailbox) -->
  ",",
  mailbox(Mailbox).



%! minute(-Minute:between(0,99))// .
%
% ```abnf
% minute = 2DIGIT / obs-minute
% ```

minute(Minute) -->
  dcg_integer(#(2, 'DIGIT'), Minute).
minute(Minute) -->
  'obs-minute'(Minute).



%! month(-Month:between(1,12))// .
%
% ```abnf
% month = "Jan" / "Feb" / "Mar" / "Apr"
%       / "May" / "Jun" / "Jul" / "Aug"
%       / "Sep" / "Oct" / "Nov" / "Dec"
% ```

month(1)  --> "Jan".
month(2)  --> "Feb".
month(3)  --> "Mar".
month(4)  --> "Apr".
month(5)  --> "May".
month(6)  --> "Jun".
month(7)  --> "Jul".
month(8)  --> "Aug".
month(9)  --> "Sep".
month(10) --> "Oct".
month(11) --> "Nov".
month(12) --> "Dec".



%! 'name-addr'(-Name:list(atom), -Route:list(atom), -LocalPart:atom,
%!             -Domain:atom)// .
%
% ```abnf
% name-addr = [display-name] angle-addr
% ```

'name-addr'(Name, Route, LocalPart, Domain) -->
  ('display-name'(Name) -> "" ; {Name = []}),
  'angle-addr'(Route, LocalPart, Domain).



%! 'obs-addr-list'(-Addresses:list(pair(list(atom),list(compound))))// .
%
% ```abnf
% obs-addr-list = *([CFWS] ",") address *("," [address / CFWS])
% ```

'obs-addr-list'([H|T]) -->
  *(obs_list_prefix_),
  address(H),
  obs_addr_list_tail0(T).

obs_addr_list_tail0(L) -->
  ",", !,
  (address(H) -> {L = [H|T]} ; ?('CFWS'), {L = T}),
  obs_addr_list_tail0(T).
obs_addr_list_tail0([]) --> "".



%! 'obs-angle-addr'(-Route:list(atom), -LocalPart:atom, -Domain:list(atom))// .
%
% ```abnf
% obs-angle-addr = [CFWS] "<" obs-route addr-spec ">" [CFWS]
% ```

'obs-angle-addr'(Route, LocalPart, Domain) -->
  ?('CFWS'),
  "<",
  'obs-route'(Route),
  'addr-spec'(LocalPart, Domain),
  ">",
  ?('CFWS').



%! 'obs-body'(-Body:list(code))// .
%
% ```abnf
% obs-body = *((*LF *CR *((%d0 / text) *LF *CR)) / CRLF)
% ```

'obs-body'([H1,H2|T]) -->
  'CRLF'([H1,H2]), !,
  'obs-body'(T).
'obs-body'(L) -->
  *('LF', L1),
  *('CR', L2),
  obs_body_codes0(L3),
  {append([L1,L2,L3], L0), L0 \== []}, !,
  'obs-body'(L6),
  {append(L3, L6, L)}.
'obs-body'([]) --> "".

obs_body_codes0([0|T]) -->
  [0], !,
  *('LF'),
  *('CR'),
  obs_body_codes0(T).
obs_body_codes0([H|T]) -->
  text(H), !,
  *('LF'),
  *('CR'),
  obs_body_codes0(T).
obs_body_codes0([]) --> "".



%! 'obs-ctext'(?Code:code)// .
%
% ```abnf
% obs-ctext = obs-NO-WS-CTL
% ```

'obs-ctext'(Code) -->
  'obs-NO-WS-CTL'(Code).



%! 'obs-day'(-Day:bewtween(0,99))// .
%
% ```abnf
% obs-day = [CFWS] 1*2DIGIT [CFWS]
% ```

'obs-day'(Day) -->
  ?('CFWS'),
  dcg_integer('m*n'(1, 2, 'DIGIT'), Day).



%! 'obs-day-of-week'(-Day:between(1,7))// .
%
% ```abnf
% obs-day-of-week = [CFWS] day-name [CFWS]
% ```

'obs-day-of-week'(D) -->
  ?('CFWS'),
  'day-name'(D),
  ?('CFWS').



%! 'obs-domain'(-Domain:list(atom))// .
%
% ```abnf
% obs-domain = atom *("." atom)
% ```

'obs-domain'([H|T]) -->
  atom(H),
  *(sep_atom0, T).

sep_atom0(X) -->
  ",",
  atom(X).



%! 'obs-domain-list'(-Domains:list(atom))// .
%
% ```abnf
% obs-domain-list = *(CFWS / ",") "@" domain *("," [CFWS] ["@" domain])
% ```

'obs-domain-list'([H|T]) -->
  *(obs_domain_list_prefix_),
  "@",
  domain(H),
  obs_domain_list_tail0(T).

obs_domain_list_prefix_ --> 'CFWS'.
obs_domain_list_prefix_ --> ",".

obs_domain_list_tail0(L) -->
  ",",
  ?('CFWS'),
  ("@" -> domain(H), {L = [H|T]} ; {L = T}),
  obs_domain_list_tail0(T).
obs_domain_list_tail0([]) --> "".



%! 'obs-dtext'(?Code:code)// .
%
% ```abnf
% obs-dtext = obs-NO-WS-CTL / quoted-pair
% ```

'obs-dtext'(C) --> 'obs-NO-WS-CTL'(C).
'obs-dtext'(C) --> 'quoted-pair'(C).



%! 'obs-FWS'// .
%
% ```abnf
% obs-FWS = 1*WSP *(CRLF 1*WSP)
% ```

'obs-FWS' -->
  +('WSP'),
  *(obs_fws_part).

obs_fws_part -->
  'CRLF',
  +('WSP').



%! 'obs-group-list'// .
%
% ```abnf
% obs-group-list = 1*([CFWS] ",") [CFWS]
% ```

'obs-group-list' -->
  +(obs_list_prefix_),
  ?('CFWS').



%! 'obs-hour'(?Hour:between(0,99))// .
%
% ```abnf
% obs-hour = [CFWS] 2*DIGIT [CFWS]
% ```

'obs-hour'(Hour) -->
  ?('CFWS'),
  dcg_integer('m*n'(1, 2, 'DIGIT'), Hour).



%! 'obs-local-part'(-Words:list(atom))// .
%
% ```abnf
% obs-local-part = word *("." word)
% ```

'obs-local-part'([H|T]) -->
  word(H),
  *(sep_word0, T).

sep_word0(X) -->
  ".",
  word(X).



%! 'obs-mbox-list'(-Mailboxes:list(compound))// .
%
% ```abnf
% obs-mbox-list = *([CFWS] ",") mailbox *("," [mailbox / CFWS])
% ```

'obs-mbox-list'([H|T]) -->
  *(obs_list_prefix_),
  mailbox(H),
  obs_mbox_list_tail0(T).

obs_mbox_list_tail0(L) -->
  ",", !,
  (mailbox(H) -> {L = [H|T]} ; ?('CFWS'), {L = T}),
  obs_mbox_list_tail0(T).
obs_mbox_list_tail0([]) --> "".



%! 'obs-minute'(?Minute:between(0,99))// .
%
% ```abnf
% obs-minute = [CFWS] 2*DIGIT [CFWS]
% ```

'obs-minute'(Minute) -->
  ?('CFWS'),
  dcg_integer('m*n'(1, 2, 'DIGIT'), Minute).



%! 'obs-NO-WS-CTL'(?Code:code)// .
%
% ```abnf
% obs-NO-WS-CTL = %d1-8     ; US-ASCII control
%               / %d11      ; characters that do not
%               / %d12      ; include the carriage
%               / %d14-31   ; return, line feed, and
%               / %d127     ; white space characters
% ```

'obs-NO-WS-CTL'(C) -->
  [C],
  {once((
    between(1, 8, C)
  ; between(11, 12, C)
  ; between(14, 31, C)
  ; C =:= 127
  ))}.



%! 'obs-phrase'(-Words:list(atom))// .
%
% ```abnf
% obs-phrase = word *(word / "." / CFWS)
% ```

'obs-phrase'([H|T]) -->
  word(H),
  obs_phrase_tail0(T).

obs_phrase_tail0([H|T]) --> word(H), !, obs_phrase_tail0(T).
obs_phrase_tail0(L)     --> ".", !, obs_phrase_tail0(L).
obs_phrase_tail0(L)     --> 'CFWS', !, obs_phrase_tail0(L).
obs_phrase_tail0([])    --> "".



%! 'obs-phrase-list'// .
%
% ```abnf
% obs-phrase-list = [phrase / CFWS] *("," [phrase / CFWS])
% ```
%
% @tbd

'obs-phrase-list' -->
  phrase_cfws_empty0,
  *(sep_phrase_cfws_empty0).

sep_phrase_cfws_empty0 -->
  ",",
  phrase_cfws_empty0.

phrase_cfws_empty0 --> phrase0(_), !.
phrase_cfws_empty0 --> 'CFWS', !.
phrase_cfws_empty0 --> "".



%! 'obs-qp'(?Code:code)// .
%
% ```abnf
% obs-qp = "\" (%d0 / obs-NO-WS-CTL / LF / CR)
% ```

'obs-qp'(C) -->
  "\\",
  obs_qp_code0(C).

obs_qp_code0(0) --> [0].
obs_qp_code0(C) --> 'obs-NO-WS-CTL'(C).
obs_qp_code0(C) --> 'LF'(C).
obs_qp_code0(C) --> 'CR'(C).



%! 'obs-qtext'(?Code:code)// .
%
% ```abnf
% obs-qtext = obs-NO-WS-CTL
% ```

'obs-qtext'(C) -->
  'obs-NO-WS-CTL'(C).



%! 'obs-route'(-Route:list(atom))// .
%
% ```abnf
% obs-route = obs-domain-list ":"
% ```

'obs-route'(Route) -->
  'obs-domain-list'(Route),
  ":".



%! 'obs-second'(?Second:between(0,99))// .
%
% ```abnf
% obs-second = [CFWS] 2*DIGIT [CFWS]
% ```

'obs-second'(Second) -->
  ?('CFWS'),
  dcg_integer('m*n'(1, 2, 'DIGIT'), Second).



%! 'obs-utext'(?Code:code)// .
%
% ```abnf
% obs-utext = %d0 / obs-NO-WS-CTL / VCHAR
% ```

'obs-utext'(0) --> [0].
'obs-utext'(Code) --> 'obs-NO-WS-CTL'(Code).
'obs-utext'(Code) --> 'VCHAR'(Code).



%! 'obs-unstruct'(-Codes:list(code))// .
%
% ```abnf
% obs-unstruct = *((*LF *CR *(obs-utext *LF *CR)) / FWS)
% ```

'obs-unstruct'(Cs) -->
  'FWS', !,
  'obs-unstruct'(Cs).
'obs-unstruct'(Cs) -->
  *('LF', Cs1),
  *('CR', Cs2),
  obs_unstruct_codes0(Cs3),
  {append([Cs1,Cs2,Cs3], Cs0), Cs0 \== []}, !,
  'obs-unstruct'(Cs4),
  {append(Cs3, Cs4, Cs)}.
'obs-unstruct'([]) --> "".

obs_unstruct_codes0([H|T]) -->
  'obs-utext'(H), !,
  *('LF'),
  *('CR'),
  obs_unstruct_codes0(T).
obs_unstruct_codes0([]) --> "".



%! 'obs-year'(?Year:between(0,99))// .
%
% ```abnf
% obs-year = [CFWS] 2*DIGIT [CFWS]
% ```

'obs-year'(Year) -->
  ?('CFWS'),
  dcg_integer('m*n'(1, 2, 'DIGIT'), Year).



%! 'obs-zone'(?Timezone:atom)// .
%
% ```abnf
% obs-zone = "UT"            ; Universal Time
%          / "GMT"           ; North American UT
%          / "EST" / "EDT"   ; Eastern:  - 5/ - 4
%          / "CST" / "CDT"   ; Central:  - 6/ - 5
%          / "MST" / "MDT"   ; Mountain: - 7/ - 6
%          / "PST" / "PDT"   ; Pacific:  - 8/ - 7
%          / %d65-73         ; Military zones - "A"
%          / %d75-90         ; through "I" and "K"
%          / %d97-105        ; through "Z", both
%          / %d107-122       ; upper and lower case
% ```

'obs-zone'('Universal Time') --> "UT".
'obs-zone'('North American UT') --> "GMT".
'obs-zone'('Eastern') --> ("EST" ; "EDT").
'obs-zone'('Central') --> ("CST" ; "CDT").
'obs-zone'('Mountain') --> ("MST" ; "MDT").
'obs-zone'('Pacific') --> ("PST" ; "PDT").
'obs-zone'('Military') --> between(65, 73).
'obs-zone'('Military') --> between(75, 90).
'obs-zone'('Military') --> between(97, 105).
'obs-zone'('Military') --> between(107, 122).



%! phrase0(?Words:list(atom))// .
%
% ```abnf
% phrase = 1*word / obs-phrase
% ```

phrase0(Words) --> +(word, Words).
phrase0(Words) --> 'obs-phrase'(Words).



%! qcontent(?Code:code)// .
%
% ```abnf
% qcontent = qtext / quoted-pair
% ```

qcontent(Code) --> qtext(Code).
qcontent(Code) --> 'quoted-pair'(Code).



%! qtext(?Code:code)// .
%
% ```abnf
% qtext = %d33        ; Printable US-ASCII
%       / %d35-91     ;  characters not including
%       / %d93-126    ;  "\" or the quote character
%       / obs-qtext
% ```

qtext(33) --> [33].
qtext(Code) --> between(35, 91, Code).
qtext(Code) --> between(93, 126, Code).
qtext(Code) --> 'obs-qtext'(Code).



%! 'quoted-pair'(?Code:code)// .
%
% ```abnf
% quoted-pair = ("\" (VCHAR / WSP)) / obs-qp
% ```

'quoted-pair'(Code) -->
  "\\",
  'VCHAR'(Code).
'quoted-pair'(Code) -->
  "\\",
  'WSP'(Code).
'quoted-pair'(Code) -->
  'obs-qp'(Code).



%! 'quoted-atom'(?Atom:atom)// .
%
% ```abnf
% quoted-atom = [CFWS] DQUOTE *([FWS] qcontent) [FWS] DQUOTE [CFWS]
% ```

'quoted-atom'(Atom) -->
  ?('CFWS'),
  'DQUOTE',
  dcg_atom(*('quoted-atom_'), Atom),
  ?('FWS'),
  'DQUOTE',
  ?('CFWS').

'quoted-atom_'(Code) -->
  ?('FWS'),
  qcontent(Code).



%! second(?Second:between(0,99))// .
%
% ```abnf
% second = 2DIGIT / obs-second
% ```

second(Second) --> dcg_integer(#(2, 'DIGIT'), Second).
second(Second) --> 'obs-second'(Second).



%! specials(?Code:code)// .
%
% ```abnf
% specials = "(" / ")"   ; Special characters that do
%          / "<" / ">"   ;  not appear in atext
%          / "[" / "]"
%          / ":" / ";"
%          / "@" / "\"
%          / "," / "."
%          / DQUOTE
% ```

specials(0'()  --> "(".
specials(0'))  --> ")".
specials(0'<)  --> "<".
specials(0'>)  --> ">".
specials(0'[)  --> "[".
specials(0'])  --> "]".
specials(0':)  --> ":".
specials(0';)  --> ";".
specials(0'@)  --> "@".
specials(0'\\) --> "\\".
specials(0',)  --> ",".
specials(0'.)  --> ".".
specials(0'")  --> 'DQUOTE'.



%! text(?Code:code)// .
%
% ```abnf
% text = %d1-9      ; Characters excluding CR
%      / %d11       ;  and LF
%      / %d12
%      / %d14-127
% ```

text(Code) --> between(1, 9, Code).
text(Code) --> between(11, 12, Code).
text(Code) --> between(14, 127, Code).



%! time(?Hour:between(0,99), ?Minute:between(0,99), ?Second:between(0,99),
%!      ?Offset:between(-9999,9999))// .
%
% ```abnf
% time = time-of-day zone
% ```

time(Hour, Minute, Second, Offset) -->
  'time-of-day'(Hour, Minute, Second),
  zone(Offset).



%! 'time-of-day'(?Hour:between(0,99), ?Minute:between(0,99),
%!               ?Second:between(0,99))// .
%
% ```abnf
% time-of-day = hour ":" minute [ ":" second ]
% ```

'time-of-day'(Hour, Minute, Second) -->
  hour(Hour),
  ":",
  minute(Minute),
  (":", second(Second) ; {Second = 0}).



%! unstructured(?Unstructured:atom)// .
%
% ```abnf
% unstructured = (*([FWS] VCHAR) *WSP) / obs-unstruct
% ```

unstructured(Atom) -->
  dcg_atom(*(unstructured_), Atom),
  *('WSP').
unstructured(Atom) -->
  'obs-unstruct'(Atom).

unstructured_(Code) -->
  ?('FWS'),
  'VCHAR'(Code).



%! word(?Word:atom)// .
%
% ```abnf
% word = atom / quoted-atom
% ```

word(Word) -->
  atom(Word).
word(Word) -->
  'quoted-atom'(Word).



%! year(?Year:nonneg)// .
%
% ```abnf
% year = (FWS 4*DIGIT FWS) / obs-year
% ```

year(Year) -->
  'FWS',
  dcg_integer('m*'(4, 'DIGIT'), Year),
  'FWS'.
year(Year) -->
  'obs-year'(Year).



%! zone(?Offset:between(-9999,9999))// .
%
% ```abnf
% zone = (FWS ( "+" / "-" ) 4DIGIT) / obs-zone
% ```

zone(N2) -->
  'FWS',
  ("+" -> {Sg = 1} ; "-" -> {Sg = -1}),
  dcg_integer(#(4, 'DIGIT'), N1),
  {N2 #= Sg * N1}.
zone(N) -->
  'obs-zone'(N).





% HELPERS %

obs_list_prefix_ -->
  ?('CFWS'),
  ",".
