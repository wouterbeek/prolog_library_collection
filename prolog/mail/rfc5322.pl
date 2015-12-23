:- module(
  rfc5322,
  [
    mailbox//1 % -Pair:pair(string)
  ]
).

/** <module> RFC 5322: Internet Message Format

@author Wouter Beek
@compat RFC 5322
@see https://tools.ietf.org/html/rfc5322
@version 2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1, % ?Code:code
     'CR'//0,
     'CRLF'//0,
     'DIGIT'//1, % ?Weight:nonneg
     'DQUOTE'//0,
     'HTAB'//0,
     'LF'//0,
     'SP'//0,
     'VCHAR'//1, % ?Code:code
     'WSP'//1 % ?Code:code
   ]).





%! 'addr-spec'(-Pair:pair(string))// is det.
% ```abnf
% addr-spec = local-part "@" domain
% ```

'addr-spec'(LocalPart-Domain) --> 'local-part'(LocalPart), "@", domain(Domain).



%! address// is det.
% ```abnf
% address = mailbox / group
% ```

address --> mailbox.
address --> group.



%! 'angle-addr'(-Address)// is det.
% ```abnf
% angle-addr = [CFWS] "<" addr-spec ">" [CFWS] / obs-angle-addr
% ```

'angle-addr'(X) --> ?('CFWS'), "<", 'addr-spec'(X), ">", ?('CFWS').
'angle-addr'(X) --> 'obs-angle-addr'(X).



%! atext(-Code:code)// .
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

atext(C)   --> 'ALPHA'(C).
atext(C)   --> 'DIGIT'(C).
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



%! atom(-Atom:string)// is det.
% ```abnf
% atom = [CFWS] 1*atext [CFWS]
% ```

atom(S) --> ?('CFWS'), +(atext, Cs), ?('CFWS'), {string_codes(S, Cs)}.



%! ccontent// .
% ```abnf
% ccontent = ctext / quoted-pair / comment
% ```

ccontent --> ctext(_).
ccontent --> 'quoted-pair'(_).
ccontent --> comment.



%! 'CFWS'// .
% ```abnf
% CFWS = (1*([FWS] comment) [FWS]) / FWS
% ```

'CFWS' --> +(?('FWS'), comment), ?('FWS').
'CFWS' --> 'FWS'.



%! comment// .
% ```abnf
% comment = "(" *([FWS] ccontent) [FWS] ")"
% ```

comment --> "(", *(?('FWS'), ccontent), ?('FWS'), ")".



%! ctext(-Code:code)// .
% ```abnf
% ctext = %d33-39     ; Printable US-ASCII
%       / %d42-91     ;  characters not including
%       / %d93-126    ;  "(", ")", or "\"
%       / obs-ctext
% ```

ctext(C) -->
  [C],
  {(between(33, 39, C) ; between(42, 91, C)! ; between(93, 126, C))}, !.
ctext(C) --> 'obs-ctext'(C).



%! date(-Year:nonneg, -Month:bewtween(0,99), -Day:between(0,99))// is det.
% ```abnf
% date = day month year
% ```

date(Y, Mo, D) --> day(D), month(Mo), year(Y).



%! 'date-time'(-Datetime:compound)// .
% ```abnf
% date-time = [ day-of-week "," ] date time [CFWS]
% ```

'date-time'(datetime(Y,Mo,D,H,Mi,S,Off)) -->
  ('day-of-week'(D) -> "," ; ""),
  date(Y, Mo, D),
  time(H, Mi, S, Off),
  ?('CFWS').



%! day(-Day:between(0,99))// is det.
% ```abnf
% day = ([FWS] 1*2DIGIT FWS) / obs-day
% ```

day(D) --> ?('FWS'), 'm*n'(1, 2, 'DIGIT', Ds), 'FWS', {pos_num(Ds, D)}.
day(D) --> 'obs-day'(D).



%! 'day-name'(-Day:between(1,7))// is det.
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



%! 'day-of-week'(-Day)// .
% ```abnf
% day-of-week = ([FWS] day-name) / obs-day-of-week
% ```

'day-of-week'(D) --> ?('FWS'), 'day-name'(D).
'day-of-week'(D) --> 'obs-day-of-week'(D).



%! 'display-name'(-Name:list(string))// is det.
% ```abnf
% display-name = phrase
% ```

'display-name'(S) --> phrase(S).



%! domain(-Domain:string)// is det.
% ```abnf
% domain = dot-atom / domain-literal / obs-domain
% ```

domain(S) --> 'dot-atom'(S), !.
domain(S) --> 'domain-literal'(S), !.
domain(S) --> 'obs-domain'(S).



%! 'domain-literal'(-String:string)// is det.
% ```abnf
% domain-literal = [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
% ```

'domain-literal'(S) --> ?('CFWS'), "[", *(spc_dtext, Cs), ?('FWS'), "]", ?('CFWS'), {string_codes(S, Cs)}.
spc_dtext(C) --> ?('FWS'), dtext(C).



%! 'dot-atom'(-String:string)// is det.
% ```abnf
% dot-atom = [CFWS] dot-atom-text [CFWS]
% ```

'dot-atom'(L) --> ?('CFWS'), 'dot-atom-text'(L), ?('CFWS').



%! 'dot-atom-text'(-String:string)// is det.
% ```abnf
% dot-atom-text = 1*atext *("." 1*atext)
% ```

'dot-atom-text'(S) --> atext(H), *(dot_or_atext, T), {string_codes(S, [H|T])}.
dot_or_atext(0'.) --> ".".
dot_or_atext(C)   --> atext(C).



%! dtext(-Code:code)// .
% ```abnf
% dtext = %d33-90     ; Printable US-ASCII
%       / %d94-126    ;  characters not including
%       / obs-dtext   ;  "[", "]", or "\"
% ```

dtext(C) --> [C], {once(between(33, 90, C) ; between(94, 126, C))}.
dtext(C) --> 'obs-dtext'(C).



%! group(-Group:list(string))// is det.
% ```abnf
% group = display-name ":" [group-list] ";" [CFWS]
% ```

group --> 'display-name'(Name), ":", ?('group-list', L), ";", ?('CFWS').



%! 'group-list'// is det.
% ```abnf
% group-list = mailbox-list / CFWS / obs-group-list
% ```

'group-list' --> 'mailbox-list'.
'group-list' --> 'CFWS'.
'group-list' --> 'obs-group-list'.



%! hour(-Hour:between(0,99))// is det.
% ```abnf
% hour = 2DIGIT / obs-hour
% ```

hour(H) --> #(2, 'DIGIT', Ds), !, {pos_sum(Ds, H)}.
hour(H) --> 'obs-hour'(H).



%! 'FWS'// .
% ```abnf
% FWS = ([*WSP CRLF] 1*WSP) / obs-FWS   ; Folding white space
% ```
                                          
'FWS' --> ?(*('WSP'), 'CRLF'), +('WSP').
'FWS' --> 'obs-FWS'.



%! 'local-part'(-String:string)// is det.
% ```abnf
% local-part = dot-atom / quoted-string / obs-local-part
% ```

'local-part'(S) --> 'dot-atom'(S).
'local-part'(S) --> 'quoted-string'(S).
'local-part'(S) --> 'obs-local-part'(S).



%! mailbox(-Pair:pair(string))// is det.
% ```abnf
% mailbox = name-addr / addr-spec
% ```

mailbox(X) --> 'name-addr'(X).
mailbox(X) --> 'addr-spec'(X).



%! 'mailbox-list'(-Mailboxes:list)// is det.
% ```abnf
% mailbox-list = (mailbox *("," mailbox)) / obs-mbox-list
% ```

'mailbox-list'([H|T]) --> mailbox(H), *(sep_mailbox, T), !.
'mailbox-list'(L) --> 'obs-mbox-list'(L).
sep_mailbox(X) --> ",", mailbox(X).



%! minute(-Minute:between(0,99))// is det.
% ```abnf
% minute = 2DIGIT / obs-minute
% ```

minute(Mi) --> #(2, 'DIGIT', Ds), !, {pos_sum(Ds, Mi)}.
minute(Mi) --> 'obs-minute'(Mi).



%! month(-Month:between(1,12))// is det.
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



%! 'name-addr'(-Pair:pair(string))// is det.
% ```abnf
% name-addr = [display-name] angle-addr
% ```

'name-addr'(Name-Addr) --> ?('display-name', Name), 'angle-addr'(Addr).



%! 'obs-day-of-week'(-Day:between(1,7))// is det.
% ```abnf
% obs-day-of-week = [CFWS] day-name [CFWS]
% ```

'obs-day-of-week'(D) --> ?('CFWS'), 'day-name'(D), ?('CFWS').



%! phrase(-Words:list(string))// .
% ```abnf
% phrase = 1*word / obs-phrase
% ```

phrase(L) --> +(word, L).
phrase(L) --> 'obs-phrase'(L).



%! qcontent(-Code:code)// .
% ```abnf
% qcontent = qtext / quoted-pair
% ```

qcontent(C) --> qtext(C).
qcontent(C) --> 'quoted-pair'(C).



%! qtext(-Code:code)// .
% ```abnf
% qtext = %d33        ; Printable US-ASCII
%       / %d35-91     ;  characters not including
%       / %d93-126    ;  "\" or the quote character
%       / obs-qtext
% ```

qtext(33) --> [33].
qtext(C)  --> [C], {once(between(35, 91, C) ; between(93, 126, C))}.
qtext(C)  --> 'obs-qtext'(C).



%! 'quoted-pair'(-Code:code)// .
% ```abnf
% quoted-pair = ("\" (VCHAR / WSP)) / obs-qp
% ```

'quoted-pair'(C) --> "\\", 'VCHAR'(C).
'quoted-pair'(C) --> "\\", 'WSP'(C).
'quoted-pair'(C) --> 'obs-qp'(C).



%! 'quoted-string'(-String:string)// .
% ```abnf
% quoted-string = [CFWS] DQUOTE *([FWS] qcontent) [FWS] DQUOTE [CFWS]
% ```

'quoted-string'(S) -->
  ?('CFWS'),
  'DQUOTE',
  *(quoted_string_code, Cs),
  ?('FWS'),
  'DQUOTE',
  ?('CFWS'),
  {string_codes(S, Cs)}.
quoted_string_code(C) --> ?('FWS'), qcontent(C).



%! second(-Second:between(0,99))// is det.
% ```abnf
% second = 2DIGIT / obs-second
% ```

second(S) --> #(2, 'DIGIT', Ds), !, {pos_sum(Ds, S)}.
second(S) --> 'obs-second'(S).



%! specials(-Code:code)// .
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
specials(0'")  --> 'DQUOTE'.   %"



%! time(
%!   -Hour:between(0,99),
%!   -Minute:between(0,99),
%!   -Second:between(0,99),
%!   -Offset:between(-9999,9999)
%! )// is det.
% ```abnf
% time = time-of-day zone
% ```

time(H, Mi, S, Off) -->
  'time-of-day'(H, Mi, S),
  zone(Off).



%! 'time-of-day'(
%!   -Hour:between(0,99),
%!   -Minute:between(0,99),
%!   -Second:between(0,99)
%! )// is det.
% ```abnf
% time-of-day = hour ":" minute [ ":" second ]
% ```

'time-of-day'(H, Mi, S) -->
  hour(H),
  ":",
  minute(Mi),
  (":" -> second(S) ; {S = 0}).



%! unstructured(-String:string)// .
% ```abnf
% unstructured = (*([FWS] VCHAR) *WSP) / obs-unstruct
% ```

unstructured(S) --> *(unstructured_code, Cs) *('WSP'), {string_codes(S, Cs)}.
unstructured(S) --> 'obs-unstruct'(S).
unstructured_code(C) --> ?('FWS'), 'VCHAR'(C).



%! word(-Word:string)// .
% ```abnf
% word = atom / quoted-string
% ```

word(S) --> atom(S).
word(S) --> 'quoted-string'(S).



%! year(-Year:nonneg)// is det.
% ```abnf
% year = (FWS 4*DIGIT FWS) / obs-year
% ```

year(Y) --> 'FWS', 'm*'(4, 'DIGIT', Ds), 'FWS', {pos_sum(Ds, Y)}.
year(Y) --> 'obs-year'(Y).



%! zone(-Offset:between(-9999,9999))// is det.
% ```abnf
% zone = (FWS ( "+" / "-" ) 4DIGIT) / obs-zone
% ```

zone(N) -->
  'FWS',
  ("+" -> {Sg = 1} ; "-" -> {Sg = -1}),
  #(4, 'DIGIT', Ds), !,
  {pos_sum(Ds, N0), N is -N0}.
zone(N) --> 'obs-zone'(N).
