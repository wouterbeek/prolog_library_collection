:- module(
  news,
  [
    assert_post/1, % +Post:dict
    group/1,       % -Group:uri
    group_post/2,  % ?Group:uri, -Post:uri
    group_post/3,  % ?Group:uri, ?Post0:uri, -Post:dict
    scrape/0,
    scrape/1       % +Group:uri
  ]
).

/** <module> W3C mailinglist scraper

http://lists.w3.org/Archives/Public/html-tidy/2016JulSep/0000.html

@author Wouter Beek
@version 2016/12
*/

:- use_module(library(date)).
:- use_module(library(date_time/date_time)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/http_download)).
:- use_module(library(print_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(xpath)).




% API %

%! group(-Group:uri) is nondet.
%
% Enumerates the homepages of W3C mailinglists.

group(AbsUri) :-
  uri_components(
    BaseUri,
    uri_components(http,'lists.w3.org','/Archives/Public/',_,_)
  ),
  html_download(BaseUri, Dom),
  xpath(Dom, //dt/a(1,@href), RelUri),
  uri_resolve(RelUri, BaseUri, AbsUri).



%! group_post(+Group:uri, -Post:uri) is nondet.
%! group_post(-Group:uri, -Post:uri) is nondet.
%
% Enumerates the posts of mailinglist Group.

group_post(Group, PostUri) :-
  (var(Group) -> group(Group) ; true),
  group_timeframe(Group, Timeframe),
  timeframe_post(Timeframe, PostUri).



%! group_post(+Group:uri, +Post0:uri, -Post:dict) is nondet.
%! group_post(+Group:uri, -Post0:uri, -Post:dict) is nondet.
%! group_post(-Group:uri, -Post0:uri, -Post:dict) is nondet.

group_post(Group, Post, Dict) :-
  (var(Post) -> group_post(Group, Post) ; true),
  post_dict(Group, Post, Dict).



%! scrape is det.
%! scrape(+Group:uri) is det.

scrape :-
  thread_create(scrape0, _, [detached(true)]).

scrape0 :-
  forall(
    group(Group),
    scrape(Group)
  ).


scrape(Group) :-
  %thread_create(scrape0(Group), _, [detached(true)]).

%scrape0(Group) :-
  forall(
    group_post(Group, Post),
    (
      post_dict(Group, Post, Dict),
      print_dict(Dict),
      assert_post(Dict),
      sleep(1)
    )
  ),
  format(user_output, "DONE!~n", []).





% IMPLEMENTATION %

%! group_timeframe(+Group:uri, -Timeframe:uri) is nondet.

group_timeframe(BaseUri, AbsUri) :-
  html_download(BaseUri, Dom),
  xpath(Dom, //tr/td(1)/a(@href), RelUri),
  uri_resolve(RelUri, BaseUri, AbsUri).



%! post_dict(+Group:uri, +Post:uri, -Post:dict) is det.
%
% Prints a warning whenever something goes wrong.

post_dict(Group, Post, Dict) :-
  html_download(Post, Dom),
  (   post_dom_dict(Group, Post, Dom, Dict)
  ->  true
  ;   msg_warning("Did not parse post ~a~n", [Post])
  ).



%! post_dom_dict(+Group:uri, +Post:uri, +Post:dom, -Post:dict) is det.

post_dom_dict(Group, Post, Dom, Dict5) :-
  % body
  xpath_chk(Dom, //pre(@id=body,content), Body),
  % from
  xpath_chk(Dom, //span(@id=from,content), FromNameDom),
  FromNameDom = [_,FromName0|_],
  atom_concat(FromName, ' <', FromName0),
  xpath_chk(Dom, //span(@id=from)/a(normalize_space), FromEmail),
  % from_date
  xpath_chk(Dom, //span(@id=date,content), FromDateDom),
  FromDateDom = [_,FromDate0|_],
  once(atom_phrase(nonstandard_date(FromDate), FromDate0)),
  % id
  xpath_chk(Dom, //span(@id='message-id',content), IdDom),
  IdDom = [_,Id|_],
  % subject
  xpath_chk(Dom, //h1(normalize_space), Subject),
  % to_date
  xpath_chk(Dom, //span(@id=received,content), ToDateDom),
  ToDateDom = [_,ToDate0|_],
  once(atom_phrase(nonstandard_date(ToDate), ToDate0)),
  Dict1 = _{
    '@id': Post,
    body: Body,
    from: _{email: FromEmail, name: FromName},
    from_date: FromDate,
    group: Group,
    id: Id,
    subject: Subject,
    to_date: ToDate
  },
  % cc
  (   xpath_chk(Dom, //span(@id=cc)/a(normalize_space), CcEmailAuth)
  ->  atomic_list_concat([mailto,CcEmailAuth], :, CcEmail),
      Dict2 = Dict1.put(_{cc: _{email: CcEmail}})
  ;   Dict2 = Dict1
  ),
  % in_reply_to
  (   xpath_chk(Dom, //map(2)//li(2)/a(2,@href), RelInReplyTo)
  ->  uri_resolve(RelInReplyTo, Post, InReplyTo),
      Dict3 = Dict2.put(_{in_reply_to: InReplyTo})
  ;   Dict3 = Dict2
  ),
  % previous
  (   xpath_chk(Dom, //map(2)//li(2)/a(1,@href), RelPrev)
  ->  uri_resolve(RelPrev, Post, Prev),
      Dict4 = Dict3.put(_{previous: Prev})
  ;   Dict4 = Dict3
  ),
  % to
  (   xpath_chk(Dom, //span(@id=to)/a(normalize_space), ToEmailAuth)
  ->  atomic_list_concat([mailto,ToEmailAuth], :, ToEmail),
      Dict5 = Dict4.put(_{to: _{email: ToEmail}})
  ;   Dict5 = Dict4
  ).



%! timeframe_post(+TimeframeUri, -PostUri) is nondet.

timeframe_post(BaseUri, AbsUri) :-
  html_download(BaseUri, Dom),
  % @bug Removing both occurrences of ‘ul’ returns results twice!?
  xpath(Dom, //div(@class='messages-list')/ul/li/ul/li/a(1,@href), RelUri),
  uri_resolve(RelUri, BaseUri, AbsUri).





% RDF %

%! assert_post(+Post:dict) is det.

assert_post(Dict) :-
  _{
    '@id': Post,
    body: Body,
    from: _{email: FromEmail, name: FromName},
    from_date: FromDate,
    group: Group,
    id: Id,
    subject: Subject,
    to_date: ToDate
  } :< Dict,
  G = w3c,
  rdf_assert(Post, ex:body, Body^^rdf:'HTML', G),
  rdf_assert(Post, ex:from, FromEmail, G),
  rdf_assert(Post, ex:fromDate, FromDate^^xsd:dateTime, G),
  rdf_assert(FromEmail, ex:name, FromName^^xsd:string, G),
  rdf_assert(Post, ex:belongsTo, Group, G),
  rdf_assert(Post, ex:id, Id, G),
  rdf_assert(Post, ex:subject, Subject^^xsd:string, G),
  rdf_assert(Post, ex:toDate, ToDate^^xsd:dateTime, G),
  % cc
  (   _{cc: _{email: CcEmail}} :< Dict
  ->  rdf_assert(Post, ex:cc, CcEmail, G)
  ;   true
  ),
  % in_reply_to
  (   _{in_reply_to: InReplyTo} :< Dict
  ->  rdf_assert(Post, ex:inReplyTo, InReplyTo, G)
  ;   true
  ),
  % previous
  (   _{previous: Prev} :< Dict
  ->  rdf_assert(Post, ex:previous, Prev, G)
  ;   true
  ),
  % to
  (   _{to: _{email: ToEmail}} :< Dict
  ->  rdf_assert(Post, ex:to, ToEmail, G)
  ;   true
  ).





% GRAMMAR %

day_name --> "Sunday".
day_name --> "Monday".
day_name --> "Tuesday".
day_name --> "Wednesday".
day_name --> "Thursday".
day_name --> "Friday".
day_name --> "Saturday".
day_name --> "Sunday".
day_name --> date:day_name(_).



day_of_the_month(D) -->
  digit(D1),
  (digit(D2) -> {D is D1 * 10 + D2} ; {D = D1}),
  {between(1, 31, D)}.



month_name(1) --> "January".
month_name(2) --> "February".
month_name(3) --> "March".
month_name(4) --> "April".
month_name(5) --> "May".
month_name(6) --> "June".
month_name(7) --> "July".
month_name(8) --> "August".
month_name(9) --> "September".
month_name(10) --> "October".
month_name(11) --> "November".
month_name(12) --> "December".
month_name(Mo) --> date:month_name(Mo).


nonstandard_date(DateTime) -->
  (": " -> "" ; ""),
  day_name, ", ", date:ws,
  day_of_the_month(D), date:ws,
  month_name(Mo), date:ws,
  date:year(Y), date:ws,
  date:hour(H1), ":",
  date:minute(Mi1), ":",
  date:second(S1), date:ws,
  date:timezone(H2, Mi2, S2), date:ws,
  ("(" -> timezone_label(H2, Mi2, S2), ")" ; ""),
  {
    H is H1 + H2,
    Mi is Mi1 + Mi2,
    S is S1 + S2,
    date_time_stamp(date(Y,Mo,D,H,Mi,S,0,-,-), Stamp),
    stamp_date_time(Stamp, PlDateTime, 'UTC'),
    'dt-pl_to_date_time'(PlDateTime, DateTime)
  }.



timezone_label(-10, 0, 0) --> "EST".
timezone_label(0, 0, 0) --> "GMT".
timezone_label(7, 0, 0) --> "MST".
timezone_label(7, 0, 0) --> "PDT".
timezone_label(8, 0, 0) --> "PST".
