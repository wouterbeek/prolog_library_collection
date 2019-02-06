:- module(
  uri_ext,
  [
    append_segments/3,     % +Segments1, +Segments2, ?Segments3
    uri_comp_set/4,        % +Kind, +Uri1, +Component, -Uri2
    uri_comps/2,           % ?Uri, ?Components
    uri_file_extensions/2, % +Uri, -Extensions
    uri_local_name/2,      % +Uri, -Local
    uri_media_type/2,      % +Uri, -MediaType
    uri_scheme/1,          % ?Scheme
    uri_strip/2            % +Uri, -Base
  ]
).

/** <module> URI extension

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(plunit)).

:- use_module(library(dict)).
:- use_module(library(file_ext)).
:- use_module(library(http/http_generic), []).





%! append_segments(+Segments1:list(atom), +Segments2:list(atom), +Segments3:list(atom)) is semidet.
%! append_segments(+Segments1:list(atom), +Segments2:list(atom), -Segments3:list(atom)) is det.
%
% Appends lists of path segments.  Empty segments commonly appear at
% the beginning and end of URI paths.

append_segments(L1a, L2a, L3) :-
  exclude([X]>>(X==''), L1a, L1b),
  exclude([X]>>(X==''), L2a, L2b),
  append(L1b, L2b, L3).

:- begin_tests(append_segments).

test('append_segments(+,+,+)', [forall(test_append_segments(L1,L2,L3))]) :-
  append_segments(L1, L2, L3).
test('append_segments(+,+,+)', [forall(test_append_segments(L1,L2,L3))]) :-
  append_segments(L1, L2, L3_),
  assertion(L3_ == L3).

test_append_segments(['',a,b,c,''], [''], [a,b,c]).

:- end_tests(append_segments).



%! uri_comp_set(+Kind:oneof([fragment,query]), +Uri1, +Component, -Uri2) is det.
%
% Change a specific URI component.

uri_comp_set(fragment, Uri1, Fragment, Uri2) :-
  uri_components(Uri1, uri_components(Scheme,Authority,Path,Query,_)),
  uri_components(Uri2, uri_components(Scheme,Authority,Path,Query,Fragment)).
uri_comp_set(query, Uri1, QueryComponents, Uri2) :-
  uri_components(Uri1, uri_components(Scheme,Authority,Path,_,Fragment)),
  uri_query_components(Query, QueryComponents),
  uri_components(Uri2, uri_components(Scheme,Authority,Path,Query,Fragment)).



%! uri_comps(+Uri, -Components) is det.
%! uri_comps(-Uri, +Components) is det.
%
% Components is a compound term of the form
% `uri(Scheme,Authority,Segments,Query,Fragment)', where:
%
%   * Authority is either an atom or a compound term of the form
%     `auth(User,Password,Host,Port)'.
%
%   * Segments is a list of atomic path segments.
%
%   * Query is (1) a list of unary compound terms, or (2) a list of
%     pairs, or (3) a flat dict (i.e., a dict with non-dict values).

uri_comps(Uri, uri(Scheme,AuthorityComp,Segments,QueryComponents,Fragment)) :-
  ground(Uri), !,
  uri_components(Uri, uri_components(Scheme,Authority,Path,Query,Fragment)),
  (   atom(Authority),
      var(AuthorityComp)
  ->  AuthorityComp = Authority
  ;   auth_comps_(Scheme, Authority, AuthorityComp)
  ),
  atomic_list_concat([''|Segments], /, Path),
  (   var(Query)
  ->  QueryComponents = []
  ;   % @hack Currently needed because buggy URI query components are
      %       common.
      catch(uri_query_components(Query, QueryComponents0), _, fail)
  ->  list_to_set(QueryComponents0, QueryComponents)
  ;   QueryComponents = []
  ).
uri_comps(Uri, uri(Scheme,Authority0,Segments,QueryComponents,Fragment)) :-
  (   atom(Authority0)
  ->  Authority = Authority0
  ;   auth_comps_(Scheme, Authority, Authority0)
  ),
  (   var(Segments)
  ->  true
  ;   Segments == ['']
  ->  Path = '/'
  ;   atomic_list_concat([''|Segments], /, Path)
  ),
  (   var(QueryComponents)
  ->  true
  ;   is_list(QueryComponents)
  ->  uri_query_components(Query, QueryComponents)
  ;   is_dict(QueryComponents)
  ->  dict_pairs(QueryComponents, QueryPairs),
      uri_query_components(Query, QueryPairs)
  ;   atomic(QueryComponents)
  ->  Query = QueryComponents
  ;   type_error(uri_query_components, QueryComponents)
  ),
  uri_components(Uri, uri_components(Scheme,Authority,Path,Query,Fragment)).

auth_comps_(_, Authority, auth(User,Password,Host,Port)) :-
  ground(Authority), !,
  uri_authority_components(Authority, uri_authority(User,Password,Host,Port)).
auth_comps_(Scheme, Authority, auth(User,Password,Host,Port0)) :-
  (   var(Port0)
  ->  true
  ;   % Leave out the port if it is the default port for the given
      % Scheme.
      ground(Scheme),
      uri:default_port(Scheme, Port0)
  ->  true
  ;   Port = Port0
  ),
  % Create the Authorityority string.
  uri_authority_components(Authority, uri_authority(User,Password,Host,Port)).



%! uri_file_extensions(+Uri:atom, -Extensions:list(atom)) is det.

uri_file_extensions(Uri, Extensions) :-
  uri_local_name(Uri, Local),
  file_extensions(Local, Extensions).



%! uri_local_name(+Uri:atom, -Local:atom) is det.

uri_local_name(Uri, Local) :-
  uri_comps(Uri, uri(_,_,Segments,_,_)),
  last(Segments, Local).



%! uri_media_type(+Uri:atom, -MediaType:compound) is det.

uri_media_type(Uri, MediaType) :-
  uri_file_extensions(Uri, Extensions),
  file_extensions_media_type(Extensions, MediaType).



%! uri_scheme(+Schema:atom) is semidet.
%! uri_scheme(-Schema:atom) is nondet.

uri_scheme(aaa).
uri_scheme(aaas).
uri_scheme(about).
uri_scheme(acap).
uri_scheme(acct).
uri_scheme(acr).
uri_scheme(adiumxtra).
uri_scheme(afp).
uri_scheme(afs).
uri_scheme(aim).
uri_scheme(appdata).
uri_scheme(apt).
uri_scheme(attachment).
uri_scheme(aw).
uri_scheme(barion).
uri_scheme(beshare).
uri_scheme(bitcoin).
uri_scheme(blob).
uri_scheme(bolo).
uri_scheme(browserext).
uri_scheme(callto).
uri_scheme(cap).
uri_scheme(chrome).
uri_scheme('chrome-extension').
uri_scheme(cid).
uri_scheme(coap).
uri_scheme('coap+tcp').
uri_scheme('coap+ws').
uri_scheme(coaps).
uri_scheme('coaps+tcp').
uri_scheme('coaps+ws').
uri_scheme('com-eventbrite-attendee').
uri_scheme(content).
uri_scheme(conti).
uri_scheme(crid).
uri_scheme(cvs).
uri_scheme(data).
uri_scheme(dav).
uri_scheme(diaspora).
uri_scheme(dict).
uri_scheme(did).
uri_scheme(dis).
uri_scheme('dlna-playcontainer').
uri_scheme('dlna-playsingle').
uri_scheme(dns).
uri_scheme(dntp).
uri_scheme(dtn).
uri_scheme(dvb).
uri_scheme(ed2k).
uri_scheme(elsi).
uri_scheme(example).
uri_scheme(facetime).
uri_scheme(fax).
uri_scheme(feed).
uri_scheme(feedready).
uri_scheme(file).
uri_scheme(filesystem).
uri_scheme(finger).
uri_scheme(fish).
uri_scheme(ftp).
uri_scheme(geo).
uri_scheme(gg).
uri_scheme(git).
uri_scheme(gizmoproject).
uri_scheme(go).
uri_scheme(gopher).
uri_scheme(graph).
uri_scheme(gtalk).
uri_scheme(h323).
uri_scheme(ham).
uri_scheme(hcp).
uri_scheme(http).
uri_scheme(https).
uri_scheme(hxxp).
uri_scheme(hxxps).
uri_scheme(hydrazone).
uri_scheme(iax).
uri_scheme(icap).
uri_scheme(icon).
uri_scheme(im).
uri_scheme(imap).
uri_scheme(info).
uri_scheme(iotdisco).
uri_scheme(ipn).
uri_scheme(ipp).
uri_scheme(ipps).
uri_scheme(irc).
uri_scheme(irc6).
uri_scheme(ircs).
uri_scheme(iris).
uri_scheme('iris.beep').
uri_scheme('iris.lwz').
uri_scheme('iris.xpc').
uri_scheme('iris.xpcs').
uri_scheme(isostore).
uri_scheme(itms).
uri_scheme(jabber).
uri_scheme(jar).
uri_scheme(jms).
uri_scheme(keyparc).
uri_scheme(lastfm).
uri_scheme(ldap).
uri_scheme(ldaps).
uri_scheme(lvlt).
uri_scheme(magnet).
uri_scheme(mailserver).
uri_scheme(mailto).
uri_scheme(maps).
uri_scheme(market).
uri_scheme(message).
uri_scheme('microsoft.windows.camera').
uri_scheme('microsoft.windows.camera.multipicker').
uri_scheme('microsoft.windows.camera.picker').
uri_scheme(mid).
uri_scheme(mms).
uri_scheme(modem).
uri_scheme(mongodb).
uri_scheme(moz).
uri_scheme('ms-access').
uri_scheme('ms-browser-extension').
uri_scheme('ms-drive-to').
uri_scheme('ms-enrollment').
uri_scheme('ms-excel').
uri_scheme('ms-gamebarservices').
uri_scheme('ms-gamingoverlay').
uri_scheme('ms-getoffice').
uri_scheme('ms-help').
uri_scheme('ms-infopath').
uri_scheme('ms-inputapp').
uri_scheme('ms-lockscreencomponent-config').
uri_scheme('ms-media-stream-id').
uri_scheme('ms-mixedrealitycapture').
uri_scheme('ms-officeapp').
uri_scheme('ms-people').
uri_scheme('ms-project').
uri_scheme('ms-powerpoint').
uri_scheme('ms-publisher').
uri_scheme('ms-restoretabcompanion').
uri_scheme('ms-search-repair').
uri_scheme('ms-secondary-screen-controller').
uri_scheme('ms-secondary-screen-setup').
uri_scheme('ms-settings').
uri_scheme('ms-settings-airplanemode').
uri_scheme('ms-settings-bluetooth').
uri_scheme('ms-settings-camera').
uri_scheme('ms-settings-cellular').
uri_scheme('ms-settings-cloudstorage').
uri_scheme('ms-settings-connectabledevices').
uri_scheme('ms-settings-displays-topology').
uri_scheme('ms-settings-emailandaccounts').
uri_scheme('ms-settings-language').
uri_scheme('ms-settings-location').
uri_scheme('ms-settings-lock').
uri_scheme('ms-settings-nfctransactions').
uri_scheme('ms-settings-notifications').
uri_scheme('ms-settings-power').
uri_scheme('ms-settings-privacy').
uri_scheme('ms-settings-proximity').
uri_scheme('ms-settings-screenrotation').
uri_scheme('ms-settings-wifi').
uri_scheme('ms-settings-workplace').
uri_scheme('ms-spd').
uri_scheme('ms-sttoverlay').
uri_scheme('ms-transit-to').
uri_scheme('ms-useractivityset').
uri_scheme('ms-virtualtouchpad').
uri_scheme('ms-visio').
uri_scheme('ms-walk-to').
uri_scheme('ms-whiteboard').
uri_scheme('ms-whiteboard-cmd').
uri_scheme('ms-word').
uri_scheme(msnim).
uri_scheme(msrp).
uri_scheme(msrps).
uri_scheme(mtqp).
uri_scheme(mumble).
uri_scheme(mupdate).
uri_scheme(mvn).
uri_scheme(news).
uri_scheme(nfs).
uri_scheme(ni).
uri_scheme(nih).
uri_scheme(nntp).
uri_scheme(notes).
uri_scheme(ocf).
uri_scheme(oid).
uri_scheme(onenote).
uri_scheme('onenote-cmd').
uri_scheme(opaquelocktoken).
uri_scheme(openpgp4fpr).
uri_scheme(pack).
uri_scheme(palm).
uri_scheme(paparazzi).
uri_scheme(pkcs11).
uri_scheme(platform).
uri_scheme(pop).
uri_scheme(pres).
uri_scheme(prospero).
uri_scheme(proxy).
uri_scheme(pwid).
uri_scheme(psyc).
uri_scheme(qb).
uri_scheme(query).
uri_scheme(redis).
uri_scheme(rediss).
uri_scheme(reload).
uri_scheme(res).
uri_scheme(resource).
uri_scheme(rmi).
uri_scheme(rsync).
uri_scheme(rtmfp).
uri_scheme(rtmp).
uri_scheme(rtsp).
uri_scheme(rtsps).
uri_scheme(rtspu).
uri_scheme(secondlife).
uri_scheme(service).
uri_scheme(session).
uri_scheme(sftp).
uri_scheme(sgn).
uri_scheme(shttp).
uri_scheme(sieve).
uri_scheme(sip).
uri_scheme(sips).
uri_scheme(skype).
uri_scheme(smb).
uri_scheme(sms).
uri_scheme(smtp).
uri_scheme(snews).
uri_scheme(snmp).
uri_scheme('soap.beep').
uri_scheme('soap.beeps').
uri_scheme(soldat).
uri_scheme(spiffe).
uri_scheme(spotify).
uri_scheme(ssh).
uri_scheme(steam).
uri_scheme(stun).
uri_scheme(stuns).
uri_scheme(submit).
uri_scheme(svn).
uri_scheme(tag).
uri_scheme(teamspeak).
uri_scheme(tel).
uri_scheme(teliaeid).
uri_scheme(telnet).
uri_scheme(tftp).
uri_scheme(things).
uri_scheme(thismessage).
uri_scheme(tip).
uri_scheme(tn3270).
uri_scheme(tool).
uri_scheme(turn).
uri_scheme(turns).
uri_scheme(tv).
uri_scheme(udp).
uri_scheme(unreal).
uri_scheme(urn).
uri_scheme(ut2004).
uri_scheme('v-event').
uri_scheme(vemmi).
uri_scheme(ventrilo).
uri_scheme(videotex).
uri_scheme(vnc).
uri_scheme('view-source').
uri_scheme(wais).
uri_scheme(webcal).
uri_scheme(wpid).
uri_scheme(ws).
uri_scheme(wss).
uri_scheme(wtai).
uri_scheme(wyciwyg).
uri_scheme(xcon).
uri_scheme('xcon-userid').
uri_scheme(xfire).
uri_scheme('xmlrpc.beep').
uri_scheme('xmlrpc.beeps').
uri_scheme(xmpp).
uri_scheme(xri).
uri_scheme(ymsgr).
uri_scheme('z39.50').
uri_scheme('z39.50r').
uri_scheme('z39.50s').



%! uri_strip(+Uri1:atom, -Uri2:atom) is det.
%
% Uri2 is like Uri1, but without the query and fragment components.

uri_strip(Uri1, Uri2) :-
  uri_comps(Uri1, uri(Scheme,Auth,Segments,_,_)),
  uri_comps(Uri2, uri(Scheme,Auth,Segments,_,_)).
