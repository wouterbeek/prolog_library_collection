%! rest_handler(+Req, +HandleId, :Exists_1, :Instance_4, :Class_3) is det.
%
% REST call on an endpoint that have the following form:
%
%    <PREFIX>/<CLASS>/<INSTANCE>
%
% where specific requests can be issued against, and where an overview
% of all instances can be issued against <PREFIX>/<CLASS>.
%
% If <INSTANCE> does not exist (yet) the also the <CLASS>-level call
% is made.

rest_handler(Req, HandleId, Exists_1, Instance_4, Class_3) :-
  memberchk(request_uri(Local), Req),
  http_accept(Req, MTs),
  memberchk(method(Method), Req),
  http_link_to_id(HandleId, Endpoint),
  (   Local == Endpoint
  ->  rest_handler0(Class_3, Req, Method, MTs)
  ;   iri_to_resource(Local, Res),
      call(Exists_1, Res)
  ->  rest_handler0(Instance_4, Req, Method, MTs, Res)
  ), !.
rest_handler(Req, _, _, _, _) :-
  rest_not_found(Req).



%! rest_handler0(:Goal_3, +Req, +Method, +MTs) is det.

rest_handler0(Goal_3, Req, Method, MTs) :-
  member(MT, MTs),
  rest_call_or_exception(Goal_3, Req, Method, MT), !.
rest_handler0(_, _, _, _) :-
  why_not_acceptable(Why),
  http_status_reply(not_acceptable(Why)).


%! rest_handler0(:Instance_4, +Req, +Method, +MTs, +Res) is det.

rest_handler0(Instance_4, Req, Method, MTs, Res) :-
  member(MT, MTs),
  rest_call_or_exception(Instance_4, Req, Method, MT, Res), !.
rest_handler0(_, _, _, _, _) :-
  why_not_acceptable(Why),
  http_status_reply(not_acceptable(Why)).





% HELPERS %

%! rest_call_or_exception(:Goal_3, +Req, +Method, +MediaType) is det.

rest_call_or_exception(Goal_3, Req, Method, MT) :-
  catch(call(Goal_3, Req, Method, MT), E, rest_exception(MT, E)).


%! rest_call_or_exception(:Instance_4, +Req, +Method, +MediaType, +Res) is det.

rest_call_or_exception(Instance_4, Req, Method, MT, Res) :-
  catch(call(Instance_4, Req, Method, MT, Res), E, rest_exception(MT, E)).



%! rest_not_found(+Req) is det.

rest_not_found(Req) :-
  memberchk(request_uri(Res), Req),
  http_status_reply(Req, not_found(Res)).


why_not_acceptable(p("No acceptable media type could be served.")).
