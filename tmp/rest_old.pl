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
