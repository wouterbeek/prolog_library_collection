:- encoding(utf8).
:- module(
  http_client2,
  [
    http_call/2,                  % +Uri, :Goal_1
    http_call/3,                  % +Uri, :Goal_1, +Options
    http_head2/2,                 % +Uri, +Options
    http_lmod/2,                  % +Uri, -Time
    http_metadata_content_type/2, % +Metas, -MediaType
    http_metadata_file_name/2,    % +Metas, -File
    http_metadata_link/3,         % +Metas, +Relation, -Uri
    http_open2/2,                 % +CurrentUri, -In
    http_open2/3,                 % +CurrentUri, -In, +Options
  % DEBUGGING
    curl/0,
    nocurl/0
  ]
).

/** <module> HTTP Client

```prolog
%! merge_separable_header(+Pair1:pair(atom,list(term)),
%!                         -Pair2:pair(atom,term)) is det.
%
% Succeeds iff the given HTTP Key is separable.
%
% “Multiple message-header fields with the same field-name MAY be
% present in a message if and only if the entire field-value for that
% header field is defined as a comma-separated list [i.e., #(values)].
% It MUST be possible to combine the multiple header fields into one
% "field-name: field-value" pair, without changing the semantics of
% the message, by appending each subsequent field-value to the first,
% each separated by a comma.  The order in which header fields with
% the same field-name are received is therefore significant to the
% interpretation of the combined field value, and thus a proxy MUST
% NOT change the order of these field values when a message is
% forwarded.”

merge_separable_header(Key-[Val], Key-Val) :- !.
merge_separable_header(Key-Vals, Key-Val) :-
  http:http_separable(Key), !,
  atomic_list_concat(Vals, ', ', Val).
merge_separable_header(Key-[H|T], Key-H) :-
  print_message(warning, http_nonseparable(Key,[H|T])).
```

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(http/http_client), []).
:- use_module(library(http/http_cookie), []).
:- use_module(library(http/http_generic)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(media_type)).
:- use_module(library(stream_ext)).
:- use_module(library(string_ext)).
:- use_module(library(uri_ext)).

:- use_module(http_open_cp, []).

:- meta_predicate
    http_call(+, 1),
    http_call(+, 1, +).

:- multifile
    http:post_data_hook/3.

:- public
    ssl_verify/5.

ssl_verify(_SSL, _ProblemCertificate, _AllCertificates, _FirstCertificate, _Error).





%! http_accept_value(+MediaTypes:list(compound), -Accept:atom) is det.
%
% Create an atomic HTTP Accept header value out of a given list of
% Media Types (from most to least acceptable).
%
% ```
% Accept = #( media-range [ accept-params ] )
% media-range = ( "*/*"
%               / ( type "/" "*" )
%               / ( type "/" subtype )
%               ) *( OWS ";" OWS parameter )
% parameter = token "=" ( token / quoted-string )
% accept-params  = weight *( accept-ext )
% weight = OWS ";" OWS "q=" qvalue
% accept-ext = OWS ";" OWS token [ "=" ( token / quoted-string ) ]
% ```

http_accept_value(MediaTypes, Accept) :-
  length(MediaTypes, NumMediaTypes),
  Interval is 1.0 / NumMediaTypes,
  atom_phrase(accept_(MediaTypes, Interval, Interval), Accept).

accept_([], _, _) --> !, "".
accept_([H], N, _) --> !,
  media_type(H),
  weight_(N).
accept_([H|T], N1, Interval) -->
  media_type(H),
  weight_(N1),
  {N2 is N1 + Interval},
  accept_(T, N2, Interval).

weight_(N) -->
  {format(atom(Atom), ";q=~3f", [N])},
  atom(Atom).


     
%! http_call(+Uri:atom, :Goal_1) is nondet.
%! http_call(+Uri:atom, :Goal_1, +Options:list(compound)) is nondet.
%
% Uses URIs that appear with the ‘next’ keyword in HTTP Link headers
% to non-deterministically call Goal_1 for all subsequent input
% streams.
%
% Detects cycles in HTTP Link header referals, in which case the
% cyclic_link_header/1 is thrown.
%
% The following call is made: `call(Goal_1, In)'.

http_call(Uri, Goal_1) :-
  http_call(Uri, Goal_1, []).


http_call(FirstUri, Goal_1, Options1) :-
  State = state(FirstUri),
  % Non-deterministically enumerate over URIs that appear in HTTP Link
  % headers with the ‘next’ keyword.
  repeat,
  State = state(CurrentUri),
  merge_options([next(NextUri)], Options1, Options2),
  (   http_open2(CurrentUri, In, Options2)
  ->  (   % There is a next URI: keep the choicepoint open.
          atom(NextUri)
      ->  State = state(CurrentUri),
          % Detect directly cyclic `Link' headers.
          (   CurrentUri == NextUri
          ->  throw(error(cyclic_link_header(NextUri)))
          ;   nb_setarg(1, State, NextUri)
          )
      ;   % There is no next URI: abandon choicepoint.
          !
      ),
      call_cleanup(
        call(Goal_1, In),
        close(In)
      )
  ;   !, fail
  ).



%! http_head2(+Uri:atom, +Options:list(compound)) is det.

http_head2(Uri, Options1) :-
  merge_options([method(head)], Options1, Options2),
  http_open2(Uri, In, Options2),
  close(In).



%! http_lmod(+Uri:atom, -Time:float) is det.

http_lmod(Uri, Time) :-
  http_open_cp:http_open(
    Uri,
    In,
    [header(last_modified,LastModified),method(head),status_code(Status)]
  ),
  call_cleanup(
    (
      assertion(Status =:= 200),
      assertion(at_end_of_stream(In))
    ),
    close(In)
  ),
  parse_time(LastModified, Time).



%! http_open2(+CurrentUri:atom, -In:stream) is det.
%! http_open2(+CurrentUri:atom, -In:stream, +Options:list(compound)) is det.
%
% Alternative to http_open/3 in the SWI standard library with the
% following additons:
%
%   * Allows Prolog truth/falsity to be bound to HTTP status codes
%     (e.g., Prolog truth = HTTP status code 201 for creation
%     requests).  For HTTP status codes that bind to neither truth nor
%     falsity, an exception http_status/1 is thrown.
%
%   * If present, returns the URI that appears in the HTTP Link header
%     with the ‘next’ key.  These next URIs must be used in sequent
%     requests in order to retrieve a full result set.
%
%   * Returns full meta-data, including all HTTP headers.
%
%   * Emits detailed, cURL-like debug messages about sent requests and
%     received replies.
%
% @arg Meta A list of dictionaries, each of which describing an
%      HTTP(S) request/reply interaction as well metadata about the
%      stream.
%
% @arg Options The following options are supported:
%
%   * accept(+Accept)
%
%     Accept is either a registered file name extension, a Media Types
%     compound term, or a list of Media Type compounds.
%
%   * failure(+or([-1,between(400,599)]))
%
%     Default is 400.  -1 means that no status code is mapped onto
%     failure.
%
%   * metadata(-list(dict))
%
%   * number_of_hops(+positive_integer)
%
%     The maximum number of consecutive redirects that is followed.
%     The default is 5.
%
%   * number_of_repeats(+positive_integer)
%
%     The maximum number of times the same URI is allowed to be
%     addressed during a redirect cycle.  The default
%     is 2.
%
%   * number_of_retries(+positive_integer)
%
%     The maximum number of times the same HTTP request is retries upon
%     receiving an HTTP error code (i.e., HTTP status codes 400
%     through 599).  The default is 1.
%
%   * success(+between(200,299))
%
%     Default is 200.
%
%   * Other options are passed to http_open/3.

http_open2(CurrentUri, In) :-
  http_open2(CurrentUri, In, []).


http_open2(CurrentUri, In, Options1) :-
  ignore(option(next(NextUri), Options1)),
  ignore(option(metadata(Metas), Options1)),
  http_options_(Options1, State, Options2),
  http_open2_(CurrentUri, In, State, Metas0, Options2),
  reverse(Metas0, Metas),
  http_metadata_status(In, Metas, Options2),
  ignore(http_metadata_link(Metas, next, NextUri)).

http_options_(Options1, State, Options3) :-
  (   select_option(accept(Accept), Options1, Options2)
  ->  http_open2_accept_(Accept, Atom)
  ;   Atom = '*',
      Options2 = Options1
  ),
  Options3 = [request_header('Accept'=Atom)|Options2],
  option(number_of_hops(MaxHops), Options3, 5),
  option(number_of_repeats(MaxRepeats), Options3, 2),
  option(number_of_retries(MaxRetries), Options3, 1),
  State = _{
    maximum_number_of_hops: MaxHops,
    maximum_number_of_repeats: MaxRepeats,
    maximum_number_of_retries: MaxRetries,
    number_of_retries: 1,
    visited: []
  }.

http_open2_(Uri, In2, State1, [Meta|Metas], Options1) :-
  (   debugging(http(send_request)),
      option(post(RequestBody), Options1)
  ->  debug(http(send_request), "REQUEST BODY\n~w", [RequestBody])
  ;   true
  ),
  merge_options(
    Options1,
    [
      cert_verify_hook(cert_accept_any),
      raw_headers(HeaderLines),
      redirect(false),
      status_code(Status),
      timeout(60),
      version(Major-Minor)
    ],
    Options2
  ),
  get_time(Start),
  http_open_cp:http_open(Uri, In1, Options2),
  ignore(option(status_code(Status), Options2)),
  get_time(End),
  http_lines_pairs(HeaderLines, HeaderPairs),
  (   memberchk(location-[Location], HeaderPairs)
  ->  State2 = State1.put(_{location: Location})
  ;   State2 = State1
  ),
  dict_pairs(HeadersMeta, HeaderPairs),
  Meta = http{
    headers: HeadersMeta,
    status: Status,
    timestamp: Start-End,
    uri: Uri,
    version: version{major: Major, minor: Minor}
  },
  State3 = State2.put(_{meta: Meta}),
  % Print status codes and reply headers as debug messages.
  % Use curl/0 to show these debug messages.
  (   debugging(http(receive_reply))
  ->  debug(http(receive_reply), "", []),
      http_status_reason(Status, Reason),
      debug(http(receive_reply), "< ~d (~s)", [Status,Reason]),
      maplist(debug_header, HeaderPairs),
      debug(http(receive_reply), "", [])
  ;   true
  ),
  http_open2_(Uri, In1, Status, State3, In2, Metas, Options1).

debug_header(Key-Values) :-
  maplist(debug_header(Key), Values).

debug_header(Key, Value) :-
  debug(http(receive_reply), "< ~a: ~w", [Key,Value]).

% list of Media Types
http_open2_accept_(MediaTypes, Atom) :-
  is_list(MediaTypes), !,
  http_accept_value(MediaTypes, Atom).
% file name extension
http_open2_accept_(Ext, Atom) :-
  atom(Ext), !,
  (   media_type_extension(MediaType, Ext)
  ->  http_open2_accept_([MediaType], Atom)
  ;   existence_error(media_type_extension, Ext)
  ).
% Media Type
http_open2_accept_(MediaType, Atom) :-
  http_open2_accept_([MediaType], Atom).

% succes status code
http_open2_(Uri, In1, Status, State, In2, [], _) :-
  between(200, 299, Status), !,
  http_open2_success_(Uri, In1, State, In2).
% redirect status code
http_open2_(Uri1, In1, Status, State1, In2, Metas, Options) :-
  between(300, 399, Status), !,
  close(In1),
  _{location: Location, visited: Visited1} :< State1,
  uri_resolve(Location, Uri1, Uri2),
  Visited2 = [Uri2|Visited1],
  (   length(Visited2, NumVisited),
      _{maximum_number_of_hops: MaxHops} :< State1,
      NumVisited >= MaxHops
  ->  Metas = [],
      reverse(Visited2, Visited3),
      print_message(warning, http(max_redirect(NumVisited,Visited3)))
  ;   include(==(Uri2), Visited2, Visited3),
      length(Visited3, NumRepeats),
      _{maximum_number_of_repeats: MaxRepeats} :< State1,
      NumRepeats >= MaxRepeats
  ->  Metas = [],
      print_message(warning, http(redirect_loop(Uri2)))
  ;   State2 = State1.put(_{visited: Visited2}),
      http_open2_(Uri2, In2, State2, Metas, Options)
  ).
% authentication error status code
http_open2_(_, In, Status, _, In, [], _) :-
  Status =:= 401, !.
% non-authentication error status code
http_open2_(Uri, In1, Status, State1, In2, Metas, Options) :-
  between(400, 599, Status), !,
  _{
    maximum_number_of_retries: MaxRetries,
    number_of_retries: Retries1
  } :< State1,
  Retries2 is Retries1 + 1,
  (   Retries2 >= MaxRetries
  ->  In2 = In1,
      Metas = []
  ;   close(In1),
      State2 = State1.put(_{number_of_retries: Retries2}),
      http_open2_(Uri, In2, State2, Metas, Options)
  ).
% unrecodnized status code
http_open2_(_, In, Status, _, _, [], _) :-
  close(In),
  domain_error(http_status, Status).

% Change the input stream encoding based on the value of the
% `Content-Type' header.
http_open2_success_(_, In, State, In) :-
  _{meta: Meta} :< State,
  http_metadata_content_type([Meta], _MediaType), !.
/*
  (   media_type_encoding(MediaType, Encoding)
  ->  (   recode_stream(Encoding, In1)
      ->  In2 = In1
      ;   recode_stream(Encoding, In1, In2)
      )
  ;   In2 = In1
  ).
*/
% If there is no `Content-Type' header, then there must be no content
% either.
http_open2_success_(Uri, In, _, In) :-
  (   at_end_of_stream(In)
  ->  true
  ;   print_message(warning, http(no_content_type, Uri))
  ).

http_lines_pairs(Lines, GroupedPairs) :-
  maplist(http_parse_header_pair, Lines, Pairs),
  keysort(Pairs, SortedPairs),
  group_pairs_by_key(SortedPairs, GroupedPairs).

http_parse_header_pair(Line, Key-Value) :-
  once(phrase(http_parse_header_simple(Key, Value), Line)).

% ```
% header-field = field-name ":" OWS field-value OWS
% field-name = token
% OWS = *( SP | HTAB )
% ```
http_parse_header_simple(Key, Value) -->
  string_without(":", KeyCodes),
  ":",
  {
    atom_codes(Key0, KeyCodes),
    downcase_atom(Key0, Key)
  },
  remainder_as_string(String0),
  {
    string_strip(String0, "\s\t", String),
    atom_string(Value, String)
  }.

http:post_data_hook(string(String), Out, HdrExtra) :-
  atom_string(Atom, String),
  http_header:http_post_data(atom(Atom), Out, HdrExtra).
http:post_data_hook(string(MediaType,String), Out, HdrExtra) :-
  atom_string(Atom, String),
  http_header:http_post_data(atom(MediaType,Atom), Out, HdrExtra).



%! http_metadata_content_type(+Metas:list(dict), -MediaType:compound) is semidet.
%
% We cannot expect that an HTTP `Content-Type' header is present:
%
%   - Some HTTP replies have no content.
%
%   - Some non-empty HTTP replies omit the header (probably
%     incorrectly), e.g., `http://abs.270a.info/sparql'.
%
%   - Some Content-Type headers' values do not follow the grammar for
%     Media Types.

http_metadata_content_type(Metas, MediaType) :-
  Metas = [Meta|_],
  get_dict('content-type', Meta.headers, [ContentType|T]),
  assertion(T == []),
  http_parse_header_value(content_type, ContentType, MediaType).



%! http_metadata_file_name(+Metas:list(dict), -File:atom) is semidet.

http_metadata_file_name(Metas, File) :-
  Metas = [Meta|_],
  dict_get('content-disposition', Meta.headers, [ContentDisposition|T]),
  assertion(T == []),
  split_string(ContentDisposition, ";", " ", ["attachment"|Params]),
  member(Param, Params),
  split_string(Param, "=", "\"", ["filename",File0]), !,
  atom_string(File, File0).



%! http_metadata_link(+Metas:list(dict), +Relation:atom, -Uri:atom) is semidet.

http_metadata_link(Metas, Relation, Uri) :-
  [Meta|_] = Metas,
  dict_get(link, Meta.headers, Links),
  % This header may appear multiple times.
  atomic_list_concat(Links, ;, Link),
  atom_string(Relation, Relation0),
  split_string(Link, ",", " ", Comps),
  member(Comp, Comps),
  split_string(Comp, ";", "<> ", [Uri0|Params]),
  member(Param, Params),
  split_string(Param, "=", "\"", ["rel",Relation0]), !,
  atom_string(Uri, Uri0).



%! http_metadata_status(+In:stream, +Metas:list(dict),
%!                      +Options:list(compound)) is det.

http_metadata_status(In, Metas, Options) :-
  Metas = [Meta|_],
  _{status: Status, uri: Uri} :< Meta,
  option(success(Success), Options, 200),
  option(failure(Failure), Options, 400),
  (   Status =:= Success
  ->  true
  ;   (   ground(In)
      ->  call_cleanup(
            read_string(In, 1 000, Msg),
            close(In)
          )
      ;   Msg = "no content"
      ),
      % Map the failure code to `fail', but throw an error for other
      % error codes.
      (Status =:= Failure -> fail ; throw(error(http_status(Status,Msg),Uri)))
  ).





% DEBUGGING %

%! curl is det.
%
% Enable detailed, cURL-like debug messages.

curl :-
  debug(http(receive_reply)),
  debug(http(send_request)).



%! nocurl is det.
%
% Disable detailed, cURL-like debug messages.

nocurl :-
  nodebug(http(receive_reply)),
  nodebug(http(send_request)).
