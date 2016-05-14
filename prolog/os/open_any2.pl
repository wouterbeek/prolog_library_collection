:- module(
  open_any2,
  [
    close_any2/3,       % +Close_0, +M1,     -M2
    open_any2/5,        % +Source,  +Mode,   -Stream, -Close_0, -M
    open_any2/6,        % +Source,  +Mode,   -Stream, -Close_0, -M, +Opts
    read_mode/1,        % ?Mode
    call_on_stream/2,   % +Source,           :Goal_3
    call_on_stream/3,   % +Source,           :Goal_3, +Opts
    call_onto_stream/3, % +Source,  +Sink,   :Goal_6
    call_onto_stream/4, % +Source,  +Sink,   :Goal_6, +Opts
    call_to_stream/2,   %           +Sink,   :Goal_3
    call_to_stream/3,   %           +Sink,   :Goal_3, +Opts
    source_numlines/2,  % +Source,  -NumLines
    write_mode/1        % ?Mode
  ]
).

/** <module> Open any v2

Wrapper around library(iostream)'s open_any/5.

@author Wouter Beek
@version 2015/10-2016/05
*/

:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(debug_ext)).
:- use_module(library(date_time/date_time)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(http/http_cookie)). % HTTP cookie support.
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_open)). % HTTP support.
:- use_module(library(http/http_ssl_plugin)). % HTTPS support.
:- use_module(library(http/http11)).
:- use_module(library(option_ext)).
:- use_module(library(iostream)).
:- use_module(library(lists)).
:- use_module(library(os/io_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(print_ext)).
:- use_module(library(readutil)).
:- use_module(library(ssl)). % SSL support.
:- use_module(library(string_ext)).
:- use_module(library(typecheck)).
:- use_module(library(uri)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

% @tbd This avoids the following warning, but should it be necessary?
%      ```
%      [HTTP] time: 0.026562sec; message: '$c_call_prolog'/0: Undefined procedure: open_any2:ssl_verify/5
%      ```
:- public
    ssl_verify/5.

ssl_verify(_SSL, _ProblemCertificate, _AllCertificates, _FirstCertificate, _Error).

:- meta_predicate
    call_on_stream(+, 3),
    call_on_stream(+, 3, +),
    call_on_stream1(+, +, 3, +, -, +),
    call_on_stream2(+, +, 3, +, -, +),
    call_onto_stream(+, +, 6),
    call_onto_stream(+, +, 6, +),
    call_to_stream(+, 3),
    call_to_stream(+, 3, +).





%! call_on_stream(+Source, :Goal_3) is det.
%! call_on_stream(+Source, :Goal_3, +Opts) is det.
% The following call is made: `call(Goal_3, M, In)`.
%
% The following options are supported:
%   - metadata(-dict)
%   - Other options are passed to:
%       - archive_data_stream/3
%       - archive_open/3
%       - open_any2/5

call_on_stream(Source, Goal_3) :-
  call_on_stream(Source, Goal_3, []).


call_on_stream(Source, Goal_3, Opts) :-
  % This allows the calling context to request one specific entity.
  option(archive_entry(Entry), Opts, _),
  % Archive options that cannot be overridden.
  findall(format(Format), archive_format(Format, true), FormatOpts),
  merge_options([close_parent(false)|FormatOpts], Opts, ArchOpts1),
  % Archive options that can be overridden.
  merge_options(ArchOpts1, [filter(all)], ArchOpts2),
  setup_call_cleanup(
    open_any2(Source, read, In1, Close_0, M1, Opts),
    setup_call_cleanup(
      archive_open(In1, Arch, ArchOpts2),
      call_on_stream1(Arch, Entry, Goal_3, M1, M2, Opts),
      archive_close(Arch)
    ),
    (
      (var(M2) -> M3 = M1 ; M3 = M2),
      close_any2(Close_0, M3, M4)
    )
  ),
  ignore(option(metadata(M4), Opts)).


archive_format(Format) :-
  archive_format(Format, _).

archive_format('7zip',  true ).
archive_format(ar,      true ).
archive_format(cab,     true ).
archive_format(cpio,    true ).
archive_format(empty,   true ).
archive_format(gnutar,  true ).
archive_format(iso9660, true ).
archive_format(lha,     true ).
archive_format(mtree,   false).
archive_format(rar,     true ).
archive_format(raw,     true ).
archive_format(tar,     true ).
archive_format(xar,     true ).
archive_format(zip,     true ).

    
% Semi-deterministic if an archive entry name is given.
call_on_stream1(Arch, Entry, Goal_3, M1, M3, Opts) :-
  nonvar(Entry), !,
  call_on_stream2(Arch, Entry, Goal_3, M1, M3, Opts), !.
% Non-deterministic if no archive entry name is given.
call_on_stream1(Arch, Entry, Goal_3, M1, M3, Opts) :-
  call_on_stream2(Arch, Entry, Goal_3, M1, M3, Opts).


call_on_stream2(Arch, Entry, Goal_3, M1, M3, Opts0) :-
  merge_options([meta_data(MEntries1)], Opts0, Opts),
  (   archive_data_stream(Arch, In2, Opts)
  ->  maplist(archive_entry_metadata0, MEntries1, MEntries2),
      (   MEntries2 = [H1|T],
          atom_string(Entry, H1.'llo:name')
      ->  call_cleanup(
            call(Goal_3, In2, M1, M2),
            close_any2(close(In2), H1, H2)
          ),
          M3 = M2.put(_{'llo:archive_entry': [H2|T]})
      ;   close(In2),
          fail
      )
  ;   throw(error(no_content(Arch),_))
  ).


archive_entry_metadata0(
  archive_meta_data{
    filetype: Filetype0,
    filters: Filters0,
    format: Format0,
    mtime: Mtime,
    name: Name0,
    size: Size
  },
  _{
    '@type': 'llo:ArchiveEntry',
    'llo:filetype': Filetype,
    'llo:filters': Filters,
    'llo:format': Format,
    'llo:mtime': Mtime,
    'llo:name': Name,
    'llo:size': Size
  }
) :-
  maplist(atom_string,
    [Filetype0,Format0,Name0|Filters0],
    [Filetype,Format,Name|Filters]
  ).



%! call_onto_stream(+Source, +Sink, :Goal_6) is det.
%! call_onto_stream(+Source, +Sink, :Goal_6, +Opts) is det.

call_onto_stream(Source, Sink, Goal_6) :-
  call_onto_stream(Source, Sink, Goal_6, []).


call_onto_stream(Source, Sink, Goal_6, InOpts) :-
  remove_option(InOpts, metadata(_), OutOpts),
  call_on_stream(Source, call_onto_stream0(Sink, Goal_6, OutOpts), InOpts).


call_onto_stream0(Sink, Mod:Goal_6, Opts, In, M1, M2) :-
  Goal_3 =.. [Goal_6,In,M1,M2],
  call_to_stream(Sink, Mod:Goal_3, Opts).
  


%! call_to_stream(+Sink, :Goal_3) is det.
%! call_to_stream(+Sink, :Goal_3, +Opts) is det.
% The following options are supported:
%   * metadata(-dict)
%   * mode(+oneof([append,write]))
%     Default is `write`.

call_to_stream(Sink, Goal_3) :-
  call_to_stream(Sink, Goal_3, []).


call_to_stream(Sink, Goal_3, Opts) :-
  option(mode(Mode), Opts, write),
  setup_call_cleanup(
    open_any2(Sink, Mode, Out, Close_0, M1, Opts),
    call(Goal_3, Out, M1, M2),
    close_any2(Close_0, M2, M3)
  ),
  ignore(option(metadata(M3), Opts)).



%! close_any2(+Close_0, +M1, -M2) is det.

close_any2(Close_0, M1, M2) :-
  (   Close_0 = close(Stream)
  ->  stream_metadata(Stream, MStream),
      M2 = M1.put(MStream)
  ;   M2 = M1
  ),
  close_any(Close_0).



%! open_any2(+Source, +Mode, -Stream, -Close_0, -M) is det.
%! open_any2(+Source, +Mode, -Stream, -Close_0, -M, +Opts) is nondet.
% The following options are supported:
%   * compression(+oneof([deflate,gzip,none]))
%     Whether or not compression is used on the opened stream.
%     Default is `none`.
%   * parse_headers(+boolean)
%     Whether HTTP headers are parsed according to HTTP 1.1 grammars.
%     Default is `false`.
%   * max_redirects(+positive_integer)
%     The maximum number of redirects that is followed when opening a stream
%     over HTTP.
%     Default is 5.
%   * max_retries(+positive_integer)
%     The maximum number of retries that is performed when opening a stream
%     over HTTP.  A retry is made whenever a 4xx- or 5xx-range HTTP status
%     code is returned.
%     Default is 1.
%   * Other options are passed to open_any/5.
%
% @throws existence_error if an HTTP request returns an error code.

open_any2(Source, Mode, Stream, Close_0, M) :-
  open_any2(Source, Mode, Stream, Close_0, M, []).


open_any2(Source0, Mode, Stream, Close_0, M3, Opts) :-
  source_type(Source0, Mode, Source, Type),
  
  % Base IRI
  (   base_iri(Source, BaseIri, Opts)
  ->  M1 = _{'llo:base_iri': BaseIri}
  ;   M1 = _{}
  ),
  
  % We want more support for opening an HTTP IRI stream
  % than what `library(http/http_open)` provides.
  (   Type == http_iri
  ->  must_be(oneof([read]), Mode),
      http_open2(Source, Stream0, Close0_0, Ms, Opts),
      M2 = M1.put(_{'llo:http_communication': Ms, '@type': 'llo:HttpIri'})
  ;   open_any(Source, Mode, Stream0, Close0_0, Opts),
      M2 = M1.put(_{'llo:mode': Mode, '@type': 'llo:FileIri'})
  ),

  % Perform compression on the stream (decompress a read stream;
  % compress a write stream).
  option(compression(Comp), Opts, none),
  compression(Mode, Comp, Stream0, Stream, M2, M3, Close0_0, Close_0),

  % Make sure the metadata is accessible even the case of an HTTP error code.
  (   http_get_dict('llo:status', M3, Status),
      http_error(Status)
  ->  existence_error(open_any2, M3)
  ;   true
  ).


%! http_open2(+Iri, -Read, -Close_0, -Ms, +Opts) is det.

http_open2(Iri, Stream, Close_0, Ms, Opts) :-
  option(max_redirects(MaxRedirect), Opts, 5),
  option(max_retries(MaxRetry), Opts, 1),
  State = _{
    max_redirects: MaxRedirect,
    max_retries: MaxRetry,
    redirects: 0,
    retries: 0,
    visited: []
  },
  http_open2(Iri, State, Stream, Close_0, Ms, Opts).


%! http_open2(+Iri, +State, -Read, -Close_0, -Ms, +Opts) is det.

http_open2(Iri, State, Stream, Close_0, Ms, Opts0) :-
  copy_term(Opts0, Opts1),
  Opts2 = [
    authenticate(false),
    cert_verify_hook(cert_accept_any),
    header(location,Location),
    raw_headers(Lines),
    redirect(false),
    status_code(Status),
    version(Major-Minor)
  ],
  merge_options(Opts1, Opts2, Opts3),
  call_time(catch(http_open(Iri, Stream0, Opts3), E, true), Time),
  (   var(E)
  ->  deb_http_headers(Lines),
      http_parse_headers(Lines, Groups, Opts0),
      dict_pairs(Headers, Groups),
      M = _{
        'llo:headers': Headers,
        'llo:iri': Iri,
        'llo:status': Status,
        'llo:time': Time,
        'llo:version': _{'llo:major': Major, 'llo:minor': Minor}
      },
      http_open2(Iri, State, Location, Stream0, Stream, Close_0, M, Ms, Opts0)
  ;   throw(E)
  ).


%! http_open2(
%!   +Iri, +State, +Location,
%!   +Stream0, -Stream, -Close_0,
%!   +M, -Ms, +Opts
%! ) is det.

% Authentication error.
http_open2(Iri, State, _, Stream0, Stream, Close_0, M, [M|Ms], Opts) :-
  http_auth_error(M.'llo:status'),
  option(raw_headers(Lines), Opts),
  http_open:parse_headers(Lines, Headers),
  http:authenticate_client(Iri, auth_reponse(Headers, Opts, AuthOpts)), !,
  close(Stream0),
  http_open2(Iri, State, Stream, Close_0, Ms, AuthOpts).
% Non-authentication error.
http_open2(Iri, State, _, Stream0, Stream, Close_0, M, [M|Ms], Opts) :-
  http_error(M.'llo:status'), !,
  call_cleanup(
    deb_http_error(Iri, M.'llo:status', Stream0, Opts),
    close(Stream0)
  ),
  dict_inc(retries, State),
  (   State.retries >= State.max_retries
  ->  Close_0 = true,
      Ms = []
  ;   http_open2(Iri, State, Stream, Close_0, Ms, Opts)
  ).
% Redirect.
http_open2(Iri0, State, Location, Stream0, Stream, Close_0, M, [M|Ms], Opts) :-
  http_redirect(M.'llo:status'), !,
  close(Stream0),
  uri_resolve(Location, Iri0, Iri),
  dict_prepend(visited, State, Iri),
  (   is_redirect_limit_exceeded(State)
  ->  throw_max_redirect_error(Iri, State.max_redirects)
  ;   is_redirect_loop(Iri, State)
  ->  throw_looping_redirect_error(Iri)
  ;   true
  ),
  http_open:redirect_options(Opts, RedirectOpts),
  http_open2(Iri, State, Stream, Close_0, Ms, RedirectOpts).
% Success.
http_open2(_, _, _, Stream, Stream, close(Stream), M, [M], _).





% HELPERS %

%! base_iri(+Source, -BaseIri, +Opts) is det.
% The following option is supported:
%   - base_iri(+atom)
%     Overruling other ways in which the base IRI may be set.

% Overruled by option.
base_iri(_, BaseIri, Opts) :-
  option(base_iri(BaseIri), Opts), !.
% Fail for streams.
base_iri(Stream, _, _) :-
  is_stream(Stream), !,
  fail.
% The IRI that is read from, sans the fragment component.
base_iri(Iri, BaseIri, _) :-
  is_iri(Iri), !,
  % Remove the fragment part, if any.
  uri_components(Iri, uri_components(Scheme,Auth,Path,Query,_)),
  uri_components(BaseIri, uri_components(Scheme,Auth,Path,Query,_)).
% The file is treated as an IRI.
base_iri(File, BaseIri, Opts) :-
  uri_file_name(Iri, File),
  base_iri(Iri, BaseIri, Opts).



%! compression(+Mode, +Compression, +Stream0, -Stream, +M0, -M, +Close0_0, -Close_0) is det.

compression(Mode, Comp, Stream0, Stream, M0, M, Close0_0, Close_0) :-
  compression(Mode, Comp, Stream0, Stream, M0, M),
  (Stream0 == Stream -> Close0_0 = Close_0 ; Close_0 = close(Stream)).


%! compression(+Mode, +Compression, +Stream0, -Stream, +M0, -M) is det.

compression(Mode, Comp, Stream0, Stream, M0, M) :-
  write_mode(Mode), !,
  (   Comp == none
  ->  Stream0 = Stream
  ;   zopen(Stream0, Stream, [close_parent(true),format(Comp)])
  ),
  M = M0.put(_{'llo:compress': Comp}).
compression(Mode, Comp, Stream0, Stream, M0, M) :-
  read_mode(Mode), !,
  (   Comp == none
  ->  Stream0 = Stream
  ;   zopen(Stream0, Stream, [close_parent(true),format(Comp)])
  ),
  M = M0.put(_{'llo:decompress': Comp}).



%! deb_http_error is det.

deb_http_error(Iri, Status, Stream, Opts) :-
  debugging(open_any2(http(error))), !,
  option(raw_headers(Headers), Opts),
  http_error_message(Iri, Status, Headers, Stream).
deb_http_error(_, _, _, _).



%! deb_http_headers(+Lines) is det.

deb_http_headers(Lines) :-
  debugging(open_any2(http(headers))), !,
  maplist(print_http_header0, Lines).
deb_http_headers(_).

print_http_header0(Cs) :-
  string_codes(S, Cs),
  msg_notification("~s~n", [S]).



%! http_error_message(+Iri, +Status, +Lines, +Stream) is det.

http_error_message(Iri, Status, Lines, Stream) :-
  maplist([Cs,Header]>>phrase('header-field'(Header), Cs), Lines, Headers),
  create_grouped_sorted_dict(Headers, http_headers, MHeaders),
  (http_status_label(Status, Label) -> true ; Label = 'NO LABEL'),
  dcg_with_output_to(string(S1), dict(MHeaders, 2)),
  read_input_to_string(Stream, S2),
  msg_warning(
    "HTTP ERROR:~n  Response:~n    ~d (~a)~n  Final IRI:~n    ~a~n  Parsed headers:~n~s~nMessage content:~n~s~n",
    [Status,Label,Iri,S1,S2]
  ).



%! http_parse_headers(+Lines, -Groups, +Opts) is det.

http_parse_headers(Lines, Groups, Opts) :-
  maplist(http_parse_header(Opts), Lines, Pairs),
  keysort(Pairs, SortedPairs),
  group_pairs_by_key(SortedPairs, Groups).


%! http_parse_header(+Opts, +Line, -Pair) is det.

http_parse_header(Opts, Line, Key-Value) :-
  option(parse_headers(true), Opts), !,
  phrase('header-field'(Key-Value), Line).
http_parse_header(_, Line, Key-Value) :-
  phrase(http_parse_header(Key, Value), Line).

http_parse_header(Key3, Val2) -->
  http11:'field-name'(Key1),
  ":", http11:'OWS',
  rest(Val1),
  {
    atom_codes(Key2, Key1),
    atomic_list_concat([llo,Key2], :, Key3),
    string_codes(Val2, Val1)
  }.



%! is_redirect_limit_exceeded(+State) is semidet.

is_redirect_limit_exceeded(State) :-
  State.max_redirects == inf, !,
  fail.
is_redirect_limit_exceeded(State) :-
  length(State.visited, Len),
  Len > State.max_redirects.



%! is_redirect_loop(+Iri, +State) is semidet.

is_redirect_loop(Iri, State) :-
  include(==(Iri), State.visited, Sames),
  length(Sames, NumSames),
  NumSames >= 2.



%! read_mode(+Mode) is semidet.
%! read_mode(-Mode) is multi.
% Mode is one of the following values:
%   - read

read_mode(read).



%! source_numlines(+Source, -NumLines) is det.

source_numlines(Source, N) :-
  catch(
    call_on_stream(Source, {N}/[In,M,M]>>stream_numlines(In, N)),
    error(no_content(_),_),
    N = 0
  ).

stream_numlines(In, N) :-
  stream_numlines(In, 0, N).

stream_numlines(In, M1, N) :-
  read_line_to_codes(In, Cs),
  (   Cs == end_of_file
  ->  N = M1
  ;   M2 is M1 + 1,
      stream_numlines(In, M2, N)
  ).



%! source_type(+Source0, +Mode, -Source, -Type) is nondet.
% Type has either of the following values:
%   - file_iri
%   - http_iri
%   - steam
%   - string

source_type(stream(Stream), _,    Stream, stream  ) :- !.
source_type(string(S),      _,    S,      string  ) :- !.
source_type(Stream,         _,    Stream, stream  ) :- is_stream(Stream), !.
source_type(Iri,            _,    Iri,    file_iri) :- is_file_iri(Iri), !.
source_type(Iri,            _,    Iri,    http_iri) :-
  is_iri(Iri), !,
  uri_components(Iri, uri_components(Scheme,_,_,_,_)),
  (memberchk(Scheme, [http,https]) -> true  ; type_error(http_iri, Iri)).
source_type(File0,          Mode, Iri,    file_iri) :-
  atom(File0),
  absolute_file_name(File0, File, [access(Mode)]),
  is_absolute_file_name(File), !,
  uri_file_name(Iri, File).
source_type(Source, _, _, _) :-
  existence_error(source_sink, Source).



%! stream_metadata(+Stream, -M) is det.

stream_metadata(Stream, M):-
  stream_property(Stream, position(Pos)),
  stream_position_data(byte_count, Pos, NumBytes),
  stream_position_data(char_count, Pos, NumChars),
  stream_position_data(line_count, Pos, NumLines),
  stream_property(Stream, newline(Newline)),
  M = _{
    'llo:byte_count': NumBytes,
    'llo:char_count': NumChars,
    'llo:line_count': NumLines,
    'llo:newline': Newline
  }.



%! throw_looping_redirect_error(+Iri) is det.

throw_looping_redirect_error(Iri) :-
  throw(
    error(
      permission_error(redirect, http, Iri),
      context(_, 'Redirection loop')
    )
  ).



%! throw_max_redirect_error(+Iri, +Max) is det.

throw_max_redirect_error(Iri, Max) :-
  format(atom(Comment), "max_redirect (~w) limit exceeded", [Max]),
  throw(
    error(
      permission_error(redirect, http, Iri),
      context(_, Comment)
    )
  ).



%! write_mode(+Mode) is semidet.
%! write_mode(-Mode) is multi.
% Mode is one of the following values:
%   - append
%   - write

write_mode(append).
write_mode(write).
