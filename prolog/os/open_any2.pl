:- module(
  open_any2,
  [
    close_any2/1, % +Close_0
    open_any2/4, % +Source, +Mode, -Stream, :Close_0
    open_any2/5, % +Source
                 % +Mode:oneof([append,read,write])
                 % -Stream:stream
                 % -Close_0
                 % :Options:list(compound)
    read_mode/1, % ?Mode:atom
    write_mode/1 % ?Mode:atom
  ]
).

/** <module> Open any v2

Wrapper around library(iostream)'s open_any/5.

@author Wouter Beek
@version 2015/10-2016/01
*/

:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(datetime/datetime)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_cookie)). % HTTP cookie support.
:- use_module(library(http/http_info)).
:- use_module(library(http/http_open)). % HTTP support.
:- use_module(library(http/http_ssl_plugin)). % HTTPS support.
:- use_module(library(http/http11)).
:- use_module(library(option_ext)).
:- use_module(library(iostream)).
:- use_module(library(lists)).
:- use_module(library(msg_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(ssl)). % SSL support.
:- use_module(library(typecheck)).
:- use_module(library(uri)).
:- use_module(library(zlib)).

% @tbd This avoids the following warning, but should it be necessary?
%      ```
%      [HTTP] time: 0.026562sec; message: '$c_call_prolog'/0: Undefined procedure: open_any2:ssl_verify/5
%      ```
:- public
    ssl_verify/5.

ssl_verify(_SSL, _ProblemCertificate, _AllCertificates, _FirstCertificate, _Error).

:- meta_predicate
    close_any2(+),
    open_any2(+,+,-,-),
    open_any2(+,+,-,-,:).

is_meta(http_error).

:- predicate_options(open_any2/5, 5, [
     metadata(-dict),
     pass_to(http_open2/4, 4),
     pass_to(open_any/5, 5),
     pass_to(zopen/3, 3)
   ]).
:- predicate_options(http_open2/4, 4, [
     http_error(+callable),
     metadata(-dict),
     retry(+positive_integer)
   ]).





%! close_any2(+Close_0) is det.
% Synonym of close_any/1 for consistency.

close_any2(Close_0) :-
  close_any(Close_0).



%! open_any2(
%!   +Source,
%!   +Mode:oneof([append,read,write]),
%!   -Stream:stream,
%!   +Close_0
%! ) is det.
% Wrapper around open_any2/5 with default options.

open_any2(Source, Mode, Stream, Close_0) :-
  open_any2(Source, Mode, Stream, Close_0, []).


%! open_any2(
%!   +Source,
%!   +Mode:oneof([append,read,write]),
%!   -Stream:stream,
%!   +Close_0,
%!   :Options:list(compound)
%! ) is nondet.
% The following options are supported:
%   * compress(+oneof([deflate,gzip,none]))
%   * metadata(-dict)
%   * retry(+positive_integer)
%   * Passed to open_any/5.

open_any2(Source0, Mode, Stream, Close_0, Opts0) :-
  meta_options(is_meta, Opts0, Opts1),
  source_type(Source0, Mode, Source, Type),
  ignore(option(metadata(M), Opts1)),
  open_any_options(Type, Opts1, Opts2),

  % We want more support for opening an HTTP IRI stream
  % than what `library(http/http_open)` provides.
  (   Type == http_iri, Mode == read
  ->  http_open2(Source, Stream, Close0_0, Opts2)
  ;   open_any(Source, Mode, Stream0, Close0_0, Opts2)
  ),

  % Compression.
  (   option(compress(Comp), Opts1),
      (   write_mode(Mode)
      ->  must_be(oneof([deflate,gzip]), Comp),
          ZOpts = [format(Comp)]
      ;   ZOpts = []
      ),
      zopen(Stream0, Stream, ZOpts),
      Close_0 = close(Stream)
  ;   Stream = Stream0,
      Close_0 = Close0_0
  ),
  open_any_metadata(Source, Mode, Type, Comp, Opts2, M).


%! http_open2(
%!   +Iri:atom,
%!   +Read:stream,
%!   +Close_0,
%!   +Options:list(compound)
%! ) is det.

http_open2(Iri, Read, Close_0, Opts1) :-
  select_option(retry(N), Opts1, Opts2, 1),
  http_open2(Iri, Read, 0, N, Close_0, Opts2).

http_open2(Iri, Read1, M1, N, Close_0, Opts1) :-
  copy_term(Opts1, Opts2),
  call_time(catch(http_open(Iri, Read2, Opts2), Exception, true), Time),
  (   var(Exception)
  ->  option(status_code(Status), Opts2),
      must_be(ground, Status),
      (   is_http_error(Status)
      ->  option(raw_headers(Headers), Opts2),
          call_cleanup(
            http_error_message(Iri, Status, Headers, Read2),
            close(Read2)
          ),
          M2 is M1 + 1,
          (   M2 =:= N
          ->  Close_0 = true,
              Opts1 = Opts2
          ;   http_open2(Iri, Read1, M2, N, Close_0, Opts1)
          )
      ;   Read1 = Read2,
          Close_0 = close(Read1),
          Opts1 = Opts2
      )
  ;   message_to_string(Exception, S),
      msg_warning("[HTTP] time: ~fsec; message: ~s~n", [Time,S]),
      fail
  ).





% HELPERS %

%! base_iri(+Source, -BaseIri:atom) is det.

% The IRI that is read from, sans the fragment component.
base_iri(Iri, BaseIri) :-
  is_iri(Iri), !,
  % Remove the fragment part, if any.
  uri_components(Iri, uri_components(Scheme,Auth,Path,Query,_)),
  uri_components(BaseIri, uri_components(Scheme,Auth,Path,Query,_)).
% The file is treated as an IRI.
base_iri(File, BaseIri) :-
  uri_file_name(Iri, File),
  base_iri(Iri, BaseIri).



%! http_error_message(
%!   +Iri:atom,
%!   +Status:between(100,599),
%!   +Lines:list(list(code)),
%!   +Read:stream
%! ) is det.

http_error_message(Iri, Status, Lines, Read) :-
  maplist(parse_header, Lines, Headers),
  create_grouped_sorted_dict(Headers, http_headers, M),
  (http_status_label(Status, Label) -> true ; Label = 'NO LABEL'),
  format(
    user_error,
    "HTTP ERROR:~n  Response:~n    ~d (~a)~n  Final IRI:~n    ~a~n  Parsed headers:~n",
    [Status,Label,Iri]
  ),
  print_dict(M, 2),
  nl(user_error),
  format(user_error, "  Message content:~n", []),
  copy_stream_data(Read, user_error),
  nl(user_error).



%! open_any_metadata(
%!   +Source,
%!   +Mode:oneof([append,read,write]),
%!   +Type:oneof([file_iri,http_iri,stream,string]),
%!   +Compress:oneof([deflate,gzip]),
%!   +Options:list(compound),
%!   -Metadata:dict
%! ) is det.

open_any_metadata(Source, Mode, Type, Comp, Opts, M4) :- !,
  % File-type specific.
  (   Type == file_iri
  ->  base_iri(Source, BaseIri),
      M1 = metadata{base_iri: BaseIri, source_type: file_iri}
  ;   Type == http_iri
  ->  base_iri(Source, BaseIri),
      option(final_url(FinalIri), Opts),
      option(raw_headers(Lines), Opts),
      option(status_code(StatusCode), Opts),
      option(version(Version), Opts),
      maplist(parse_header, Lines, Headers),
      create_grouped_sorted_dict(Headers, http_headers, MHeaders),
      exclude(pair_has_var_value, [
        final_iri-FinalIri,
        headers-MHeaders,
        status_code-StatusCode,
        version-Version
      ], MPairs),
      dict_pairs(MHttp, http, MPairs),
      M1 = metadata{base_iri: BaseIri, http: MHttp, source_type: http_iri}
  ;   M1 = metadata{}
  ),

  % Mode.
  put_dict(mode, M1, Mode, M2),

  % Compression.
  (   write_mode(Mode),
      ground(Comp)
  ->  put_dict(compress, M2, Comp, M3)
  ;   M3 = M2
  ),

  % Source type.
  put_dict(source_type, M3, Type, M4).
parse_header(Line, Header) :- phrase('header-field'(Header), Line).



%! open_any_options(
%!   +Type:oneof([file_iri,http_iri,stream,string]),
%!   +Options:list(compound),
%!   -OpenOptions:list(compound)
%! ) is det.

open_any_options(http_iri, Opts1, Opts3) :- !,
  Opts2 = [
    cert_verify_hook(cert_accept_any),
    final_url(_),
    raw_headers(_),
    status_code(_),
    version(_)
  ],
  merge_options(Opts1, Opts2, Opts3).
open_any_options(_, Opts, Opts).



%! read_mode(+Mode:atom) is semidet.
%! read_mode(-Mode:atom) is multi.

read_mode(read).



%! source_type(
%!   +Source0,
%!   +Mode:oneof([append,read,write]),
%!   -Source,
%!   -Type:oneof([file_iri,http_iri,stream,string])
%! ) is nondet.

source_type(stream(Stream), _, Stream, stream) :- !.
source_type(string(S), _, S, string) :- !.
source_type(Stream, _, Stream, stream) :-
  is_stream(Stream), !.
source_type(Iri, _, Iri, file_iri) :-
  is_file_iri(Iri), !.
source_type(Iri, _, Iri, http_iri) :-
  is_http_iri(Iri), !.
source_type(File, _, Iri, file_iri) :-
  is_absolute_file_name(File), !,
  uri_file_name(Iri, File).
source_type(Pattern, Mode, Iri, Type) :-
  absolute_file_name(
    Pattern,
    File,
    [access(Mode),expand(true),file_errors(error),solutions(first)]
  ),
  source_type(File, Mode, Iri, Type).


%! write_mode(+Mode:atom) is semidet.
%! write_mode(-Mode:atom) is multi.

write_mode(append).
write_mode(write).
