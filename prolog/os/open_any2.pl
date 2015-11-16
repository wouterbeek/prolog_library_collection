:- module(
  open_any2,
  [
    close_any2/1, % :Close_0
    open_any2/4, % +Source, +Mode, -Stream, :Close_0
    open_any2/5, % +Source
                 % +Mode:oneof([append,read,write])
                 % -Stream:stream
                 % :Close_0
                 % :Options:list(compound)
    read_mode/1, % ?Mode:atom
    write_mode/1 % ?Mode:atom
  ]
).

/** <module> Open any v2

Wrapper around library(iostream) that adds a metadata option
to open_any/5.

@author Wouter Beek
@version 2015/10-2015/11
*/

:- use_module(library(apply)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_cookie)). % HTTP cookie support.
:- use_module(library(http/http_info)).
:- use_module(library(http/http_open)). % HTTP support.
:- use_module(library(http/http_ssl_plugin)). % HTTPS support.
:- use_module(library(http/rfc2616_header)).
:- use_module(library(option_ext)).
:- use_module(library(iostream)).
:- use_module(library(lists)).
:- use_module(library(ssl)). % SSL support.
:- use_module(library(typecheck)).
:- use_module(library(uri)).
:- use_module(library(zlib)).

:- meta_predicate(close_any2(0)).
:- meta_predicate(http_open2(+,+,0,:)).
:- meta_predicate(open_any2(+,+,-,0)).
:- meta_predicate(open_any2(+,+,-,0,:)).

is_meta(http_error).

:- predicate_options(open_any2/5, 5, [
     metadata(-dict),
     pass_to(http_open2/4, 4),
     pass_to(open_any/5, 5),
     pass_to(zopen/3, 3)
   ]).
:- predicate_options(http_open2/4, 4, [
     http_error(+callable),
     metadata(-dict)
   ]).





%! close_any2(:Close_0) is det.
% Synonym of close_any/1 for consistency.

close_any2(Close_0):-
  close_any(Close_0).



%! open_any2(
%!   +Source,
%!   +Mode:oneof([append,read,write]),
%!   -Stream:stream,
%!   -Close_0
%! ) is det.
% Wrapper around open_any2/5 with default options.

open_any2(Source, Mode, Stream, Close_0):-
  open_any2(Source, Mode, Stream, Close_0, []).


%! open_any2(
%!   +Source,
%!   +Mode:oneof([append,read,write]),
%!   -Stream:stream,
%!   -Close_0,
%!   :Options:list(compound)
%! ) is nondet.

open_any2(Source0, Mode, Stream, Close_0, Opts0):-
  meta_options(is_meta, Opts0, Opts1),
  source_type(Source0, Source, Type),
  ignore(option(metadata(M), Opts1)),
  open_any_options(Type, Opts1, Opts2),

  % We want more support for opening an HTTP IRI stream
  % than what `library(http/http_open)` provides.
  (   Type == http_iri, Mode == read
  ->  Close_0 = close(Stream),
	    http_open2(Source, Stream, Close_0, Opts2)
  ;   open_any(Source, Mode, Stream0, Close_0, Opts2)
  ),

  % Compression.
  (   option(compress(Comp), Opts1),
      (   write_mode(Mode)
      ->  must_be(oneof([deflate,gzip]), Comp),
          ZOpts = [format(Comp)]
      ;   ZOpts = []
      ),
      zopen(Stream0, Stream, ZOpts)
  ;   Stream = Stream0
  ),

  % HTTP error.
  (   is_http_error0(Opts2)
  ->  memberchk(status_code(Status), Opts2),
      http_status_label(Status, Label),
      throw(
        error(
          permission_error(url,Source),
          context(_,status(Status,Label))
        )
      )
  ;   open_any_metadata(Source, Mode, Type, Comp, Opts2, M)
  ).


%! http_open2(
%!   +Iri:atom,
%!   +Read:stream,
%!   :Close_0,
%!   :Options:list(compound)
%! ) is det.

http_open2(Iri, Read, Close_0, Opts):-
  copy_term(Opts, OptsCp),
  http_open(Iri, Read, OptsCp),
  option(metadata(M), OptsCp),
  (   between(200, 299, M.http.status_code),
      option(metadata(M), Opts)
  ->  true
  ;   option(http_error(Error_2), Opts, http_error_default),
      call_cleanup(call(Error_2, M, Read), Close_0),
      http_open2(Iri, Read, Close_0, Opts)
  ).





% HELPERS %

%! base_iri(+Source, -BaseIri:atom) is det.

% The IRI that is read from, sans the fragment component.
base_iri(Iri, BaseIri):-
  is_iri(Iri), !,
  % Remove the fragment part, if any.
  uri_components(Iri, uri_components(Scheme,Auth,Path,Query,_)),
  uri_components(BaseIri, uri_components(Scheme,Auth,Path,Query,_)).
% The file is treated as an IRI.
base_iri(File, BaseIri):-
  uri_file_name(Iri, File),
  base_iri(Iri, BaseIri).



%! http_error_default(+Metadata:dict, +Read:stream) is det.

http_error_default(M, Read):-
  format(user_error, "RESPONSE: ~D~n", [M.http.status_code]),
  dict_pairs(M.http.headers, http_headers, ResHs),
  maplist(print_header(user_error), ResHs),
  copy_stream_data(Read, user_error),
  nl(user_error).



%! is_http_error0(+Options:list(compound)) is semidet.
% Succeeds if Options contains an HTTP status code
% that describes an error condition.

is_http_error0(Opts):-
  option(status_code(Status), Opts),
  ground(Status),
  is_http_error(Status).



%! open_any_metadata(
%!   +Source,
%!   +Mode:oneof([append,read,write]),
%!   +Type:oneof([file_iri,http_iri,stream,string]),
%!   +Compress:oneof([deflate,gzip]),
%!   +Options:list(compound),
%!   -Metadata:dict
%! ) is det.

open_any_metadata(Source, Mode, Type, Comp, Opts, M4):- !,
  % File-type specific.
  (   Type == file_iri
  ->  base_iri(Source, BaseIri),
      M1 = metadata{base_iri: BaseIri, source_type: file_iri}
  ;   Type == http_iri
  ->  base_iri(Source, BaseIri),
      option(final_url(FinalIri), Opts),
      option(headers(Headers1), Opts),
      option(status_code(StatusCode), Opts),
      option(version(Version), Opts),
      %maplist(http_parsed_header_pair, Headers1, Headers2),
      maplist(option_pair, Headers1, Headers2),
      create_grouped_sorted_dict(Headers2, http_headers, MHeaders),
      MHttp = metadata{
                final_iri: FinalIri,
                headers: MHeaders,
                status_code: StatusCode,
                version: Version
              },
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



%! open_any_options(
%!   +Type:oneof([file_iri,http_iri,stream,string]),
%!   +Options:list(compound),
%!   -OpenOptions:list(compound)
%! ) is det.

open_any_options(http_iri, Opts1, Opts3):- !,
  Opts2 = [
    cert_verify_hook(cert_accept_any),
    final_url(_),
    headers(_),
    status_code(_),
    version(_)
  ],
  merge_options(Opts1, Opts2, Opts3).
open_any_options(_, Opts, Opts).



%! print_header(+Write:stream, +Header:pair) is det.

print_header(Write, N-V):-
  format(Write, "~w=~w~n", [N,V]).



%! read_mode(+Mode:atom) is semidet.
%! read_mode(-Mode:atom) is multi.

read_mode(read).



%! source_type(
%!   +Source0,
%!   -Source,
%!   -Type:oneof([file_iri,http_iri,stream,string])
%! ) is nondet.

source_type(stream(Stream), Stream, stream):- !.
source_type(string(S), S, string):- !.
source_type(Stream, Stream, stream):-
  is_stream(Stream), !.
source_type(Iri, Iri, file_iri):-
  is_file_iri(Iri), !.
source_type(Iri, Iri, http_iri):-
  is_http_iri(Iri), !.
source_type(File, Iri, file_iri):-
  is_absolute_file_name(File), !,
  uri_file_name(Iri, File).
source_type(Pattern, Iri, Type):-
  expand_file_name(Pattern, Files),
  % NONDET
  member(File, Files),
  source_type(File, Iri, Type).


%! write_mode(+Mode:atom) is semidet.
%! write_mode(-Mode:atom) is multi.

write_mode(append).
write_mode(write).
