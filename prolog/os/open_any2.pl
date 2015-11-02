:- module(
  open_any2,
  [
    close_any2/1, % :Close_0
    open_any2/4, % +Input
                 % +Mode:oneof([append,read,write])
                 % -Stream:stream
                 % :Close_0
    open_any2/5, % +Input
                 % +Mode:oneof([append,read,write])
                 % -Stream:stream
                 % :Close_0
                 % +Options:list(compound)
    read_mode/1, % ?Mode:atom
    write_mode/1 % ?Mode:atom
  ]
).

/** <module> Open any v2

Wrapper around library(iostream) that adds a metadata option
to open_any/5.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(dict_ext)).
:- use_module(library(http/http_cookie)).
:- use_module(library(http/http_deb)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(option)).
:- use_module(library(iostream)).
:- use_module(library(ssl)).
:- use_module(library(typecheck)).
:- use_module(library(uri)).
:- use_module(library(zlib)).

:- predicate_options(open_any2/5, 5, [
     metadata(-dict),
     pass_to(open_any/5, 5)
   ]).





%! close_any2(:Close_0) is det.

close_any2(Close_0):-
  close_any(Close_0).



%! open_any2(
%!   +Input,
%!   +Mode:oneof([append,read,write]),
%!   -Stream:stream,
%!   -Close_0,
%!   +Options:list(compound)
%! ) is det.
% Wrapper around open_any2/5 with default options.

open_any2(In, Mode, Stream, Close_0):-
  open_any2(In, Mode, Stream, Close_0, []).


%! open_any2(
%!   +Input,
%!   +Mode:oneof([append,read,write]),
%!   -Stream:stream,
%!   -Close_0,
%!   +Options:list(compound)
%! ) is det.

open_any2(In0, Mode, Stream, Close_0, Opts1):-
  input_type(In0, In, Type),
  ignore(option(metadata(M), Opts1)),
  open_any_options(Type, Opts1, Opts2),
  open_any(In, Mode, Stream0, Close_0, Opts2),

  % Compression.
  (   option(compress(Comp), Opts1),
      write_mode(Mode),
      must_be(oneof([deflate,gzip]), Comp)
  ->  zopen(Stream0, Stream, [format(Comp)])
  ;   Stream = Stream0
  ),

  % HTTP error.
  (   is_http_error(Opts2)
  ->  memberchk(status_code(StatusCode), Opts2),
      http_status_label(StatusCode, Label),
      throw(
        error(
          permission_error(url,In),
          context(_,status(StatusCode,Label))
        )
      )
  ;   open_any_metadata(In, Mode, Type, Comp, Opts2, M)
  ).





% HELPERS %

%! base_iri(+Input, -BaseIri:atom) is det.

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



%! input_type(+Input0, -Input, -Type:oneof([file_iri,http_iri,stream,string])) is det.

input_type(stream(Stream), Stream, stream):- !.
input_type(string(S), S, string):- !.
input_type(Stream, Stream, stream):-
  is_stream(Stream), !.
input_type(Iri, Iri, file_iri):-
  is_file_iri(Iri), !.
input_type(Iri, Iri, http_iri):-
  is_iri(Iri), !.
input_type(File, Iri, file_iri):-
  uri_file_name(Iri, File).



%! is_http_error(+Options:list(compound)) is semidet.
% Succeeds if Options contains an HTTP status code
% that describes an error condition.

is_http_error(Opts):-
  option(status_code(StatusCode), Opts),
  ground(StatusCode),
  between(400, 599, StatusCode).



%! open_any_metadata(
%!   +Input,
%!   +Mode:oneof([append,read,write]),
%!   +Type:oneof([file_iri,http_iri,stream,string]),
%!   +Compress:oneof([deflate,gzip]),
%!   +Options:list(compound),
%!   -Metadata:dict
%! ) is det.

open_any_metadata(In, Mode, Type, Comp, Opts, M4):- !,
  % File-type specific.
  (   Type == file_iri
  ->  base_iri(In, BaseIri),
      M1 = metadata{base_iri: BaseIri, input_type: file_iri}
  ;   Type == http_iri
  ->  base_iri(In, BaseIri),
      option(final_url(FinalIri), Opts),
      option(headers(Headers), Opts),
      option(status_code(StatusCode), Opts),
      option(version(Version), Opts),
      create_grouped_sorted_dict(Headers, http_headers, MHeaders),
      MHttp = metadata{
                final_iri: FinalIri,
                headers: MHeaders,
                status_code: StatusCode,
                version: Version
              },
      M1 = metadata{base_iri: BaseIri, http: MHttp, input_type: http_iri}
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

  % Input type.
  put_dict(input_type, M3, Type, M4).



%! open_any_options(
%!   +Type:oneof([file_iri,http_iri,stream,string]),
%!   +Options:list(compound),
%!   -OpenOptions:list(compound)
%! ) is det.

open_any_options(http_iri, Opts1, Opts2):- !,
  HttpOpts = [
    cert_verify_hook(cert_accept_any),
    final_url(_),
    headers(_),
    status_code(_),
    version(_)
  ],
  merge_options(Opts1, HttpOpts, Opts2).
open_any_options(_, Opts, Opts).



%! read_mode(+Mode:atom) is semidet.
%! read_mode(-Mode:atom) is multi.

read_mode(read).



%! write_mode(+Mode:atom) is semidet.
%! write_mode(-Mode:atom) is multi.

write_mode(append).
write_mode(write).
