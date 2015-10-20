:- module(
  open_any2,
  [
    close_any2/1, % :Close_0
    open_any2/4, % +Input
                 % +Mode:oneof([append,read,write])
                 % -Read:stream
                 % :Close_0
    open_any2/5 % +Input
                % +Mode:oneof([append,read,write])
                % -Read:stream
                % :Close_0
                % +Options:list(compound)
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
:- use_module(library(typecheck)).
:- use_module(library(uri)).

http_open:ssl_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error).

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
%!   -Read:stream,
%!   -Close_0,
%!   +Options:list(compound)
%! ) is det.
% Wrapper around open_any2/5 with default options.

open_any2(In, Mode, Read, Close_0):-
  open_any2(In, Mode, Read, Close_0, []).


%! open_any2(
%!   +Input,
%!   +Mode:oneof([append,read,write]),
%!   -Read:stream,
%!   -Close_0,
%!   +Options:list(compound)
%! ) is det.

open_any2(In0, Mode, Read, Close, Opts):-
  input_type(In0, In, Type),
  ignore(option(metadata(MOpen), Opts)),
  open_any_options(In, Opts, OOpen),
  open_any(In, Mode, Read, Close, Opts),
  (   is_http_error(OOpen)
  ->  memberchk(status_code(StatusCode), OOpen),
      http_status_label(StatusCode, Label),
      throw(
        error(
          permission_error(url,In),
          context(_,status(StatusCode,Label))
        )
      )
  ;   open_any_metadata(In, Type, OOpen, MOpen)
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



%! input_type(+Input0, -Input, -Type:oneof([file,iri,stream,string])) is det.

input_type(stream(Read), Read, stream):- !.
input_type(string(S), S, string):- !.
input_type(Read, Read, stream):-
  is_stream(Read), !.
input_type(Iri, Iri, file):-
  is_file_iri(Iri), !.
input_type(Iri, Iri, iri):-
  is_iri(Iri), !.
input_type(File, Iri, file):-
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
%!   +Type:oneof([file,iri,stream,string]),
%!   +Options:list(compound),
%!   -Metadata:dict
%! ) is det.

open_any_metadata(In, file, _, MOpen):- !,
  base_iri(In, BaseIri),
  MOpen = metadata{base_iri: BaseIri, input_type: file}.
open_any_metadata(In, iri, Opts, MOpen):- !,
  base_iri(In, BaseIri),
  option(final_url(FinalIri), Opts),
  option(headers(Headers), Opts),
  option(status_codes(StatusCode), Opts),
  option(version(Version), Opts),
  create_grouped_sorted_dict(Headers, http_headers, MHeaders),
  MHttp = metadata{
            final_iri: FinalIri,
            headers: MHeaders,
            status_code: StatusCode,
            version: Version
          },
  MOpen = metadata{base_iri: BaseIri, http: MHttp, input_type: iri}.
open_any_metadata(_, Type, _, MOpen):-
  MOpen = metadata{input_type: Type}.



%! open_any_options(
%!   +Type:oneof([file,iri,stream,string]),
%!   +Options:list(compound),
%!   -OpenOptions:list(compound)
%! ) is det.

open_any_options(iri, Opts0, HttpOpts):- !,
  HttpOpts0 = [final_url(_),headers(_),status_code(_),version(_)],
  merge_options(Opts0, HttpOpts0, HttpOpts).
open_any_options(_, Opts, Opts).
