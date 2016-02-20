:- module(
  open_any2,
  [
    close_any2/1, % +Close_0
    open_any2/4,  % +Source, +Mode, -Stream, -Close_0
    open_any2/5,  % +Source, +Mode, -Stream, -Close_0, :Opts
    read_mode/1,  % ?Mode
    write_mode/1  % ?Mode
  ]
).

/** <module> Open any v2

Wrapper around library(iostream)'s open_any/5.

@author Wouter Beek
@version 2015/10-2016/02
*/

:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(date_time/date_time)).
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



%! open_any2(+Source, +Mode, -Stream, -Close_0) is det.
%! open_any2(+Source, +Mode, -Stream, -Close_0, :Opts) is nondet.
% The following options are supported:
%   * compress(+oneof([deflate,gzip,none]))
%   * metadata(-dict)
%   * retry(+positive_integer)
%   * Passed to open_any/5.
%
% @throws existence_error if an HTTP request returns an error code.

open_any2(Source, Mode, Stream, Close_0) :-
  open_any2(Source, Mode, Stream, Close_0, []).

open_any2(Source1, Mode, Stream2, Close2_0, Opts1) :-
  meta_options(is_meta, Opts1, Opts2),
  source_type(Source1, Mode, Source2, Type),
  ignore(option(metadata(D), Opts2)),
  open_any_options(Type, Opts2, Opts3),

  % We want more support for opening an HTTP IRI stream
  % than what `library(http/http_open)` provides.
  (   Type == http_iri, Mode == read
  ->  http_open2(Source2, Stream1, Close1_0, Opts3)
  ;   open_any(Source2, Mode, Stream1, Close1_0, Opts3)
  ),

  % Compression.
  (   (   write_mode(Mode),
          option(compress(Comp), Opts2)
      ->  ZOpts1 = [format(Comp)]
      ;   read_mode(Mode),
          option(decompress(Decomp), Opts2)
      ->  ZOpts1 = [format(Decomp)]
      )
  ->  merge_options(ZOpts1, [close_parent(true)], ZOpts2),
      zopen(Stream1, Stream2, ZOpts2),
      Close2_0 = close(Stream2)
  ;   Stream2 = Stream1,
      Close2_0 = Close1_0
  ),
  open_any_metadata(Source2, Mode, Type, Comp, Opts3, D),
  (   get_dict('llo:status_code', D, Status),
      is_http_error(Status)
  ->  existence_error(open_any2, D)
  ;   true
  ).


%! http_open2(+Iri, +Read, +Close_0, +Opts) is det.

http_open2(Iri, Read, Close_0, Opts1) :-
  select_option(retry(N), Opts1, Opts2, 1),
  http_open2(Iri, Read, 0, N, Close_0, Opts2).

http_open2(Iri, Read1, M1, N, Close_0, Opts1) :-
  copy_term(Opts1, Opts2),
  call_time(catch(http_open(Iri, Read2, Opts2), E, true), Time),
  (   var(E)
  ->  option(status_code(Status), Opts2),
      option(time(Time), Opts2),
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
  ;   throw(E)
  ).





% HELPERS %

%! base_iri(+Source, -BaseIri) is det.

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



%! http_error_message(+Iri, +Status, +Lines:list(list(code)), +Read) is det.

http_error_message(Iri, Status, Lines, Read) :-
  maplist([Cs,Header]>>phrase('header-field'(Header), Cs), Lines, Headers),
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



%! open_any_metadata(+Source, +Mode, +Type, +Compress, +Opts, -Metadata) is det.

open_any_metadata(Source, Mode1, Type1, Comp, Opts, D4) :-
  atomic_list_concat([llo,Type1], :, Type2),
  D1 = _{'@type': Type2},
  (   Type1 == file_iri
  ->  base_iri(Source, BaseIri),
      D2 = D1.put(_{'llo:base_iri': _{'@type': 'xsd:anyURI', '@value': BaseIri}})
  ;   Type1 == http_iri
  ->  base_iri(Source, BaseIri),
      option(final_url(FinalIri), Opts),
      option(raw_headers(Lines), Opts),
      option(status_code(Status), Opts),
      option(time(Time), Opts),
      option(version(Major-Minor), Opts),
      maplist([Cs,Header]>>phrase('header-field'(Header), Cs), Lines, L0),
      create_grouped_sorted_dict(L0, D01),
      D02 = D1.put(_{
        'llo:base_iri': _{'@type': 'xsd:anyURI', '@value': BaseIri},
        'llo:final_iri': _{'@type': 'xsd:anyURI', '@value': FinalIri},
        'llo:status_code': Status,
        'llo:time': _{'@type': 'xsd:float', '@value': Time},
        'llo:version': _{
          '@type': 'llo:Version',
          'llo:major': _{'@type': 'xsd:nonNegativeInteger', '@value': Major},
          'llo:minor': _{'@type': 'xsd:nonNegativeInteger', '@value': Minor}
        }
      }),
      D2 = D02.put(D01)
  ;   D2 = D1
  ),
  atomic_list_concat([llo,Mode1], :, Mode2),
  D3 = D2.put(_{'llo:mode': Mode2}),
  (   write_mode(Mode1),
      ground(Comp)
  ->  D4 = D3.put(_{'llo:compression': Comp})
  ;   D4 = D3
  ).



%! open_any_options(+Type, +Opts, -OpenOpts) is det.

open_any_options(http_iri, Opts1, Opts3) :- !,
  Opts2 = [
    cert_verify_hook(cert_accept_any),
    final_url(_),
    raw_headers(_),
    status_code(_),
    time(_),
    version(_)
  ],
  merge_options(Opts1, Opts2, Opts3).
open_any_options(_, Opts, Opts).



%! read_mode(+Mode) is semidet.
%! read_mode(-Mode) is multi.
% Mode is one of the following values:
%   - read

read_mode(read).



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
source_type(Iri,            _,    Iri,    http_iri) :- is_http_iri(Iri), !.
source_type(File,           _,    Iri,    file_iri) :-
  is_absolute_file_name(File), !,
  uri_file_name(Iri, File).
source_type(Pattern, Mode, Iri, Type) :-
  Opts = [access(Mode),expand(true),file_errors(error),solutions(first)],
  absolute_file_name(Pattern, File, Opts),
  source_type(File, Mode, Iri, Type).



%! write_mode(+Mode) is semidet.
%! write_mode(-Mode) is multi.
% Mode is one of the following values:
%   - append
%   - write

write_mode(append).
write_mode(write).
