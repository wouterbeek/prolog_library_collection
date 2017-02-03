:- module(http_upload, []).

/** <module> HTTP upload file

@author Wouter Beek
@version 2017/02
*/

:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_multipart_plugin)).
:- use_module(library(io)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- http_handler('/file-upload', file_upload_handler, [methods([post])]).

:- public
    save_file/3.

file_upload_handler(Req) :-
  is_multipart_post_request(Req), !,
  http_read_data(Req, Parts, [on_filename(save_file)]),
  memberchk(file=file(FileName,_Saved), Parts),
  reply_json_dict(_{file: FileName}, [status(200)]).
file_upload_handler(_) :-
  throw(http_reply(bad_request(bad_file_upload))).

is_multipart_post_request(Req) :-
  memberchk(method(post), Req),
  memberchk(content_type(Val), Req),
  http_parse_header_value(content_type, Val, media(multipart/'form-data',_)).

save_file(In, file(FileName,File2), Opts) :-
  option(filename(FileName), Opts),
  setup_call_cleanup(
    tmp_file_stream(octet, File1, Out),
    copy_stream_data(In, Out),
    close(Out)
  ),
  atomic_list_concat(Segments, /, FileName),
  last(Segments, Local),
  absolute_file_name(
    q('pack/Vrijheid-Zonder-Maar/resource/img/'),
    Dir,
    [access(write),file_type(directory)]
  ),
  absolute_file_name(Local, File2, [access(write),relative_to(Dir)]),
  rename_file(File1, File2).

:- multifile
    prolog:message//1.

prolog:message(bad_file_upload) -->
  [ "A file upload must be submitted as multipart/form-data using", nl,
    "name=file and providing a file-name"
  ].
