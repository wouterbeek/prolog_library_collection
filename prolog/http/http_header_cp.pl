/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2020, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(http_header_cp,
          [ http_read_request/2,        % +Stream, -Request
            http_read_reply_header/2,   % +Stream, -Reply
            http_reply/2,               % +What, +Stream
            http_reply/3,               % +What, +Stream, +HdrExtra
            http_reply/4,               % +What, +Stream, +HdrExtra, -Code
            http_reply/5,               % +What, +Stream, +HdrExtra, +Context,
                                        % -Code
            http_reply/6,               % +What, +Stream, +HdrExtra, +Context,
                                        % +Request, -Code
            http_reply_header/3,        % +Stream, +What, +HdrExtra
            http_status_reply/4,        % +Status, +Out, +HdrExtra, -Code
            http_status_reply/5,        % +Status, +Out, +HdrExtra,
                                        % +Context, -Code

            http_timestamp/2,           % +Time, -HTTP string

            http_post_data/3,           % +Stream, +Data, +HdrExtra

            http_read_header/2,         % +Fd, -Header
            http_parse_header/2,        % +Codes, -Header
            http_parse_header_value/3,  % +Header, +HeaderValue, -MediaTypes
            http_join_headers/3,        % +Default, +InHdr, -OutHdr
            http_update_encoding/3,     % +HeaderIn, -Encoding, -HeaderOut
            http_update_connection/4,   % +HeaderIn, +Request, -Connection, -HeaderOut
            http_update_transfer/4      % +HeaderIn, +Request, -Transfer, -HeaderOut
          ]).
:- autoload(library(http/html_write),
	    [ print_html/2, print_html/1, page/4, html/3,
	      html_print_length/2
	    ]).
:- autoload(library(http/http_exception),[map_exception_to_http_status/4]).
:- autoload(library(http/mimepack),[mime_pack/3]).
:- autoload(library(http/mimetype),[file_mime_type/2]).
:- autoload(library(apply),[maplist/2]).
:- autoload(library(base64),[base64/2]).
:- autoload(library(debug),[debug/3,debugging/1]).
:- autoload(library(error),[syntax_error/1,domain_error/2]).
:- autoload(library(lists),[append/3,member/2,select/3,delete/3]).
:- autoload(library(memfile),
	    [ new_memory_file/1, open_memory_file/3,
	      free_memory_file/1, open_memory_file/4,
	      size_memory_file/3
	    ]).
:- autoload(library(option),[option/3,option/2]).
:- autoload(library(pairs),[pairs_values/2]).
:- autoload(library(readutil),
	    [read_line_to_codes/2,read_line_to_codes/3]).
:- autoload(library(sgml_write),[xml_write/3]).
:- autoload(library(socket),[gethostname/1]).
:- autoload(library(uri),
	    [ uri_components/2, uri_data/3, uri_encoded/3, uri_query_components/2
	    ]).
:- autoload(library(url),[parse_url_search/2]).
:- autoload(library(dcg/basics),
	    [ integer/3, atom/3, whites/2, blanks_to_nl/2, string/3,
	      number/3, blanks/2, float/3, nonblanks/3, eos/2
	    ]).
:- use_module(library(settings),[setting/4,setting/2]).

:- use_module(library(media_type), []).

:- multifile
    http:status_page/3,             % +Status, +Context, -HTML
    http:status_reply/3,            % +Status, -Reply, +Options
    http:serialize_reply/2,         % +Reply, -Body
    http:post_data_hook/3.          % +Data, +Out, +HdrExtra

% see http_update_transfer/4.

:- setting(http:chunked_transfer, oneof([never,on_request,if_possible]),
           on_request, 'When to use Transfer-Encoding: Chunked').


/** <module> Handling HTTP headers

The library library(http/http_header) provides   primitives  for parsing
and composing HTTP headers. Its functionality  is normally hidden by the
other parts of the HTTP server and client libraries.
*/

:- discontiguous
    term_expansion/2.


                 /*******************************
                 *          READ REQUEST        *
                 *******************************/

%!  http_read_request(+FdIn:istream, -Request) is det.
%
%   Read an HTTP request-header from FdIn and return the broken-down
%   request fields as +Name(+Value) pairs  in   a  list.  Request is
%   unified to =end_of_file= if FdIn is at the end of input.

http_read_request(In, Request) :-
    catch(read_line_to_codes(In, Codes), E, true),
    (   var(E)
    ->  (   Codes == end_of_file
        ->  debug(http(header), 'end-of-file', []),
            Request = end_of_file
        ;   debug(http(header), 'First line: ~s', [Codes]),
            Request =  [input(In)|Request1],
            phrase(request(In, Request1), Codes),
            (   Request1 = [unknown(Text)|_]
            ->  string_codes(S, Text),
                syntax_error(http_request(S))
            ;   true
            )
        )
    ;   (   debugging(http(request))
        ->  message_to_string(E, Msg),
            debug(http(request), "Exception reading 1st line: ~s", [Msg])
        ;   true
        ),
        Request = end_of_file
    ).


%!  http_read_reply_header(+FdIn, -Reply)
%
%   Read the HTTP reply header. Throws   an exception if the current
%   input does not contain a valid reply header.

http_read_reply_header(In, [input(In)|Reply]) :-
    read_line_to_codes(In, Codes),
    (   Codes == end_of_file
    ->  debug(http(header), 'end-of-file', []),
        throw(error(syntax(http_reply_header, end_of_file), _))
    ;   debug(http(header), 'First line: ~s~n', [Codes]),
        (   phrase(reply(In, Reply), Codes)
        ->  true
        ;   atom_codes(Header, Codes),
            syntax_error(http_reply_header(Header))
        )
    ).


                 /*******************************
                 *        FORMULATE REPLY       *
                 *******************************/

%!  http_reply(+Data, +Out:ostream) is det.
%!  http_reply(+Data, +Out:ostream, +HdrExtra) is det.
%!  http_reply(+Data, +Out:ostream, +HdrExtra, -Code) is det.
%!  http_reply(+Data, +Out:ostream, +HdrExtra, +Context, -Code) is det.
%!  http_reply(+Data, +Out:ostream, +HdrExtra, +Context, +Request, -Code) is det.
%
%   Compose  a  complete  HTTP  reply  from   the  term  Data  using
%   additional headers from  HdrExtra  to   the  output  stream Out.
%   ExtraHeader is a list of Field(Value). Data is one of:
%
%           * html(HTML)
%           HTML tokens as produced by html//1 from html_write.pl
%
%           * file(+MimeType, +FileName)
%           Reply content of FileName using MimeType
%
%           * file(+MimeType, +FileName, +Range)
%           Reply partial content of FileName with given MimeType
%
%           * tmp_file(+MimeType, +FileName)
%           Same as =file=, but do not include modification time
%
%           * bytes(+MimeType, +Bytes)
%           Send a sequence of Bytes with the indicated MimeType.
%           Bytes is either a string of character codes 0..255 or
%           list of integers in the range 0..255. Out-of-bound codes
%           result in a representation error exception.
%
%           * stream(+In, +Len)
%           Reply content of stream.
%
%           * cgi_stream(+In, +Len)
%           Reply content of stream, which should start with an
%           HTTP header, followed by a blank line.  This is the
%           typical output from a CGI script.
%
%           * Status
%           HTTP status report as defined by http_status_reply/4.
%
%   @param HdrExtra provides additional reply-header fields, encoded
%          as Name(Value). It can also contain a field
%          content_length(-Len) to _retrieve_ the
%          value of the Content-length header that is replied.
%   @param Code is the numeric HTTP status code sent
%
%   @tbd    Complete documentation

http_reply(What, Out) :-
    http_reply(What, Out, [connection(close)], _).

http_reply(Data, Out, HdrExtra) :-
    http_reply(Data, Out, HdrExtra, _Code).

http_reply(Data, Out, HdrExtra, Code) :-
    http_reply(Data, Out, HdrExtra, [], Code).

http_reply(Data, Out, HdrExtra, Context, Code) :-
    http_reply(Data, Out, HdrExtra, Context, [method(get)], Code).

http_reply(Data, Out, HdrExtra, _Context, Request, Code) :-
    byte_count(Out, C0),
    memberchk(method(Method), Request),
    catch(http_reply_data(Data, Out, HdrExtra, Method, Code), E, true),
    !,
    (   var(E)
    ->  true
    ;   (   E = error(io_error(write,_), _)
        ;   E = error(socket_error(_,_), _)
        )
    ->  byte_count(Out, C1),
        Sent is C1 - C0,
        throw(error(http_write_short(Data, Sent), _))
    ;   E = error(timeout_error(write, _), _)
    ->  throw(E)
    ;   map_exception_to_http_status(E, Status, NewHdr, NewContext),
        http_status_reply(Status, Out, NewHdr, NewContext, Request, Code)
    ).
http_reply(Status, Out, HdrExtra, Context, Request, Code) :-
    http_status_reply(Status, Out, HdrExtra, Context, Request, Code).

:- meta_predicate
    if_no_head(0, +).

%!  http_reply_data(+Data, +Out, +HdrExtra, +Method, -Code) is semidet.
%
%   Fails if Data is not a defined   reply-data format, but a status
%   term. See http_reply/3 and http_status_reply/6.
%
%   @error Various I/O errors.

http_reply_data(Data, Out, HdrExtra, Method, Code) :-
    http_reply_data_(Data, Out, HdrExtra, Method, Code),
    flush_output(Out).

http_reply_data_(html(HTML), Out, HdrExtra, Method, Code) :-
    !,
    phrase(reply_header(html(HTML), HdrExtra, Code), Header),
    debug(http(send_reply), "< ~s", [Header]),
    format(Out, '~s', [Header]),
    if_no_head(print_html(Out, HTML), Method).
http_reply_data_(file(Type, File), Out, HdrExtra, Method, Code) :-
    !,
    phrase(reply_header(file(Type, File), HdrExtra, Code), Header),
    reply_file(Out, File, Header, Method).
http_reply_data_(gzip_file(Type, File), Out, HdrExtra, Method, Code) :-
    !,
    phrase(reply_header(gzip_file(Type, File), HdrExtra, Code), Header),
    reply_file(Out, File, Header, Method).
http_reply_data_(file(Type, File, Range), Out, HdrExtra, Method, Code) :-
    !,
    phrase(reply_header(file(Type, File, Range), HdrExtra, Code), Header),
    reply_file_range(Out, File, Header, Range, Method).
http_reply_data_(tmp_file(Type, File), Out, HdrExtra, Method, Code) :-
    !,
    phrase(reply_header(tmp_file(Type, File), HdrExtra, Code), Header),
    reply_file(Out, File, Header, Method).
http_reply_data_(bytes(Type, Bytes), Out, HdrExtra, Method, Code) :-
    !,
    phrase(reply_header(bytes(Type, Bytes), HdrExtra, Code), Header),
    debug(http(send_reply), "< ~s", [Header]),
    format(Out, '~s', [Header]),
    if_no_head(format(Out, '~s', [Bytes]), Method).
http_reply_data_(stream(In, Len), Out, HdrExtra, Method, Code) :-
    !,
    phrase(reply_header(cgi_data(Len), HdrExtra, Code), Header),
    copy_stream(Out, In, Header, Method, 0, end).
http_reply_data_(cgi_stream(In, Len), Out, HdrExtra, Method, Code) :-
    !,
    http_read_header(In, CgiHeader),
    seek(In, 0, current, Pos),
    Size is Len - Pos,
    http_join_headers(HdrExtra, CgiHeader, Hdr2),
    phrase(reply_header(cgi_data(Size), Hdr2, Code), Header),
    copy_stream(Out, In, Header, Method, 0, end).

if_no_head(_, head) :-
    !.
if_no_head(Goal, _) :-
    call(Goal).

reply_file(Out, _File, Header, head) :-
    !,
    debug(http(send_reply), "< ~s", [Header]),
    format(Out, '~s', [Header]).
reply_file(Out, File, Header, _) :-
    setup_call_cleanup(
        open(File, read, In, [type(binary)]),
        copy_stream(Out, In, Header, 0, end),
        close(In)).

reply_file_range(Out, _File, Header, _Range, head) :-
    !,
    debug(http(send_reply), "< ~s", [Header]),
    format(Out, '~s', [Header]).
reply_file_range(Out, File, Header, bytes(From, To), _) :-
    setup_call_cleanup(
        open(File, read, In, [type(binary)]),
        copy_stream(Out, In, Header, From, To),
        close(In)).

copy_stream(Out, _, Header, head, _, _) :-
    !,
    debug(http(send_reply), "< ~s", [Header]),
    format(Out, '~s', [Header]).
copy_stream(Out, In, Header, _, From, To) :-
    copy_stream(Out, In, Header, From, To).

copy_stream(Out, In, Header, From, To) :-
    (   From == 0
    ->  true
    ;   seek(In, From, bof, _)
    ),
    peek_byte(In, _),
    debug(http(send_reply), "< ~s", [Header]),
    format(Out, '~s', [Header]),
    (   To == end
    ->  copy_stream_data(In, Out)
    ;   Len is To - From,
        copy_stream_data(In, Out, Len)
    ).


%!  http_status_reply(+Status, +Out, +HdrExtra, -Code) is det.
%!  http_status_reply(+Status, +Out, +HdrExtra, +Context, -Code) is det.
%!  http_status_reply(+Status, +Out, +HdrExtra, +Context, +Request, -Code) is det.
%
%   Emit HTML non-200 status reports. Such  requests are always sent
%   as UTF-8 documents.
%
%   Status can be one of the following:
%      - authorise(Method)
%        Challenge authorization.  Method is one of
%        - basic(Realm)
%        - digest(Digest)
%      - authorise(basic,Realm)
%        Same as authorise(basic(Realm)).  Deprecated.
%      - bad_request(ErrorTerm)
%      - busy
%      - created(Location)
%      - forbidden(Url)
%      - moved(To)
%      - moved_temporary(To)
%      - no_content
%      - not_acceptable(WhyHtml)
%      - not_found(Path)
%      - method_not_allowed(Method, Path)
%      - not_modified
%      - resource_error(ErrorTerm)
%      - see_other(To)
%      - switching_protocols(Goal,Options)
%      - server_error(ErrorTerm)
%      - unavailable(WhyHtml)

http_status_reply(Status, Out, Options) :-
    _{header:HdrExtra, context:Context, code:Code, method:Method} :< Options,
    http_status_reply(Status, Out, HdrExtra, Context, [method(Method)], Code).

http_status_reply(Status, Out, HdrExtra, Code) :-
    http_status_reply(Status, Out, HdrExtra, [], Code).

http_status_reply(Status, Out, HdrExtra, Context, Code) :-
    http_status_reply(Status, Out, HdrExtra, Context, [method(get)], Code).

http_status_reply(Status, Out, HdrExtra, Context, Request, Code) :-
    option(method(Method), Request, get),
    parsed_accept(Request, Accept),
    status_reply_flush(Status, Out,
                       _{ context: Context,
                          method:  Method,
                          code:    Code,
                          accept:  Accept,
                          header:  HdrExtra
                        }).

parsed_accept(Request, Accept) :-
    memberchk(accept(Accept0), Request),
    http_parse_header_value(accept, Accept0, Accept1),
    !,
    Accept = Accept1.
parsed_accept(_, [ media(text/html, [], 0.1,  []),
                   media(_,         [], 0.01, [])
                 ]).

status_reply_flush(Status, Out, Options) :-
    status_reply(Status, Out, Options),
    !,
    flush_output(Out).

%!  status_reply(+Status, +Out, +Options:options)
%
%   Formulate a non-200 reply and send it to the stream Out.  Options
%   is a dict containing:
%
%     - header
%     - context
%     - method
%     - code
%     - accept

% Replies without content
status_reply(no_content, Out, Options) :-
    !,
    phrase(reply_header(status(no_content), Options), Header),
    debug(http(send_reply), "< ~s", [Header]),
    format(Out, '~s', [Header]).
status_reply(switching_protocols(_Goal,SwitchOptions), Out, Options) :-
    !,
    (   option(headers(Extra1), SwitchOptions)
    ->  true
    ;   option(header(Extra1), SwitchOptions, [])
    ),
    http_join_headers(Options.header, Extra1, HdrExtra),
    phrase(reply_header(status(switching_protocols),
                        Options.put(header,HdrExtra)), Header),
    debug(http(send_reply), "< ~s", [Header]),
    format(Out, '~s', [Header]).
status_reply(authorise(basic, ''), Out, Options) :-
    !,
    status_reply(authorise(basic), Out, Options).
status_reply(authorise(basic, Realm), Out, Options) :-
    !,
    status_reply(authorise(basic(Realm)), Out, Options).
status_reply(not_modified, Out, Options) :-
    !,
    phrase(reply_header(status(not_modified), Options), Header),
    debug(http(send_reply), "< ~s", [Header]),
    format(Out, '~s', [Header]).
% aliases (compatibility)
status_reply(busy, Out, Options) :-
    status_reply(service_unavailable(busy), Out, Options).
status_reply(unavailable(Why), Out, Options) :-
    status_reply(service_unavailable(Why), Out, Options).
status_reply(resource_error(Why), Out, Options) :-
    status_reply(service_unavailable(Why), Out, Options).
% replies with content
status_reply(Status, Out, Options) :-
    status_has_content(Status),
    status_page_hook(Status, Reply, Options),
    serialize_body(Reply, Body),
    Status =.. List,
    append(List, [Body], ExList),
    ExStatus =.. ExList,
    phrase(reply_header(ExStatus, Options), Header),
    debug(http(send_reply), "< ~s", [Header]),
    format(Out, '~s', [Header]),
    reply_status_body(Out, Body, Options).

%!  status_has_content(+StatusTerm, -HTTPCode)
%
%   True when StatusTerm  is  a  status   that  usually  comes  with  an
%   expanatory content message.

status_has_content(created(_Location)).
status_has_content(moved(_To)).
status_has_content(moved_temporary(_To)).
status_has_content(gone(_URL)).
status_has_content(see_other(_To)).
status_has_content(bad_request(_ErrorTerm)).
status_has_content(authorise(_Method)).
status_has_content(forbidden(_URL)).
status_has_content(not_found(_URL)).
status_has_content(method_not_allowed(_Method, _URL)).
status_has_content(not_acceptable(_Why)).
status_has_content(server_error(_ErrorTerm)).
status_has_content(service_unavailable(_Why)).

%!  serialize_body(+Reply, -Body) is det.
%
%   Serialize the reply as returned by status_page_hook/3 into a term:
%
%     - body(Type, Encoding, Content)
%     In this term, Type is the media type, Encoding is the
%     required wire encoding and Content a string representing the
%     content.

serialize_body(Reply, Body) :-
    http:serialize_reply(Reply, Body),
    !.
serialize_body(html_tokens(Tokens), body(text/html, utf8, Content)) :-
    !,
    with_output_to(string(Content), print_html(Tokens)).
serialize_body(Reply, Reply) :-
    Reply = body(_,_,_),
    !.
serialize_body(Reply, _) :-
    domain_error(http_reply_body, Reply).

reply_status_body(_, _, Options) :-
    Options.method == head,
    !.
reply_status_body(Out, body(_Type, Encoding, Content), _Options) :-
    (   Encoding == octet
    ->  format(Out, '~s', [Content])
    ;   setup_call_cleanup(
            set_stream(Out, encoding(Encoding)),
            format(Out, '~s', [Content]),
            set_stream(Out, encoding(octet)))
    ).

%!  http:serialize_reply(+Reply, -Body) is semidet.
%
%   Multifile hook to serialize the result of http:status_reply/3
%   into a term
%
%     - body(Type, Encoding, Content)
%     In this term, Type is the media type, Encoding is the
%     required wire encoding and Content a string representing the
%     content.

%!  status_page_hook(+Term, -Reply, +Options) is det.
%
%   Calls the following two hooks to generate an HTML page from a
%   status reply.
%
%     - http:status_reply(+Term, -Reply, +Options)
%       Provide non-HTML description of the (non-200) reply.
%       The term Reply is handed to serialize_body/2, calling
%       the hook http:serialize_reply/2.
%     - http:status_page(+Term, +Context, -HTML)
%     - http:status_page(+Code, +Context, -HTML)
%
%   @arg Term is the status term, e.g., not_found(URL)
%   @see http:status_page/3

status_page_hook(Term, Reply, Options) :-
    Context = Options.context,
    functor(Term, Name, _),
    status_number_fact(Name, Code),
    (   Options.code = Code,
        http:status_reply(Term, Reply, Options)
    ;   http:status_page(Term, Context, HTML),
        Reply = html_tokens(HTML)
    ;   http:status_page(Code, Context, HTML), % deprecated
        Reply = html_tokens(HTML)
    ),
    !.
status_page_hook(created(Location), html_tokens(HTML), _Options) :-
    phrase(page([ title('201 Created')
                ],
                [ h1('Created'),
                  p(['The document was created ',
                     a(href(Location), ' Here')
                    ]),
                  \address
                ]),
           HTML).
status_page_hook(moved(To), html_tokens(HTML), _Options) :-
    phrase(page([ title('301 Moved Permanently')
                ],
                [ h1('Moved Permanently'),
                  p(['The document has moved ',
                     a(href(To), ' Here')
                    ]),
                  \address
                ]),
           HTML).
status_page_hook(moved_temporary(To), html_tokens(HTML), _Options) :-
    phrase(page([ title('302 Moved Temporary')
                ],
                [ h1('Moved Temporary'),
                  p(['The document is currently ',
                     a(href(To), ' Here')
                    ]),
                  \address
                ]),
           HTML).
status_page_hook(gone(URL), html_tokens(HTML), _Options) :-
    phrase(page([ title('410 Resource Gone')
                ],
                [ h1('Resource Gone'),
                  p(['The document has been removed ',
                     a(href(URL), ' from here')
                    ]),
                  \address
                ]),
           HTML).
status_page_hook(see_other(To), html_tokens(HTML), _Options) :-
    phrase(page([ title('303 See Other')
                 ],
                 [ h1('See Other'),
                   p(['See other document ',
                      a(href(To), ' Here')
                     ]),
                   \address
                 ]),
            HTML).
status_page_hook(bad_request(ErrorTerm), html_tokens(HTML), _Options) :-
    '$messages':translate_message(ErrorTerm, Lines, []),
    phrase(page([ title('400 Bad Request')
                ],
                [ h1('Bad Request'),
                  p(\html_message_lines(Lines)),
                  \address
                ]),
           HTML).
status_page_hook(authorise(_Method), html_tokens(HTML), _Options):-
    phrase(page([ title('401 Authorization Required')
                ],
                [ h1('Authorization Required'),
                  p(['This server could not verify that you ',
                     'are authorized to access the document ',
                     'requested.  Either you supplied the wrong ',
                     'credentials (e.g., bad password), or your ',
                     'browser doesn\'t understand how to supply ',
                     'the credentials required.'
                    ]),
                  \address
                ]),
           HTML).
status_page_hook(forbidden(URL), html_tokens(HTML), _Options) :-
    phrase(page([ title('403 Forbidden')
                ],
                [ h1('Forbidden'),
                  p(['You don\'t have permission to access ', URL,
                     ' on this server'
                    ]),
                  \address
                ]),
           HTML).
status_page_hook(not_found(URL), html_tokens(HTML), _Options) :-
    phrase(page([ title('404 Not Found')
                ],
                [ h1('Not Found'),
                  p(['The requested URL ', tt(URL),
                     ' was not found on this server'
                    ]),
                  \address
                ]),
           HTML).
status_page_hook(method_not_allowed(Method,URL), html_tokens(HTML), _Options) :-
    upcase_atom(Method, UMethod),
    phrase(page([ title('405 Method not allowed')
                ],
                [ h1('Method not allowed'),
                  p(['The requested URL ', tt(URL),
                     ' does not support method ', tt(UMethod), '.'
                    ]),
                  \address
                ]),
           HTML).
status_page_hook(not_acceptable(WhyHTML), html_tokens(HTML), _Options) :-
    phrase(page([ title('406 Not Acceptable')
                ],
                [ h1('Not Acceptable'),
                  WhyHTML,
                  \address
                ]),
           HTML).
status_page_hook(server_error(ErrorTerm), html_tokens(HTML), _Options) :-
    '$messages':translate_message(ErrorTerm, Lines, []),
    phrase(page([ title('500 Internal server error')
                ],
                [ h1('Internal server error'),
                  p(\html_message_lines(Lines)),
                  \address
                ]),
           HTML).
status_page_hook(service_unavailable(Why), html_tokens(HTML), _Options) :-
    phrase(page([ title('503 Service Unavailable')
                ],
                [ h1('Service Unavailable'),
                  \unavailable(Why),
                  \address
                ]),
           HTML).

unavailable(busy) -->
    html(p(['The server is temporarily out of resources, ',
            'please try again later'])).
unavailable(error(Formal,Context)) -->
    { '$messages':translate_message(error(Formal,Context), Lines, []) },
    html_message_lines(Lines).
unavailable(HTML) -->
    html(HTML).

html_message_lines([]) -->
    [].
html_message_lines([nl|T]) -->
    !,
    html([br([])]),
    html_message_lines(T).
html_message_lines([flush]) -->
    [].
html_message_lines([Fmt-Args|T]) -->
    !,
    { format(string(S), Fmt, Args)
    },
    html([S]),
    html_message_lines(T).
html_message_lines([Fmt|T]) -->
    !,
    { format(string(S), Fmt, [])
    },
    html([S]),
    html_message_lines(T).

%!  http_join_headers(+Default, +Header, -Out)
%
%   Append headers from Default to Header if they are not
%   already part of it.

http_join_headers([], H, H).
http_join_headers([H|T], Hdr0, Hdr) :-
    functor(H, N, A),
    functor(H2, N, A),
    member(H2, Hdr0),
    !,
    http_join_headers(T, Hdr0, Hdr).
http_join_headers([H|T], Hdr0, [H|Hdr]) :-
    http_join_headers(T, Hdr0, Hdr).


%!  http_update_encoding(+HeaderIn, -Encoding, -HeaderOut)
%
%   Allow for rewrite of the  header,   adjusting  the  encoding. We
%   distinguish three options. If  the   user  announces  `text', we
%   always use UTF-8 encoding. If   the user announces charset=utf-8
%   we  use  UTF-8  and  otherwise  we  use  octet  (raw)  encoding.
%   Alternatively we could dynamically choose for ASCII, ISO-Latin-1
%   or UTF-8.

http_update_encoding(Header0, utf8, [content_type(Type)|Header]) :-
    select(content_type(Type0), Header0, Header),
    sub_atom(Type0, 0, _, _, 'text/'),
    !,
    (   sub_atom(Type0, S, _, _, ';')
    ->  sub_atom(Type0, 0, S, _, B)
    ;   B = Type0
    ),
    atom_concat(B, '; charset=UTF-8', Type).
http_update_encoding(Header, Encoding, Header) :-
    memberchk(content_type(Type), Header),
    (   (   sub_atom(Type, _, _, _, 'UTF-8')
        ;   sub_atom(Type, _, _, _, 'utf-8')
        )
    ->  Encoding = utf8
    ;   media_type:media_type_encoding(media(Type,[]), Encoding)
    ).
http_update_encoding(Header, octet, Header).

%!  http_update_connection(+CGIHeader, +Request, -Connection, -Header)
%
%   Merge keep-alive information from  Request   and  CGIHeader into
%   Header.

http_update_connection(CgiHeader, Request, Connect,
                       [connection(Connect)|Rest]) :-
    select(connection(CgiConn), CgiHeader, Rest),
    !,
    connection(Request, ReqConnection),
    join_connection(ReqConnection, CgiConn, Connect).
http_update_connection(CgiHeader, Request, Connect,
                       [connection(Connect)|CgiHeader]) :-
    connection(Request, Connect).

join_connection(Keep1, Keep2, Connection) :-
    (   downcase_atom(Keep1, 'keep-alive'),
        downcase_atom(Keep2, 'keep-alive')
    ->  Connection = 'Keep-Alive'
    ;   Connection = close
    ).


%!  connection(+Header, -Connection)
%
%   Extract the desired connection from a header.

connection(Header, Close) :-
    (   memberchk(connection(Connection), Header)
    ->  Close = Connection
    ;   memberchk(http_version(1-X), Header),
        X >= 1
    ->  Close = 'Keep-Alive'
    ;   Close = close
    ).


%!  http_update_transfer(+Request, +CGIHeader, -Transfer, -Header)
%
%   Decide on the transfer encoding  from   the  Request and the CGI
%   header.    The    behaviour    depends      on    the    setting
%   http:chunked_transfer. If =never=, even   explitic  requests are
%   ignored. If =on_request=, chunked encoding  is used if requested
%   through  the  CGI  header  and  allowed    by   the  client.  If
%   =if_possible=, chunked encoding is  used   whenever  the  client
%   allows for it, which is  interpreted   as  the client supporting
%   HTTP 1.1 or higher.
%
%   Chunked encoding is more space efficient   and allows the client
%   to start processing partial results. The drawback is that errors
%   lead to incomplete pages instead of  a nicely formatted complete
%   page.

http_update_transfer(Request, CgiHeader, Transfer, Header) :-
    setting(http:chunked_transfer, When),
    http_update_transfer(When, Request, CgiHeader, Transfer, Header).

http_update_transfer(never, _, CgiHeader, none, Header) :-
    !,
    delete(CgiHeader, transfer_encoding(_), Header).
http_update_transfer(_, _, CgiHeader, none, Header) :-
    memberchk(location(_), CgiHeader),
    !,
    delete(CgiHeader, transfer_encoding(_), Header).
http_update_transfer(_, Request, CgiHeader, Transfer, Header) :-
    select(transfer_encoding(CgiTransfer), CgiHeader, Rest),
    !,
    transfer(Request, ReqConnection),
    join_transfer(ReqConnection, CgiTransfer, Transfer),
    (   Transfer == none
    ->  Header = Rest
    ;   Header = [transfer_encoding(Transfer)|Rest]
    ).
http_update_transfer(if_possible, Request, CgiHeader, Transfer, Header) :-
    transfer(Request, Transfer),
    Transfer \== none,
    !,
    Header = [transfer_encoding(Transfer)|CgiHeader].
http_update_transfer(_, _, CgiHeader, none, CgiHeader).

join_transfer(chunked, chunked, chunked) :- !.
join_transfer(_, _, none).


%!  transfer(+Header, -Connection)
%
%   Extract the desired connection from a header.

transfer(Header, Transfer) :-
    (   memberchk(transfer_encoding(Transfer0), Header)
    ->  Transfer = Transfer0
    ;   memberchk(http_version(1-X), Header),
        X >= 1
    ->  Transfer = chunked
    ;   Transfer = none
    ).


%!  content_length_in_encoding(+Encoding, +In, -Bytes)
%
%   Determine hom many bytes are required to represent the data from
%   stream In using the given encoding.  Fails if the data cannot be
%   represented with the given encoding.

content_length_in_encoding(Enc, Stream, Bytes) :-
    stream_property(Stream, position(Here)),
    setup_call_cleanup(
        open_null_stream(Out),
        ( set_stream(Out, encoding(Enc)),
          catch(copy_stream_data(Stream, Out), _, fail),
          flush_output(Out),
          byte_count(Out, Bytes)
        ),
        ( close(Out, [force(true)]),
          set_stream_position(Stream, Here)
        )).


                 /*******************************
                 *          POST SUPPORT        *
                 *******************************/

%!  http_post_data(+Data, +Out:ostream, +HdrExtra) is det.
%
%   Send data on behalf on an HTTP   POST request. This predicate is
%   normally called by http_post/4 from   http_client.pl to send the
%   POST data to the server.  Data is one of:
%
%     * html(+Tokens)
%     Result of html//1 from html_write.pl
%
%     * xml(+Term)
%     Post the result of xml_write/3 using the Mime-type
%     =|text/xml|=
%
%     * xml(+Type, +Term)
%     Post the result of xml_write/3 using the given Mime-type
%     and an empty option list to xml_write/3.
%
%     * xml(+Type, +Term, +Options)
%     Post the result of xml_write/3 using the given Mime-type
%     and option list for xml_write/3.
%
%     * file(+File)
%     Send contents of a file. Mime-type is determined by
%     file_mime_type/2.
%
%     * file(+Type, +File)
%     Send file with content of indicated mime-type.
%
%     * memory_file(+Type, +Handle)
%     Similar to file(+Type, +File), but using a memory file
%     instead of a real file.  See new_memory_file/1.
%
%     * codes(+Codes)
%     As codes(text/plain, Codes).
%
%     * codes(+Type, +Codes)
%     Send Codes using the indicated MIME-type.
%
%     * bytes(+Type, +Bytes)
%     Send Bytes using the indicated MIME-type.  Bytes is either a
%     string of character codes 0..255 or list of integers in the
%     range 0..255.  Out-of-bound codes result in a representation
%     error exception.
%
%     * atom(+Atom)
%     As atom(text/plain, Atom).
%
%     * atom(+Type, +Atom)
%     Send Atom using the indicated MIME-type.
%
%     * cgi_stream(+Stream, +Len) Read the input from Stream which,
%     like CGI data starts with a partial HTTP header. The fields of
%     this header are merged with the provided HdrExtra fields. The
%     first Len characters of Stream are used.
%
%     * form(+ListOfParameter)
%     Send data of the MIME type application/x-www-form-urlencoded as
%     produced by browsers issuing a POST request from an HTML form.
%     ListOfParameter is a list of Name=Value or Name(Value).
%
%     * form_data(+ListOfData)
%     Send data of the MIME type =|multipart/form-data|= as produced
%     by browsers issuing a POST request from an HTML form using
%     enctype =|multipart/form-data|=. ListOfData is the same as for
%     the List alternative described below. Below is an example.
%     Repository, etc. are atoms providing the value, while the last
%     argument provides a value from a file.
%
%       ==
%       ...,
%       http_post([ protocol(http),
%                   host(Host),
%                   port(Port),
%                   path(ActionPath)
%                 ],
%                 form_data([ repository = Repository,
%                             dataFormat = DataFormat,
%                             baseURI    = BaseURI,
%                             verifyData = Verify,
%                             data       = file(File)
%                           ]),
%                 _Reply,
%                 []),
%       ...,
%       ==
%
%     * List
%     If the argument is a plain list, it is sent using the MIME type
%     multipart/mixed and packed using mime_pack/3. See mime_pack/3
%     for details on the argument format.

http_post_data(Data, Out, HdrExtra) :-
    http:post_data_hook(Data, Out, HdrExtra),
    !.
http_post_data(html(HTML), Out, HdrExtra) :-
    !,
    phrase(post_header(html(HTML), HdrExtra), Header),
    debug(http(send_request), "> ~s", [Header]),
    format(Out, '~s', [Header]),
    print_html(Out, HTML).
http_post_data(xml(XML), Out, HdrExtra) :-
    !,
    http_post_data(xml(text/xml, XML, []), Out, HdrExtra).
http_post_data(xml(Type, XML), Out, HdrExtra) :-
    !,
    http_post_data(xml(Type, XML, []), Out, HdrExtra).
http_post_data(xml(Type, XML, Options), Out, HdrExtra) :-
    !,
    setup_call_cleanup(
        new_memory_file(MemFile),
        (   setup_call_cleanup(
                open_memory_file(MemFile, write, MemOut),
                xml_write(MemOut, XML, Options),
                close(MemOut)),
            http_post_data(memory_file(Type, MemFile), Out, HdrExtra)
        ),
        free_memory_file(MemFile)).
http_post_data(file(File), Out, HdrExtra) :-
    !,
    (   file_mime_type(File, Type)
    ->  true
    ;   Type = text/plain
    ),
    http_post_data(file(Type, File), Out, HdrExtra).
http_post_data(file(Type, File), Out, HdrExtra) :-
    !,
    phrase(post_header(file(Type, File), HdrExtra), Header),
    debug(http(send_request), "> ~s", [Header]),
    format(Out, '~s', [Header]),
    setup_call_cleanup(
        open(File, read, In, [type(binary)]),
        copy_stream_data(In, Out),
        close(In)).
http_post_data(memory_file(Type, Handle), Out, HdrExtra) :-
    !,
    phrase(post_header(memory_file(Type, Handle), HdrExtra), Header),
    debug(http(send_request), "> ~s", [Header]),
    format(Out, '~s', [Header]),
    setup_call_cleanup(
        open_memory_file(Handle, read, In, [encoding(octet)]),
        copy_stream_data(In, Out),
        close(In)).
http_post_data(codes(Codes), Out, HdrExtra) :-
    !,
    http_post_data(codes(text/plain, Codes), Out, HdrExtra).
http_post_data(codes(Type, Codes), Out, HdrExtra) :-
    !,
    phrase(post_header(codes(Type, Codes), HdrExtra), Header),
    debug(http(send_request), "> ~s", [Header]),
    format(Out, '~s', [Header]),
    setup_call_cleanup(
        set_stream(Out, encoding(utf8)),
        format(Out, '~s', [Codes]),
        set_stream(Out, encoding(octet))).
http_post_data(bytes(Type, Bytes), Out, HdrExtra) :-
    !,
    phrase(post_header(bytes(Type, Bytes), HdrExtra), Header),
    debug(http(send_request), "> ~s", [Header]),
    format(Out, '~s~s', [Header, Bytes]).
http_post_data(atom(Atom), Out, HdrExtra) :-
    !,
    http_post_data(atom(text/plain, Atom), Out, HdrExtra).
http_post_data(atom(Type, Atom), Out, HdrExtra) :-
    !,
    phrase(post_header(atom(Type, Atom), HdrExtra), Header),
    debug(http(send_request), "> ~s", [Header]),
    format(Out, '~s', [Header]),
    setup_call_cleanup(
        set_stream(Out, encoding(utf8)),
        write(Out, Atom),
        set_stream(Out, encoding(octet))).
http_post_data(cgi_stream(In, _Len), Out, HdrExtra) :-
    !,
    debug(obsolete, 'Obsolete 2nd argument in cgi_stream(In,Len)', []),
    http_post_data(cgi_stream(In), Out, HdrExtra).
http_post_data(cgi_stream(In), Out, HdrExtra) :-
    !,
    http_read_header(In, Header0),
    http_update_encoding(Header0, Encoding, Header),
    content_length_in_encoding(Encoding, In, Size),
    http_join_headers(HdrExtra, Header, Hdr2),
    phrase(post_header(cgi_data(Size), Hdr2), HeaderText),
    debug(http(send_request), "> ~s", [HeaderText]),
    format(Out, '~s', [HeaderText]),
    setup_call_cleanup(
        set_stream(Out, encoding(Encoding)),
        copy_stream_data(In, Out),
        set_stream(Out, encoding(octet))).
http_post_data(form(Fields), Out, HdrExtra) :-
    !,
    parse_url_search(Codes, Fields),
    length(Codes, Size),
    http_join_headers(HdrExtra,
                      [ content_type('application/x-www-form-urlencoded')
                      ], Header),
    phrase(post_header(cgi_data(Size), Header), HeaderChars),
    debug(http(send_request), "> ~s", [HeaderChars]),
    format(Out, '~s', [HeaderChars]),
    format(Out, '~s', [Codes]).
http_post_data(form_data(Data), Out, HdrExtra) :-
    !,
    setup_call_cleanup(
        new_memory_file(MemFile),
        ( setup_call_cleanup(
              open_memory_file(MemFile, write, MimeOut),
              mime_pack(Data, MimeOut, Boundary),
              close(MimeOut)),
          size_memory_file(MemFile, Size, octet),
          format(string(ContentType),
                 'multipart/form-data; boundary=~w', [Boundary]),
          http_join_headers(HdrExtra,
                            [ mime_version('1.0'),
                              content_type(ContentType)
                            ], Header),
          phrase(post_header(cgi_data(Size), Header), HeaderChars),
          debug(http(send_request), "> ~s", [HeaderChars]),
          format(Out, '~s', [HeaderChars]),
          setup_call_cleanup(
              open_memory_file(MemFile, read, In, [encoding(octet)]),
              copy_stream_data(In, Out),
              close(In))
        ),
        free_memory_file(MemFile)).
http_post_data(List, Out, HdrExtra) :-          % multipart-mixed
    is_list(List),
    !,
    setup_call_cleanup(
        new_memory_file(MemFile),
        ( setup_call_cleanup(
              open_memory_file(MemFile, write, MimeOut),
              mime_pack(List, MimeOut, Boundary),
              close(MimeOut)),
          size_memory_file(MemFile, Size, octet),
          format(string(ContentType),
                 'multipart/mixed; boundary=~w', [Boundary]),
          http_join_headers(HdrExtra,
                            [ mime_version('1.0'),
                              content_type(ContentType)
                            ], Header),
          phrase(post_header(cgi_data(Size), Header), HeaderChars),
          debug(http(send_request), "> ~s", [HeaderChars]),
          format(Out, '~s', [HeaderChars]),
          setup_call_cleanup(
              open_memory_file(MemFile, read, In, [encoding(octet)]),
              copy_stream_data(In, Out),
              close(In))
        ),
        free_memory_file(MemFile)).

%!  post_header(+Data, +HeaderExtra)//
%
%   Generate the POST header, emitting HeaderExtra, followed by the
%   HTTP Content-length and Content-type fields.

post_header(html(Tokens), HdrExtra) -->
    header_fields(HdrExtra, Len),
    content_length(html(Tokens), Len),
    content_type(text/html),
    "\r\n".
post_header(file(Type, File), HdrExtra) -->
    header_fields(HdrExtra, Len),
    content_length(file(File), Len),
    content_type(Type),
    "\r\n".
post_header(memory_file(Type, File), HdrExtra) -->
    header_fields(HdrExtra, Len),
    content_length(memory_file(File), Len),
    content_type(Type),
    "\r\n".
post_header(cgi_data(Size), HdrExtra) -->
    header_fields(HdrExtra, Len),
    content_length(Size, Len),
    "\r\n".
post_header(codes(Type, Codes), HdrExtra) -->
    header_fields(HdrExtra, Len),
    content_length(codes(Codes, utf8), Len),
    content_type(Type, utf8),
    "\r\n".
post_header(bytes(Type, Bytes), HdrExtra) -->
    header_fields(HdrExtra, Len),
    content_length(bytes(Bytes), Len),
    content_type(Type),
    "\r\n".
post_header(atom(Type, Atom), HdrExtra) -->
    header_fields(HdrExtra, Len),
    content_length(atom(Atom, utf8), Len),
    content_type(Type, utf8),
    "\r\n".


                 /*******************************
                 *       OUTPUT HEADER DCG      *
                 *******************************/

%!  http_reply_header(+Out:ostream, +What, +HdrExtra) is det.
%
%   Create a reply header  using  reply_header//3   and  send  it to
%   Stream.

http_reply_header(Out, What, HdrExtra) :-
    phrase(reply_header(What, HdrExtra, _Code), String),
    !,
    debug(http(send_reply), "< ~s", [String]),
    format(Out, '~s', [String]).

%!  reply_header(+Data, +HdrExtra, -Code)// is det.
%
%   Grammar that realises the HTTP handler for sending Data. Data is
%   a  real  data  object  as  described   with  http_reply/2  or  a
%   not-200-ok HTTP status reply. The   following status replies are
%   defined.
%
%     * created(+URL, +HTMLTokens)
%     * moved(+URL, +HTMLTokens)
%     * moved_temporary(+URL, +HTMLTokens)
%     * see_other(+URL, +HTMLTokens)
%     * status(+Status)
%     * status(+Status, +HTMLTokens)
%     * authorise(+Method, +Realm, +Tokens)
%     * authorise(+Method, +Tokens)
%     * not_found(+URL, +HTMLTokens)
%     * server_error(+Error, +Tokens)
%     * resource_error(+Error, +Tokens)
%     * service_unavailable(+Why, +Tokens)
%
%   @see http_status_reply/4 formulates the not-200-ok HTTP replies.

reply_header(Data, Dict) -->
    { _{header:HdrExtra, code:Code} :< Dict },
    reply_header(Data, HdrExtra, Code).

reply_header(string(String), HdrExtra, Code) -->
    reply_header(string(text/plain, String), HdrExtra, Code).
reply_header(string(Type, String), HdrExtra, Code) -->
    vstatus(ok, Code, HdrExtra),
    date(now),
    header_fields(HdrExtra, CLen),
    content_length(codes(String, utf8), CLen),
    content_type(Type, utf8),
    "\r\n".
reply_header(bytes(Type, Bytes), HdrExtra, Code) -->
    vstatus(ok, Code, HdrExtra),
    date(now),
    header_fields(HdrExtra, CLen),
    content_length(bytes(Bytes), CLen),
    content_type(Type),
    "\r\n".
reply_header(html(Tokens), HdrExtra, Code) -->
    vstatus(ok, Code, HdrExtra),
    date(now),
    header_fields(HdrExtra, CLen),
    content_length(html(Tokens), CLen),
    content_type(text/html),
    "\r\n".
reply_header(file(Type, File), HdrExtra, Code) -->
    vstatus(ok, Code, HdrExtra),
    date(now),
    modified(file(File)),
    header_fields(HdrExtra, CLen),
    content_length(file(File), CLen),
    content_type(Type),
    "\r\n".
reply_header(gzip_file(Type, File), HdrExtra, Code) -->
    vstatus(ok, Code, HdrExtra),
    date(now),
    modified(file(File)),
    header_fields(HdrExtra, CLen),
    content_length(file(File), CLen),
    content_type(Type),
    content_encoding(gzip),
    "\r\n".
reply_header(file(Type, File, Range), HdrExtra, Code) -->
    vstatus(partial_content, Code, HdrExtra),
    date(now),
    modified(file(File)),
    header_fields(HdrExtra, CLen),
    content_length(file(File, Range), CLen),
    content_type(Type),
    "\r\n".
reply_header(tmp_file(Type, File), HdrExtra, Code) -->
    vstatus(ok, Code, HdrExtra),
    date(now),
    header_fields(HdrExtra, CLen),
    content_length(file(File), CLen),
    content_type(Type),
    "\r\n".
reply_header(cgi_data(Size), HdrExtra, Code) -->
    vstatus(ok, Code, HdrExtra),
    date(now),
    header_fields(HdrExtra, CLen),
    content_length(Size, CLen),
    "\r\n".
reply_header(chunked_data, HdrExtra, Code) -->
    vstatus(ok, Code, HdrExtra),
    date(now),
    header_fields(HdrExtra, _),
    (   {memberchk(transfer_encoding(_), HdrExtra)}
    ->  ""
    ;   transfer_encoding(chunked)
    ),
    "\r\n".
% non-200 replies without a body (e.g., 1xx, 204, 304)
reply_header(status(Status), HdrExtra, Code) -->
    vstatus(Status, Code),
    header_fields(HdrExtra, Clen),
    { Clen = 0 },
    "\r\n".
% non-200 replies with a body
reply_header(Data, HdrExtra, Code) -->
    { status_reply_headers(Data,
                           body(Type, Encoding, Content),
                           ReplyHeaders),
      http_join_headers(ReplyHeaders, HdrExtra, Headers),
      functor(Data, CodeName, _)
    },
    vstatus(CodeName, Code, Headers),
    date(now),
    header_fields(Headers, CLen),
    content_length(codes(Content, Encoding), CLen),
    content_type(Type, Encoding),
    "\r\n".

status_reply_headers(created(Location, Body), Body,
                     [ location(Location) ]).
status_reply_headers(moved(To, Body), Body,
                     [ location(To) ]).
status_reply_headers(moved_temporary(To, Body), Body,
                     [ location(To) ]).
status_reply_headers(gone(_URL, Body), Body, []).
status_reply_headers(see_other(To, Body), Body,
                     [ location(To) ]).
status_reply_headers(authorise(Method, Body), Body,
                     [ www_authenticate(Method) ]).
status_reply_headers(not_found(_URL, Body), Body, []).
status_reply_headers(forbidden(_URL, Body), Body, []).
status_reply_headers(method_not_allowed(_Method, _URL, Body), Body, []).
status_reply_headers(server_error(_Error, Body), Body, []).
status_reply_headers(service_unavailable(_Why, Body), Body, []).
status_reply_headers(not_acceptable(_Why, Body), Body, []).
status_reply_headers(bad_request(_Error, Body), Body, []).


%!  vstatus(+Status, -Code)// is det.
%!  vstatus(+Status, -Code, +HdrExtra)// is det.
%
%   Emit the HTTP header for Status

vstatus(_Status, Code, HdrExtra) -->
    {memberchk(status(Code), HdrExtra)},
    !,
    vstatus(_NewStatus, Code).
vstatus(Status, Code, _) -->
    vstatus(Status, Code).

vstatus(Status, Code) -->
    "HTTP/1.1 ",
    status_number(Status, Code),
    " ",
    status_comment(Status),
    "\r\n".

%!  status_number(?Status, ?Code)// is semidet.
%
%   Parse/generate the HTTP status  numbers  and   map  them  to the
%   proper name.
%
%   @see See the source code for supported status names and codes.

status_number(Status, Code) -->
    { var(Status) },
    !,
    integer(Code),
    { status_number(Status, Code) },
    !.
status_number(Status, Code) -->
    { status_number(Status, Code) },
    integer(Code).

%!  status_number(+Status:atom, -Code:nonneg) is det.
%!  status_number(-Status:atom, +Code:nonneg) is det.
%
%   Relates a symbolic  HTTP   status  names to their integer Code.
%   Each code also needs a rule for status_comment//1.
%
%   @throws type_error    If Code is instantiated with something other than
%                         an integer.
%   @throws domain_error  If Code is instantiated with an integer
%                         outside of the range [100-599] of defined
%                         HTTP status codes.

% Unrecognized status codes that are within a defined code class.
% RFC 7231 states:
%   "[...] a client MUST understand the class of any status code,
%    as indicated by the first digit, and treat an unrecognized status code
%    as being equivalent to the `x00` status code of that class [...]
%   "
% @see http://tools.ietf.org/html/rfc7231#section-6

status_number(Status, Code) :-
    nonvar(Status),
    !,
    status_number_fact(Status, Code).
status_number(Status, Code) :-
    nonvar(Code),
    !,
    (   between(100, 599, Code)
    ->  (   status_number_fact(Status, Code)
        ->  true
        ;   ClassCode is Code // 100 * 100,
            status_number_fact(Status, ClassCode)
        )
    ;   domain_error(http_code, Code)
    ).

status_number_fact(continue,                   100).
status_number_fact(switching_protocols,        101).
status_number_fact(ok,                         200).
status_number_fact(created,                    201).
status_number_fact(accepted,                   202).
status_number_fact(non_authoritative_info,     203).
status_number_fact(no_content,                 204).
status_number_fact(reset_content,              205).
status_number_fact(partial_content,            206).
status_number_fact(multiple_choices,           300).
status_number_fact(moved,                      301).
status_number_fact(moved_temporary,            302).
status_number_fact(see_other,                  303).
status_number_fact(not_modified,               304).
status_number_fact(use_proxy,                  305).
status_number_fact(unused,                     306).
status_number_fact(temporary_redirect,         307).
status_number_fact(bad_request,                400).
status_number_fact(authorise,                  401).
status_number_fact(payment_required,           402).
status_number_fact(forbidden,                  403).
status_number_fact(not_found,                  404).
status_number_fact(method_not_allowed,         405).
status_number_fact(not_acceptable,             406).
status_number_fact(request_timeout,            408).
status_number_fact(conflict,                   409).
status_number_fact(gone,                       410).
status_number_fact(length_required,            411).
status_number_fact(payload_too_large,          413).
status_number_fact(uri_too_long,               414).
status_number_fact(unsupported_media_type,     415).
status_number_fact(expectation_failed,         417).
status_number_fact(upgrade_required,           426).
status_number_fact(server_error,               500).
status_number_fact(not_implemented,            501).
status_number_fact(bad_gateway,                502).
status_number_fact(service_unavailable,        503).
status_number_fact(gateway_timeout,            504).
status_number_fact(http_version_not_supported, 505).


%!  status_comment(+Code:atom)// is det.
%
%   Emit standard HTTP human-readable comment on the reply-status.

status_comment(continue) -->
    "Continue".
status_comment(switching_protocols) -->
    "Switching Protocols".
status_comment(ok) -->
    "OK".
status_comment(created) -->
    "Created".
status_comment(accepted) -->
    "Accepted".
status_comment(non_authoritative_info) -->
    "Non-Authoritative Information".
status_comment(no_content) -->
    "No Content".
status_comment(reset_content) -->
    "Reset Content".
status_comment(created) -->
    "Created".
status_comment(partial_content) -->
    "Partial content".
status_comment(multiple_choices) -->
    "Multiple Choices".
status_comment(moved) -->
    "Moved Permanently".
status_comment(moved_temporary) -->
    "Moved Temporary".
status_comment(see_other) -->
    "See Other".
status_comment(not_modified) -->
    "Not Modified".
status_comment(use_proxy) -->
    "Use Proxy".
status_comment(unused) -->
    "Unused".
status_comment(temporary_redirect) -->
    "Temporary Redirect".
status_comment(bad_request) -->
    "Bad Request".
status_comment(authorise) -->
    "Authorization Required".
status_comment(payment_required) -->
    "Payment Required".
status_comment(forbidden) -->
    "Forbidden".
status_comment(not_found) -->
    "Not Found".
status_comment(method_not_allowed) -->
    "Method Not Allowed".
status_comment(not_acceptable) -->
    "Not Acceptable".
status_comment(request_timeout) -->
    "Request Timeout".
status_comment(conflict) -->
    "Conflict".
status_comment(gone) -->
    "Gone".
status_comment(length_required) -->
    "Length Required".
status_comment(payload_too_large) -->
    "Payload Too Large".
status_comment(uri_too_long) -->
    "URI Too Long".
status_comment(unsupported_media_type) -->
    "Unsupported Media Type".
status_comment(expectation_failed) -->
    "Expectation Failed".
status_comment(upgrade_required) -->
    "Upgrade Required".
status_comment(server_error) -->
    "Internal Server Error".
status_comment(not_implemented) -->
    "Not Implemented".
status_comment(bad_gateway) -->
    "Bad Gateway".
status_comment(service_unavailable) -->
    "Service Unavailable".
status_comment(gateway_timeout) -->
    "Gateway Timeout".
status_comment(http_version_not_supported) -->
    "HTTP Version Not Supported".

date(Time) -->
    "Date: ",
    (   { Time == now }
    ->  now
    ;   rfc_date(Time)
    ),
    "\r\n".

modified(file(File)) -->
    !,
    { time_file(File, Time)
    },
    modified(Time).
modified(Time) -->
    "Last-modified: ",
    (   { Time == now }
    ->  now
    ;   rfc_date(Time)
    ),
    "\r\n".


%!  content_length(+Object, ?Len)// is det.
%
%   Emit the content-length field and (optionally) the content-range
%   field.
%
%   @param Len Number of bytes specified

content_length(file(File, bytes(From, To)), Len) -->
    !,
    { size_file(File, Size),
      (   To == end
      ->  Len is Size - From,
          RangeEnd is Size - 1
      ;   Len is To+1 - From,       % To is index of last byte
          RangeEnd = To
      )
    },
    content_range(bytes, From, RangeEnd, Size),
    content_length(Len, Len).
content_length(Reply, Len) -->
    { length_of(Reply, Len)
    },
    "Content-Length: ", integer(Len),
    "\r\n".


length_of(_, Len) :-
    nonvar(Len),
    !.
length_of(codes(String, Encoding), Len) :-
    !,
    setup_call_cleanup(
        open_null_stream(Out),
        ( set_stream(Out, encoding(Encoding)),
          format(Out, '~s', [String]),
          byte_count(Out, Len)
        ),
        close(Out)).
length_of(atom(Atom, Encoding), Len) :-
    !,
    setup_call_cleanup(
        open_null_stream(Out),
        ( set_stream(Out, encoding(Encoding)),
          format(Out, '~a', [Atom]),
          byte_count(Out, Len)
        ),
        close(Out)).
length_of(file(File), Len) :-
    !,
    size_file(File, Len).
length_of(memory_file(Handle), Len) :-
    !,
    size_memory_file(Handle, Len, octet).
length_of(html_tokens(Tokens), Len) :-
    !,
    html_print_length(Tokens, Len).
length_of(html(Tokens), Len) :-     % deprecated
    !,
    html_print_length(Tokens, Len).
length_of(bytes(Bytes), Len) :-
    !,
    (   string(Bytes)
    ->  string_length(Bytes, Len)
    ;   length(Bytes, Len)          % assuming a list of 0..255
    ).
length_of(Len, Len).


%!  content_range(+Unit:atom, +From:int, +RangeEnd:int, +Size:int)// is det
%
%   Emit the =|Content-Range|= header  for   partial  content  (206)
%   replies.

content_range(Unit, From, RangeEnd, Size) -->
    "Content-Range: ", atom(Unit), " ",
    integer(From), "-", integer(RangeEnd), "/", integer(Size),
    "\r\n".

content_encoding(Encoding) -->
    "Content-Encoding: ", atom(Encoding), "\r\n".

transfer_encoding(Encoding) -->
    "Transfer-Encoding: ", atom(Encoding), "\r\n".

content_type(Type) -->
    content_type(Type, _).

content_type(Type, Charset) -->
    ctype(Type),
    charset(Charset),
    "\r\n".

ctype(Main/Sub) -->
    !,
    "Content-Type: ",
    atom(Main),
    "/",
    atom(Sub).
ctype(Type) -->
    !,
    "Content-Type: ",
    atom(Type).

charset(Var) -->
    { var(Var) },
    !.
charset(utf8) -->
    !,
    "; charset=UTF-8".
charset(CharSet) -->
    "; charset=",
    atom(CharSet).

%!  header_field(-Name, -Value)// is det.
%!  header_field(+Name, +Value) is det.
%
%   Process an HTTP request property. Request properties appear as a
%   single line in an HTTP header.

header_field(Name, Value) -->
    { var(Name) },                 % parsing
    !,
    field_name(Name),
    ":",
    whites,
    read_field_value(ValueChars),
    blanks_to_nl,
    !,
    {   field_to_prolog(Name, ValueChars, Value)
    ->  true
    ;   atom_codes(Value, ValueChars),
        domain_error(Name, Value)
    }.
header_field(Name, Value) -->
    field_name(Name),
    ": ",
    field_value(Name, Value),
    "\r\n".

%!  read_field_value(-Codes)//
%
%   Read a field eagerly upto the next whitespace

read_field_value([H|T]) -->
    [H],
    { \+ code_type(H, space) },
    !,
    read_field_value(T).
read_field_value([]) -->
    "".
read_field_value([H|T]) -->
    [H],
    read_field_value(T).


%!  http_parse_header_value(+Field, +Value, -Prolog) is semidet.
%
%   Translate Value in a meaningful Prolog   term. Field denotes the
%   HTTP request field for which we   do  the translation. Supported
%   fields are:
%
%     * content_length
%     Converted into an integer
%     * status
%     Converted into an integer
%     * cookie
%     Converted into a list with Name=Value by cookies//1.
%     * set_cookie
%     Converted into a term set_cookie(Name, Value, Options).
%     Options is a list consisting of Name=Value or a single
%     atom (e.g., =secure=)
%     * host
%     Converted to HostName:Port if applicable.
%     * range
%     Converted into bytes(From, To), where From is an integer
%     and To is either an integer or the atom =end=.
%     * accept
%     Parsed to a list of media descriptions.  Each media is a term
%     media(Type, TypeParams, Quality, AcceptExts). The list is
%     sorted according to preference.
%     * content_disposition
%     Parsed into disposition(Name, Attributes), where Attributes is
%     a list of Name=Value pairs.
%     * content_type
%     Parsed into media(Type/SubType, Attributes), where Attributes
%     is a list of Name=Value pairs.
%
%   As some fields are already parsed in the `Request`, this predicate
%   is a no-op when called on an already parsed field.
%
%   @arg Value is either an atom, a list of codes or an already parsed
%   header value.

http_parse_header_value(Field, Value, Prolog) :-
    known_field(Field, _, Type),
    (   already_parsed(Type, Value)
    ->  Prolog = Value
    ;   to_codes(Value, Codes),
        parse_header_value(Field, Codes, Prolog)
    ).

already_parsed(integer, V)    :- !, integer(V).
already_parsed(list(Type), L) :- !, is_list(L), maplist(already_parsed(Type), L).
already_parsed(Term, V)       :- subsumes_term(Term, V).


%!  known_field(?FieldName, ?AutoConvert, -Type)
%
%   True if the value of FieldName is   by default translated into a
%   Prolog data structure.

known_field(content_length,      true,  integer).
known_field(status,              true,  integer).
known_field(cookie,              true,  list(_=_)).
known_field(set_cookie,          true,  list(set_cookie(_Name,_Value,_Options))).
known_field(host,                true,  _Host:_Port).
known_field(range,               maybe, bytes(_,_)).
known_field(accept,              maybe, list(media(_Type, _Parms, _Q, _Exts))).
known_field(content_disposition, maybe, disposition(_Name, _Attributes)).
known_field(content_type,        false, media(_Type/_Sub, _Attributes)).

to_codes(In, Codes) :-
    (   is_list(In)
    ->  Codes = In
    ;   atom_codes(In, Codes)
    ).

%!  field_to_prolog(+Field, +ValueCodes, -Prolog) is semidet.
%
%   Translate the value string into  a   sensible  Prolog  term. For
%   known_fields(_,true), this must succeed. For   =maybe=,  we just
%   return the atom if the translation fails.

field_to_prolog(Field, Codes, Prolog) :-
    known_field(Field, true, _Type),
    !,
    (   parse_header_value(Field, Codes, Prolog0)
    ->  Prolog = Prolog0
    ).
field_to_prolog(Field, Codes, Prolog) :-
    known_field(Field, maybe, _Type),
    parse_header_value(Field, Codes, Prolog0),
    !,
    Prolog = Prolog0.
field_to_prolog(_, Codes, Atom) :-
    atom_codes(Atom, Codes).

%!  parse_header_value(+Field, +ValueCodes, -Value) is semidet.
%
%   Parse the value text of an HTTP   field into a meaningful Prolog
%   representation.

parse_header_value(content_length, ValueChars, ContentLength) :-
    number_codes(ContentLength, ValueChars).
parse_header_value(status, ValueChars, Code) :-
    (   phrase(" ", L, _),
        append(Pre, L, ValueChars)
    ->  number_codes(Code, Pre)
    ;   number_codes(Code, ValueChars)
    ).
parse_header_value(cookie, ValueChars, Cookies) :-
    debug(cookie, 'Cookie: ~s', [ValueChars]),
    phrase(cookies(Cookies), ValueChars).
parse_header_value(set_cookie, ValueChars, SetCookie) :-
    debug(cookie, 'SetCookie: ~s', [ValueChars]),
    phrase(set_cookie(SetCookie), ValueChars).
parse_header_value(host, ValueChars, Host) :-
    (   append(HostChars, [0':|PortChars], ValueChars),
        catch(number_codes(Port, PortChars), _, fail)
    ->  atom_codes(HostName, HostChars),
        Host = HostName:Port
    ;   atom_codes(Host, ValueChars)
    ).
parse_header_value(range, ValueChars, Range) :-
    phrase(range(Range), ValueChars).
parse_header_value(accept, ValueChars, Media) :-
    parse_accept(ValueChars, Media).
parse_header_value(content_disposition, ValueChars, Disposition) :-
    phrase(content_disposition(Disposition), ValueChars).
parse_header_value(content_type, ValueChars, Type) :-
    phrase(parse_content_type(Type), ValueChars).

%!  field_value(+Name, +Value)//

field_value(_, set_cookie(Name, Value, Options)) -->
    !,
    atom(Name), "=", atom(Value),
    value_options(Options, cookie).
field_value(_, disposition(Disposition, Options)) -->
    !,
    atom(Disposition), value_options(Options, disposition).
field_value(www_authenticate, Auth) -->
    auth_field_value(Auth).
field_value(_, Atomic) -->
    atom(Atomic).

%!  auth_field_value(+AuthValue)//
%
%   Emit the authentication requirements (WWW-Authenticate field).

auth_field_value(negotiate(Data)) -->
    "Negotiate ",
    { base64(Data, DataBase64),
      atom_codes(DataBase64, Codes)
    },
    string(Codes).
auth_field_value(negotiate) -->
    "Negotiate".
auth_field_value(basic) -->
    !,
    "Basic".
auth_field_value(basic(Realm)) -->
    "Basic Realm=\"", atom(Realm), "\"".
auth_field_value(digest) -->
    !,
    "Digest".
auth_field_value(digest(Details)) -->
    "Digest ", atom(Details).

%!  value_options(+List, +Field)//
%
%   Emit field parameters such as =|; charset=UTF-8|=.  There
%   are three versions: a plain _key_ (`secure`), _token_ values
%   and _quoted string_ values.  Seems we cannot deduce that from
%   the actual value.

value_options([], _) --> [].
value_options([H|T], Field) -->
    "; ", value_option(H, Field),
    value_options(T, Field).

value_option(secure=true, cookie) -->
    !,
    "secure".
value_option(Name=Value, Type) -->
    { string_option(Name, Type) },
    !,
    atom(Name), "=",
    qstring(Value).
value_option(Name=Value, Type) -->
    { token_option(Name, Type) },
    !,
    atom(Name), "=", atom(Value).
value_option(Name=Value, _Type) -->
    atom(Name), "=",
    option_value(Value).

string_option(filename, disposition).

token_option(path, cookie).

option_value(Value) -->
    { number(Value) },
    !,
    number(Value).
option_value(Value) -->
    { (   atom(Value)
      ->  true
      ;   string(Value)
      ),
      forall(string_code(_, Value, C),
             token_char(C))
    },
    !,
    atom(Value).
option_value(Atomic) -->
    qstring(Atomic).

qstring(Atomic) -->
    { string_codes(Atomic, Codes) },
    "\"",
    qstring_codes(Codes),
    "\"".

qstring_codes([]) --> [].
qstring_codes([H|T]) --> qstring_code(H), qstring_codes(T).

qstring_code(C) --> {qstring_esc(C)}, !, "\\", [C].
qstring_code(C) --> [C].

qstring_esc(0'").
qstring_esc(C) :- ctl(C).


                 /*******************************
                 *        ACCEPT HEADERS        *
                 *******************************/

:- dynamic accept_cache/2.
:- volatile accept_cache/2.

parse_accept(Codes, Media) :-
    atom_codes(Atom, Codes),
    (   accept_cache(Atom, Media0)
    ->  Media = Media0
    ;   phrase(accept(Media0), Codes),
        keysort(Media0, Media1),
        pairs_values(Media1, Media2),
        assertz(accept_cache(Atom, Media2)),
        Media = Media2
    ).

%!  accept(-Media)// is semidet.
%
%   Parse an HTTP Accept: header

accept([H|T]) -->
    blanks,
    media_range(H),
    blanks,
    (   ","
    ->  accept(T)
    ;   {T=[]}
    ).

media_range(s(SortQuality,Spec)-media(Type, TypeParams, Quality, AcceptExts)) -->
    media_type(Type),
    blanks,
    (   ";"
    ->  blanks,
        parameters_and_quality(TypeParams, Quality, AcceptExts)
    ;   { TypeParams = [],
          Quality = 1.0,
          AcceptExts = []
        }
    ),
    { SortQuality is float(-Quality),
      rank_specialised(Type, TypeParams, Spec)
    }.


%!  content_disposition(-Disposition)//
%
%   Parse Content-Disposition value

content_disposition(disposition(Disposition, Options)) -->
    token(Disposition), blanks,
    value_parameters(Options).

%!  parse_content_type(-Type)//
%
%   Parse  Content-Type  value  into    a  term  media(Type/SubType,
%   Parameters).

parse_content_type(media(Type, Parameters)) -->
    media_type(Type), blanks,
    value_parameters(Parameters).


%!  rank_specialised(+Type, +TypeParam, -Key) is det.
%
%   Although the specification linked  above   is  unclear, it seems
%   that  more  specialised  types  must   be  preferred  over  less
%   specialized ones.
%
%   @tbd    Is there an official specification of this?

rank_specialised(Type/SubType, TypeParams, v(VT, VS, SortVP)) :-
    var_or_given(Type, VT),
    var_or_given(SubType, VS),
    length(TypeParams, VP),
    SortVP is -VP.

var_or_given(V, Val) :-
    (   var(V)
    ->  Val = 0
    ;   Val = -1
    ).

media_type(Type/SubType) -->
    type(Type), "/", type(SubType).

type(_) -->
    "*",
    !.
type(Type) -->
    token(Type).

parameters_and_quality(Params, Quality, AcceptExts) -->
    token(Name),
    blanks, "=", blanks,
    (   { Name == q }
    ->  float(Quality), blanks,
        value_parameters(AcceptExts),
        { Params = [] }
    ;   { Params = [Name=Value|T] },
        parameter_value(Value),
        blanks,
        (   ";"
        ->  blanks,
            parameters_and_quality(T, Quality, AcceptExts)
        ;   { T = [],
              Quality = 1.0,
              AcceptExts = []
            }
        )
    ).

%!  value_parameters(-Params:list) is det.
%
%   Accept (";" <parameter>)*, returning a list of Name=Value, where
%   both Name and Value are atoms.

value_parameters([H|T]) -->
    ";",
    !,
    blanks, token(Name), blanks,
    (   "="
    ->  blanks,
        (   token(Value)
        ->  []
        ;   quoted_string(Value)
        ),
        { H = (Name=Value) }
    ;   { H = Name }
    ),
    blanks,
    value_parameters(T).
value_parameters([]) -->
    [].

parameter_value(Value) --> token(Value), !.
parameter_value(Value) --> quoted_string(Value).


%!  token(-Name)// is semidet.
%
%   Process an HTTP header token from the input.

token(Name) -->
    token_char(C1),
    token_chars(Cs),
    { atom_codes(Name, [C1|Cs]) }.

token_chars([H|T]) -->
    token_char(H),
    !,
    token_chars(T).
token_chars([]) --> [].

token_char(C) :-
    \+ ctl(C),
    \+ separator_code(C).

ctl(C) :- between(0,31,C), !.
ctl(127).

separator_code(0'().
separator_code(0')).
separator_code(0'<).
separator_code(0'>).
separator_code(0'@).
separator_code(0',).
separator_code(0';).
separator_code(0':).
separator_code(0'\\).
separator_code(0'").
separator_code(0'/).
separator_code(0'[).
separator_code(0']).
separator_code(0'?).
separator_code(0'=).
separator_code(0'{).
separator_code(0'}).
separator_code(0'\s).
separator_code(0'\t).

term_expansion(token_char(x) --> [x], Clauses) :-
    findall((token_char(C)-->[C]),
            (   between(0, 255, C),
                token_char(C)
            ),
            Clauses).

token_char(x) --> [x].

%!  quoted_string(-Text)// is semidet.
%
%   True if input starts with a quoted string representing Text.

quoted_string(Text) -->
    "\"",
    quoted_text(Codes),
    { atom_codes(Text, Codes) }.

quoted_text([]) -->
    "\"",
    !.
quoted_text([H|T]) -->
    "\\", !, [H],
    quoted_text(T).
quoted_text([H|T]) -->
    [H],
    !,
    quoted_text(T).


%!  header_fields(+Fields, ?ContentLength)// is det.
%
%   Process a sequence of  [Name(Value),   ...]  attributes  for the
%   header. A term content_length(Len) is   special. If instantiated
%   it emits the header. If not   it just unifies ContentLength with
%   the argument of the content_length(Len)   term.  This allows for
%   both sending and retrieving the content-length.

header_fields([], _) --> [].
header_fields([content_length(CLen)|T], CLen) -->
    !,
    (   { var(CLen) }
    ->  ""
    ;   header_field(content_length, CLen)
    ),
    header_fields(T, CLen).           % Continue or return first only?
header_fields([status(_)|T], CLen) -->   % handled by vstatus//3.
    !,
    header_fields(T, CLen).
header_fields([H|T], CLen) -->
    { H =.. [Name, Value] },
    header_field(Name, Value),
    header_fields(T, CLen).


%!  field_name(?PrologName)
%
%   Convert between prolog_name  and  HttpName.   Field  names  are,
%   according to RFC 2616, considered  tokens   and  covered  by the
%   following definition:
%
%   ==
%   token          = 1*<any CHAR except CTLs or separators>
%   separators     = "(" | ")" | "<" | ">" | "@"
%                  | "," | ";" | ":" | "\" | <">
%                  | "/" | "[" | "]" | "?" | "="
%                  | "{" | "}" | SP | HT
%   ==

:- public
    field_name//1.

field_name(Name) -->
    { var(Name) },
    !,
    rd_field_chars(Chars),
    { atom_codes(Name, Chars) }.
field_name(mime_version) -->
    !,
    "MIME-Version".
field_name(www_authenticate) -->
    !,
    "WWW-Authenticate".
field_name(Name) -->
    { atom_codes(Name, Chars) },
    wr_field_chars(Chars).

rd_field_chars_no_fold([C|T]) -->
    [C],
    { rd_field_char(C, _) },
    !,
    rd_field_chars_no_fold(T).
rd_field_chars_no_fold([]) -->
    [].

rd_field_chars([C0|T]) -->
    [C],
    { rd_field_char(C, C0) },
    !,
    rd_field_chars(T).
rd_field_chars([]) -->
    [].

%!  separators(-CharCodes) is det.
%
%   CharCodes is a list of separators according to RFC2616

separators("()<>@,;:\\\"/[]?={} \t").

term_expansion(rd_field_char('expand me',_), Clauses) :-

    Clauses = [ rd_field_char(0'-, 0'_)
              | Cls
              ],
    separators(SepString),
    string_codes(SepString, Seps),
    findall(rd_field_char(In, Out),
            (   between(32, 127, In),
                \+ memberchk(In, Seps),
                In \== 0'-,         % 0'
                code_type(Out, to_lower(In))),
            Cls).

rd_field_char('expand me', _).                  % avoid recursion

wr_field_chars([C|T]) -->
    !,
    { code_type(C, to_lower(U)) },
    [U],
    wr_field_chars2(T).
wr_field_chars([]) -->
    [].

wr_field_chars2([]) --> [].
wr_field_chars2([C|T]) -->              % 0'
    (   { C == 0'_ }
    ->  "-",
        wr_field_chars(T)
    ;   [C],
        wr_field_chars2(T)
    ).

%!  now//
%
%   Current time using rfc_date//1.

now -->
    { get_time(Time)
    },
    rfc_date(Time).

%!  rfc_date(+Time)// is det.
%
%   Write time according to RFC1123 specification as required by the
%   RFC2616 HTTP protocol specs.

rfc_date(Time, String, Tail) :-
    stamp_date_time(Time, Date, 'UTC'),
    format_time(codes(String, Tail),
                '%a, %d %b %Y %T GMT',
                Date, posix).

%!  http_timestamp(+Time:timestamp, -Text:atom) is det.
%
%   Generate a description of a Time in HTTP format (RFC1123)

http_timestamp(Time, Atom) :-
    stamp_date_time(Time, Date, 'UTC'),
    format_time(atom(Atom),
                '%a, %d %b %Y %T GMT',
                Date, posix).


                 /*******************************
                 *         REQUEST DCG          *
                 *******************************/

request(Fd, [method(Method),request_uri(ReqURI)|Header]) -->
    method(Method),
    blanks,
    nonblanks(Query),
    { atom_codes(ReqURI, Query),
      request_uri_parts(ReqURI, Header, Rest)
    },
    request_header(Fd, Rest),
    !.
request(Fd, [unknown(What)|Header]) -->
    string(What),
    eos,
    !,
    {   http_read_header(Fd, Header)
    ->  true
    ;   Header = []
    }.

method(get)     --> "GET", !.
method(put)     --> "PUT", !.
method(head)    --> "HEAD", !.
method(post)    --> "POST", !.
method(delete)  --> "DELETE", !.
method(patch)   --> "PATCH", !.
method(options) --> "OPTIONS", !.
method(trace)   --> "TRACE", !.

%!  request_uri_parts(+RequestURI, -Parts, ?Tail) is det.
%
%   Process the request-uri, producing the following parts:
%
%     * path(-Path)
%     Decode path information (always present)
%     * search(-QueryParams)
%     Present if there is a ?name=value&... part of the request uri.
%     QueryParams is a Name=Value list.
%     * fragment(-Fragment)
%     Present if there is a #Fragment.

request_uri_parts(ReqURI, [path(Path)|Parts], Rest) :-
    uri_components(ReqURI, Components),
    uri_data(path, Components, PathText),
    uri_encoded(path, Path, PathText),
    phrase(uri_parts(Components), Parts, Rest).

uri_parts(Components) -->
    uri_search(Components),
    uri_fragment(Components).

uri_search(Components) -->
    { uri_data(search, Components, Search),
      nonvar(Search),
      catch(uri_query_components(Search, Query),
            error(syntax_error(_),_),
            fail)
    },
    !,
    [ search(Query) ].
uri_search(_) --> [].

uri_fragment(Components) -->
    { uri_data(fragment, Components, String),
      nonvar(String),
      !,
      uri_encoded(fragment, Fragment, String)
    },
    [ fragment(Fragment) ].
uri_fragment(_) --> [].

%!  request_header(+In:istream, -Header:list) is det.
%
%   Read the remainder (after the request-uri)   of  the HTTP header
%   and return it as a Name(Value) list.

request_header(_, []) -->               % Old-style non-version header
    blanks,
    eos,
    !.
request_header(Fd, [http_version(Version)|Header]) -->
    http_version(Version),
    blanks,
    eos,
    !,
    {   Version = 1-_
    ->  http_read_header(Fd, Header)
    ;   Header = []
    }.

http_version(Version) -->
    blanks,
    "HTTP/",
    http_version_number(Version).

http_version_number(Major-Minor) -->
    integer(Major),
    ".",
    integer(Minor).


                 /*******************************
                 *            COOKIES           *
                 *******************************/

%!  cookies(-List)// is semidet.
%
%   Translate a cookie description into a list Name=Value.

cookies([Name=Value|T]) -->
    blanks,
    cookie(Name, Value),
    !,
    blanks,
    (   ";"
    ->  cookies(T)
    ;   { T = [] }
    ).
cookies(List) -->
    string(Skipped),
    ";",
    !,
    { print_message(warning, http(skipped_cookie(Skipped))) },
    cookies(List).
cookies([]) -->
    blanks.

cookie(Name, Value) -->
    cookie_name(Name),
    blanks, "=", blanks,
    cookie_value(Value).

cookie_name(Name) -->
    { var(Name) },
    !,
    rd_field_chars_no_fold(Chars),
    { atom_codes(Name, Chars) }.

cookie_value(Value) -->
    quoted_string(Value),
    !.
cookie_value(Value) -->
    chars_to_semicolon_or_blank(Chars),
    { atom_codes(Value, Chars)
    }.

chars_to_semicolon_or_blank([]), ";" -->
    ";",
    !.
chars_to_semicolon_or_blank([]) -->
    " ",
    blanks,
    eos,
    !.
chars_to_semicolon_or_blank([H|T]) -->
    [H],
    !,
    chars_to_semicolon_or_blank(T).
chars_to_semicolon_or_blank([]) -->
    [].

set_cookie(set_cookie(Name, Value, Options)) -->
    ws,
    cookie(Name, Value),
    cookie_options(Options).

cookie_options([H|T]) -->
    ws,
    ";",
    ws,
    cookie_option(H),
    !,
    cookie_options(T).
cookie_options([]) -->
    ws.

ws --> " ", !, ws.
ws --> [].


%!  cookie_option(-Option)// is semidet.
%
%   True if input represents a valid  Cookie option. Officially, all
%   cookie  options  use  the  syntax   <name>=<value>,  except  for
%   =Secure= and =HttpOnly=.
%
%   @param  Option  Term of the form Name=Value
%   @bug    Incorrectly accepts options without = for M$ compatibility.

cookie_option(Name=Value) -->
    rd_field_chars(NameChars), ws,
    { atom_codes(Name, NameChars) },
    (   "="
    ->  ws,
        chars_to_semicolon(ValueChars),
        { atom_codes(Value, ValueChars)
        }
    ;   { Value = true }
    ).

chars_to_semicolon([H|T]) -->
    [H],
    { H \== 32, H \== 0'; },
    !,
    chars_to_semicolon(T).
chars_to_semicolon([]), ";" -->
    ws, ";",
    !.
chars_to_semicolon([H|T]) -->
    [H],
    chars_to_semicolon(T).
chars_to_semicolon([]) -->
    [].

%!  range(-Range)// is semidet.
%
%   Process the range header value. Range is currently defined as:
%
%       * bytes(From, To)
%       Where From is an integer and To is either an integer or
%       the atom =end=.

range(bytes(From, To)) -->
    "bytes", whites, "=", whites, integer(From), "-",
    (   integer(To)
    ->  ""
    ;   { To = end }
    ).


                 /*******************************
                 *           REPLY DCG          *
                 *******************************/

%!  reply(+In, -Reply:list)// is semidet.
%
%   Process the first line of an HTTP   reply.  After that, read the
%   remainder  of  the  header  and    parse  it.  After  successful
%   completion, Reply contains the following fields, followed by the
%   fields produced by http_read_header/2.
%
%       * http_version(Major-Minor)
%       * status(Code, Status, Comment)
%         `Code` is an integer between 100 and 599.
%         `Status` is a Prolog internal name.
%         `Comment` is the comment following the code
%         as it appears in the reply's HTTP status line.
%         @see status_number//2.

reply(Fd, [http_version(HttpVersion), status(Code, Status, Comment)|Header]) -->
    http_version(HttpVersion),
    blanks,
    (   status_number(Status, Code)
    ->  []
    ;   integer(Status)
    ),
    blanks,
    string(CommentCodes),
    blanks_to_nl,
    !,
    blanks,
    { atom_codes(Comment, CommentCodes),
      http_read_header(Fd, Header)
    }.


                 /*******************************
                 *            READ HEADER       *
                 *******************************/

%!  http_read_header(+Fd, -Header) is det.
%
%   Read Name: Value lines from FD until an empty line is encountered.
%   Field-name are converted to Prolog conventions (all lower, _ instead
%   of -): Content-Type: text/html --> content_type(text/html)

http_read_header(Fd, Header) :-
    read_header_data(Fd, Text),
    http_parse_header(Text, Header).

read_header_data(Fd, Header) :-
    read_line_to_codes(Fd, Header, Tail),
    read_header_data(Header, Fd, Tail),
    debug(http(header), 'Header = ~n~s~n', [Header]).

read_header_data([0'\r,0'\n], _, _) :- !.
read_header_data([0'\n], _, _) :- !.
read_header_data([], _, _) :- !.
read_header_data(_, Fd, Tail) :-
    read_line_to_codes(Fd, Tail, NewTail),
    read_header_data(Tail, Fd, NewTail).

%!  http_parse_header(+Text:codes, -Header:list) is det.
%
%   Header is a list of Name(Value)-terms representing the structure
%   of the HTTP header in Text.
%
%   @error domain_error(http_request_line, Line)

http_parse_header(Text, Header) :-
    phrase(header(Header), Text),
    debug(http(header), 'Field: ~p', [Header]).

header(List) -->
    header_field(Name, Value),
    !,
    { mkfield(Name, Value, List, Tail)
    },
    blanks,
    header(Tail).
header([]) -->
    blanks,
    eos,
    !.
header(_) -->
    string(S), blanks_to_nl,
    !,
    { string_codes(Line, S),
      syntax_error(http_parameter(Line))
    }.

%!  address//
%
%   Emit the HTML for the server address on behalve of error and
%   status messages (non-200 replies).  Default is
%
%       ==
%       SWI-Prolog httpd at <hostname>
%       ==
%
%   The address can be modified by   providing  a definition for the
%   multifile predicate http:http_address//0.

:- multifile
    http:http_address//0.

address -->
    http:http_address,
    !.
address -->
    { gethostname(Host) },
    html(address([ a(href('http://www.swi-prolog.org'), 'SWI-Prolog'),
                   ' httpd at ', Host
                 ])).

mkfield(host, Host:Port, [host(Host),port(Port)|Tail], Tail) :- !.
mkfield(Name, Value, [Att|Tail], Tail) :-
    Att =.. [Name, Value].

%!  http:http_address// is det.
%
%   HTML-rule that emits the location of  the HTTP server. This hook
%   is called from address//0 to customise   the server address. The
%   server address is emitted on non-200-ok replies.

%!  http:status_page(+Status, +Context, -HTMLTokens) is semidet.
%
%   Hook called by http_status_reply/4  and http_status_reply/5 that
%   allows for emitting custom error pages   for  the following HTTP
%   page types:
%
%     - 201 - created(Location)
%     - 301 - moved(To)
%     - 302 - moved_temporary(To)
%     - 303 - see_other(To)
%     - 400 - bad_request(ErrorTerm)
%     - 401 - authorise(AuthMethod)
%     - 403 - forbidden(URL)
%     - 404 - not_found(URL)
%     - 405 - method_not_allowed(Method,URL)
%     - 406 - not_acceptable(Why)
%     - 500 - server_error(ErrorTerm)
%     - 503 - unavailable(Why)
%
%   The hook is tried twice,  first   using  the  status term, e.g.,
%   not_found(URL) and than with the code,   e.g.  `404`. The second
%   call is deprecated and only exists for compatibility.
%
%   @arg    Context is the 4th argument of http_status_reply/5, which
%           is invoked after raising an exception of the format
%           http_reply(Status, HeaderExtra, Context).  The default
%           context is `[]` (the empty list).
%   @arg    HTMLTokens is a list of tokens as produced by html//1.
%           It is passed to print_html/2.


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message//1,
    prolog:error_message//1.

prolog:error_message(http_write_short(Data, Sent)) -->
    data(Data),
    [ ': remote hangup after ~D bytes'-[Sent] ].
prolog:error_message(syntax_error(http_request(Request))) -->
    [ 'Illegal HTTP request: ~s'-[Request] ].
prolog:error_message(syntax_error(http_parameter(Line))) -->
    [ 'Illegal HTTP parameter: ~s'-[Line] ].

prolog:message(http(skipped_cookie(S))) -->
    [ 'Skipped illegal cookie: ~s'-[S] ].

data(bytes(MimeType, _Bytes)) -->
    !,
    [ 'bytes(~p, ...)'-[MimeType] ].
data(Data) -->
    [ '~p'-[Data] ].
