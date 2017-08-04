:- module(
  html_ext,
  [
    alert//2,               % +Mode:oneof([danger,info,success,warning])
                            % :Html_0
    anchor//2,              % +Name, :Header_0
    between//2,             % :Outer_0, :Middle_0
    between//3,             % :Begin_0, :Middle_0, :End_0
    code_link//1,           % +Uri
    collapse_content//2,    % +Id, :Html_0
    collapse_link//2,       % +Id, :Link_0
    copy_to_clipboard//1,   % +Txt
    date//1,                % +DT
    default//3,             % :Html_1, :DefHtml_1, +Arg1
    definition_list//1,     % +L
    definition_list//2,     % :Html_1, +L
    definition_list//3,     % +Attrs, :Html_1, +L
    developed_with//0,
    dict//1,                % +Dict
    endpoint_link//1,       % +HandleId
    endpoint_link//2,       % +HandleId, :Html_0
    error//1,               % +E
    facebook_app_id//0,
    facebook_comments//1,   % +Uri
    facebook_follow_txt//0,
    facebook_follow_txt//1, % +User
    facebook_like//1,       % +Uri
    facebook_like//2,       % +Size:oneof([large,small]), +Uri
    facebook_share//2,      % +Uri, +Title
    figure//2,              % +Uri, +Caption
    figure//3,              % +Uri, +Alt, :Caption_0
    file_name//1,           % +File
    footnote_post//2,       % +State, :Html_0
    footnote_receive//2,    % +N, :Html_0
    footnotes//0,
    form_submit_button//0,
    form_submit_button//1,  % :Html_0
    git_version//0,
    google_font//1,         % +Name
    html_bracketed//1,      % :Html_0
    html_call//3,           % :Html_2, +Arg1, +Arg2
    html_call//4,           % :Html_3, +Arg1, +Arg2, +Arg3
    html_catch//1,          % :Html_0
    html_dq//1,             % :Html_0
    html_float//1,          % +Float
    html_http_error_page/2, % +Style, +Req
    html_license//2,        % +Uri, +Lbl
    html_maplist//3,        % :Html_1, +Args1, +Args2
    html_pair//1,           % +Pair
    html_pair//2,           % +Arg1, Arg2
    html_pair//3,           % :Html_1, +Arg1, +Arg2
    html_quad//4,           % +Arg1, +Arg2, +Arg3, +Arg4
    html_quad//5,           % :Html_1, +Arg1, +Arg2, +Arg3, +Arg4
    html_select//2,         % :ItemGen_1, :Html_1
    html_select//3,         % +Attrs, :ItemGen_1, :Html_1
    html_sq//1,             % :Html_0
    html_triple//3,         % +Arg1, +Arg2, +Arg3
    html_triple//4,         % :Html_1, +Arg1, +Arg2, +Arg3
    html_tuple//1,          % +Arg
    html_tuple//2,          % :Html_1, +Args
    html_to_atom/2,         % :Html_0, -A
    human_integer//1,       % +N
    idle//1,                % +Time
    if_then//2,             % :If_0, :Then_0
    if_then_else//3,        % :If_0, :Then_0, :Else_0
    image_header//2,        % +Img, :Html_0
    input_boolean//1,       % +Name
    input_checkbox//2,      % +Name, +Attrs
    input_file//2,          % +Name, +Attrs
    input_file//3,          % +Name, +Attrs, +Opts
    input_hidden//2,        % +Name, +Attrs
    input_password//2,      % +Name, +Attrs
    input_password//3,      % +Name, +Attrs, +Opts
    input_radio//2,         % +Name, +L
    input_text//2,          % +Name, +Attrs
    input_text//3,          % +Name, +Attrs, +Opts
    insert_raw_body//1,     % +Spec
    ip//1,                  % +Ip
    link_button//2,         % +Uri, :Html_0
    linkedin_share//0,
    list//1,                % +Args
    list//2,                % :Html_1, +Args
    meta//2,                % +Name, +Content
    meta_charset//0,
    meta_ie_latest//0,
    meta_license//1,        % +Uri
    meta_viewport//0,
    nonvar//1,              % :Html_0
    nonvar//2,              % :Html_1, +Arg1
    number//2,              % +Format, +N
    once//1,                % :Html_0
    panel//3,               % +In, :Header_0, :Body_0
    panel//4,               % +Open:boolean, +In, :Header_0, :Body_0
    panels//1,              % :Panels_0
    pl_version//0,
    postscriptum//1,        % :Content_2
    ref//2,                 % +Label, :Html_0
    reply_raw_file/1,       % +Spec
    reset_button//0,
    reset_button//1,        % :Html_0
    row_2//2,               % :ContentA_0, :ContentB_0
    row_2//3,               % +Attrs, :ContentA_0, :ContentB_0
    row_2//4,               % +WidthsA, :ContentA_0, +WidthsB, :ContentB_0
    row_2//5,               % +Attrs, +WidthsA, :ContentA_0, +WidthsB, :ContentB_0
    row_4//4,               % :ContentA_0, :ContentB_0, :ContentC_0, :ContentD_0
    row_4//8,               % +WidthsA, :ContentA_0, +WidthsB, :ContentB_0
                            % +WidthsC, :ContentC_0, +WidthsD, :ContentD_0
    search_box//1,          % +Action
    search_box//2,          % +Attrs, +Action
    search_result//2,       % +Result, :Html_1
    streamer//1,            % :Html_0
    table_tree//1,          % +Tree
    table_tree//2,          % :CellHtml_1, +Tree
    table_trees//1,         % +Trees
    table_trees//2,         % :CellHtml_1, +Trees
    term//1,                % @Term
    truncate//2,            % +Str, +Max
    twitter_follow_txt//0,
    twitter_follow_txt//1,  % +User
    twitter_grid//2,        % +Uri, +Title
    twitter_mention//0,
    twitter_mention//1,     % +User
    twitter_profile//0,
    twitter_profile//1,     % +User
    twitter_tweet//1,       % +Uri
    unless//2,              % :Unless_0, :Then_0
    upload_form//1          % +Spec
  ]
).
:- reexport(library(http/html_head)).
:- reexport(library(http/html_write)).
:- reexport(library(http/js_write)).

/** <module> HTML extensions

Raw HTML can be included using quasi-quoting:

```
html({|html||...|}).
```

@author Wouter Beek
@version 2016/02-2017/03
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(html/html_pagination)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_user)).
:- use_module(library(http/json)).
:- use_module(library(licenses)).
:- use_module(library(list_ext)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(pair_ext)).
:- use_module(library(pl_ext)).
:- use_module(library(rdf/rdf_term)). % @hack
:- use_module(library(setting_ext)).
:- use_module(library(string_ext)).
:- use_module(library(typecheck)).
:- use_module(library(uri/uri_ext)).

:- html_meta
   alert(+, html, ?, ?),
   anchor(+, html, ?, ?),
   between(html, html, ?, ?),
   between(html, html, html, ?, ?),
   collapse_content(+, html, ?, ?),
   collapse_link(+, html, ?, ?),
   definition_list(html, +, ?, ?),
   definition_list(+, html, +, ?, ?),
   endpoint_link(+, html, ?, ?),
   facebook_follow0(+, html, ?, ?),
   figure(+, +, html, ?, ?),
   footnote_post(+, html, ?, ?),
   footnote_receive(+, html, ?, ?),
   form_submit_button(html, ?, ?),
   html_bracketed(html, ?, ?),
   html_catch(html, ?, ?),
   html_dq(html, ?, ?),
   html_sq(html, ?, ?),
   html_to_atom(html, -),
   if_then(0, html, ?, ?),
   if_then_else(0, html, html, ?, ?),
   image_header(+, html, ?, ?),
   link_button(+, html, ?, ?),
   nonvar(html, ?, ?),
   panel(+, html, html, ?, ?),
   panel(+, +, html, html, ?, ?),
   panels(html, ?, ?),
   postscriptum(html, ?, ?),
   ref(+, html, ?, ?),
   reset_button(html, ?, ?),
   row_2(html, html, ?, ?),
   row_2(+, html, html, ?, ?),
   row_2(+, html, +, html, ?, ?),
   row_2(+, +, html, +, html, ?, ?),
   row_4(html, html, html, html, ?, ?),
   row_4(+, html, +, html, +, html, +, html, ?, ?),
   streamer(html, ?, ?),
   twitter_follow0(+, html, ?, ?),
   unless(0, html, ?, ?).

% Clipboard
:- if(debugging(js(clipboard))).
  :- html_resource(
       js(clipboard),
       [requires([js('clipboard-1.5.12.js')]),virtual(true)]
     ).
:- else.
  :- html_resource(
       js(clipboard),
       [requires([js('clipboard-1.5.12.min.js')]),virtual(true)]
     ).
:- endif.
:- html_resource(
     clipboard,
     [requires([js(clipboard)]),virtual(true)]
   ).

:- meta_predicate
    default(3, 3, +, ?, ?),
    html_call(4, +, +, ?, ?),
    html_call(5, +, +, +, ?, ?),
    html_maplist(4, +, +, ?, ?),
    html_pair(3, +, +, ?, ?),
    html_quad(3, +, +, +, +, ?, ?),
    html_select(1, 3, ?, ?),
    html_select(+, 1, 3, ?, ?),
    html_triple(3, +, +, +, ?, ?),
    html_tuple(3, +, ?, ?),
    if_then(0, 2, ?, ?),
    if_then_else(0, 2, 2, ?, ?),
    list(3, +, ?, ?),
    nonvar(3, +, ?, ?),
    once(3, ?, ?),
    search_result(+, 3, ?, ?),
    table_tree(3, +, ?, ?),
    table_tree(+, 3, +, ?, ?),
    table_tree_cell(+, 3, +, +, ?, ?),
    table_trees(3, +, ?, ?),
    table_trees(+, 3, +, ?, ?),
    unless(0, 2, ?, ?).





%! alert(+Mode:oneof([danger,info,success,warning]), :Html_0)// is det.

alert(Mode, Html_0) -->
  {atomic_list_concat([alert,Mode], -, C)},
  html(div([class=[alert,C],role=alert], Html_0)).



%! anchor(+Name, :Header_0)// is det.

anchor(Name, Header_0) -->
  html(a(name=Name, Header_0)).



%! between(:Outer_0, :Middle_0)// is det.
%! between(:Begin_0, :Middle_0, :End_0)// is det.

between(Outer_0, Middle_0) -->
  between(Outer_0, Middle_0, Outer_0).


between(Begin_0, Middle_0, End_0) -->
  html_call(Begin_0),
  html_call(Middle_0),
  html_call(End_0).



%! code_link(+Str)// is det.

code_link(Str) -->
  html([
    \html_requires(js(clipboard)),
    \js_script({|javascript(_)||
new Clipboard('.btn');
    |}),
    code(Str),
    " ",
    \copy_to_clipboard(Str)
  ]).



%! collapse_content(+Id, :Html_0)// is det.

collapse_content(Id, Html_0) -->
  html(
    div([class=collapse,id=Id],
      div(class=[card,'card-block'], Html_0)
    )
  ).



%! collapse_link(+Id, :Link_0)// is det.

collapse_link(Id, Link_0) -->
  {
    atomic_concat(#, Id, Frag),
    lstring(read_more, Str1),
    string_concat(Str1, "…", Str2)
  },
  tooltip(
    Str2,
    a([
      'aria-controls'=Id,
      'aria-expanded'=false,
      'data-toggle'=collapse,
      href=Frag
    ], Link_0
    )
  ).



%! copy_to_clipboard(+Txt)// is det.

copy_to_clipboard(Txt) -->
  button(['data-clipboard-text'=Txt], \icon(copy)).



%! date(+DT)// is det.

date(DT) -->
  {format_time(string(Str), "%+", DT)},
  html(Str).



%! default(:Html_1, :DefHtml_1, +Arg1)// is det.

default(Html_1, DefHtml_1, Arg1) -->
  (   {var_goal(Html_1)}
  ->  html_call(DefHtml_1, Arg1)
  ;   html_call(Html_1, Arg1)
  ).



%! definition_list(+L)// is det.
%! definition_list(:Html_1, +L)// is det.
%! definition_list(+Attrs, :Html_1, +L)// is det.
%
% Generates an HTML definition list.  Definition terms are of the form
% def(Definiens,Definiendum).

definition_list(L) -->
  definition_list(html, L).


definition_list(Html_1, L) -->
  definition_list([], Html_1, L).


definition_list(Attrs, Html_1, L) -->
  html(
    dl(Attrs,
      \html_maplist(definition_list_item(Html_1), L)
    )
  ).


definition_list_item(Html_1, L) -->
  {is_list(L)}, !,
  definition_list(Html_1, L).
definition_list_item(Html_1, def(Definiens,Definiendum)) -->
  html([
    dt(\html_call(Html_1, Definiens)),
    dd(\html_call(Html_1, Definiendum))
  ]).



developed_with -->
  html(
    div(class='developed-with', [
      \html_lstring(developed_with),
      " ",
      \prolog_link,
      "."
    ])
  ).



%! dict(+Dict)// is det.

dict(Dict) -->
  {dict_pairs(Dict, Tag, Pairs)},
  html(
    span(class=dict, [
      span(class='dict-tag', Tag),
      \html_set(html_pair, Pairs)
    ])
  ).



%! endpoint_link(+HandleId)// is det.
%! endpoint_link(+HandleId, :Html_0)// is det.

endpoint_link(HandleId) -->
  {http_link_to_id(HandleId, Uri)},
  internal_link(Uri, code(Uri)).


endpoint_link(HandleId, Html_0) -->
  {http_link_to_id(HandleId, Uri)},
  internal_link(Uri, Html_0).



%! error(+E)// is det.

error(error(Formal,Context)) -->
  {Formal =.. [ErrorKind|_]},
  html(
    div(class=error, [
      div(class='error-kind', ErrorKind),
      div(class='error-formal', [
        h3('Error formal:'),
        \nonvar(error_formal, Formal)
      ]),
      \nonvar(error_context, Context)
    ])
  ).


%! error_context(+Context)// is det.

error_context(context(Preds,Msg)) -->
  {is_list(Preds)}, !,
  html(
    div(class='error-context', [
      h3('Error context:'),
      \nested_predicate_sequence(Preds),
      \nonvar(Msg)
    ])
  ).
error_context(context(Pred,Msg)) -->
  html(
    div(class='error-context', [
      h3('Error context:'),
      \predicate(Pred),
      \nonvar(Msg)
    ])
  ).
error_context(Context) -->
  {atom(Context)}, !,
  html(span(class='error-context', [h3('Error context:'),Context])).


%! error_formal(+Formal)// is det.

% Domain error.
error_formal(domain_error(Type,Term)) --> !,
  {with_output_to(string(Str), write_term(Term))},
  html(
    span(class='domain-error', [
      "The term",
      span(class='error-term', Str),
      "is of the proper type (i.e., ",
      Type,
      "), but its value is outside of the domain of supported values."
    ])
  ).
% Existence error.
error_formal(existence_error(key,Key,Dict)) --> !,
  html(
    span(class='existence-error', [
      "Key ",
      span(class='dict-key', Key),
      " does not occur in dictionary ",
      \dict(Dict),
      "."
    ])
  ).
error_formal(existence_error(Type,Term)) --> !,
  html(
    span(class='existence-error', [
      "Term ",
      \term(Term),
      " is of the proper type (i.e., ",
      Type,
      ") and is of the correct domain, but there is no existing (external) resource represented by it."
    ])
  ).
% IO error.
error_formal(io_error(Mode,Stream)) --> !,
  html(
    span(class='io-error', [
      Mode,
      \error_stream(Stream)
    ])
  ).
% Instantiation error.
error_formal(instantiation_error) --> !,
  html(
    span(class='instantiation-error', [
      "Some terms are under-instantiated.",
      " I.e. they are not acceptable as is,",
      " but if some variables were bound to appropriate values ",
      " it would be acceptable."
    ])
  ).
error_formal(instantiation_error(Term)) --> !,
  html(
    span(class='instantiation-error', [
      "Term ",
      \term(Term),
      " is under-instantiated. I.e. it  is not acceptable as is,",
      " but if some variables were bound to appropriate values",
      " it would be acceptable."
    ])
  ).
% Limit exceeded.
error_formal(limit_exceeded(max_errors,Max)) --> !,
  html(
    span(class='limit-exceeded', [
      "Limit exceeded. Maximum number of errors (i.e., ",
      span(class='max-errors', Max),
     ") reached."
    ])
  ).
% MIME error.
error_formal(mime_error(_,MustBe,Is)) --> !,
  html(
    span(class='mime-error', [
      "Must be ",
      span(class=mime, MustBe),
      " not ",
      span(class=mime,Is)
    ])
  ).
% Permission error.
error_formal(permission_error(Action,Type,Term)) --> !,
  html(
    span(class='permission-error', [
      "It is not allowed to perform action ",
      Action,
      " on the object ",
      \term(Term),
      " that is of type ",
      Type,
      "."
    ])
  ).
% Process error.
error_formal(process_error(Program,exit(Status))) --> !,
  html(
    span(class='process-error', [
     "Process error: ",
      span(class=program, Program),
      " exited with status ",
      span(class='exit-status', [
        span(class='exit-status-code', Status),
        \exit_status_reason(Status)
      ])
    ])
  ).
% Representation error.
error_formal(representation_error(Reason)) --> !,
  html(
    span(class='representation-error', [
      "Representation error: ",
      span(class='error-reason', Reason)
    ])
  ).
error_formal(representation_error(Reason)) --> !,
  html(
    span(class='representation-error', [
      "A limitation of the current Prolog implementation is breached: ",
      span(class='error-reason', Reason)
    ])
  ).
% Resource error.
error_formal(resource_error(Reason)) --> !,
  html(
    span(class='resource-error', [
      "Resource error: ",
      span(class='error-reason', Reason)
    ])
  ).
% Shell error.
error_formal(shell_error(Culprit)) --> !,
  html(
    span(class='shell-error', [
      "The shell encountered the following error: ",
      \code(Culprit)
    ])
  ).
% Socket error.
error_formal(socket_error(Reason)) --> !,
  html(
    span(class='socket-error', [
      "Socket error: ",
      span(class='error-reason', Reason)
    ])
  ).
% Syntax error.
error_formal(syntax_error(Culprit)) --> !,
  html(
    span(class='syntax-error', [
      "The following contains invalid syntax: ",
      \code(Culprit),
      "."
    ])
  ).
% Timeout error.
error_formal(timeout_error(Mode,Stream)) --> !,
  html(
    span(class='timeout-error', [
      "Timeout error: ",
      Mode,
      \error_stream(Stream)
    ])
  ).
% Type error.
error_formal(type_error(Type,Term)) -->
  html(
    span(class='type-error', [
      "Term ",
      \term(Term),
      " is not of type ",
      Type,
      "."
    ])
  ).


%! error_stream(+Stream)// is det.

error_stream(Stream) -->
  {with_output_to(string(Str), write_canonical(Stream))},
  html(span(Str)).



%! facebook_app_id// is det.

facebook_app_id -->
  {setting(html:facebook_app_id, Id), ground(Id)}, !,
  html(meta([name="fb:app_id",content=Id], [])).
facebook_app_id --> [].



%! facebook_comments(+Uri)// is det.

facebook_comments(Uri) -->
  html(
    div([
      class='fb-comments',
      'data-colorscheme'=dark,
      'data-href'=Uri,
      'data-numposts'=5
    ], [])
  ).



%! facebook_follow_txt// is det.
%! facebook_follow_txt(+User)// is det.

facebook_follow_txt -->
  {setting_nonvar(html:facebook_profile, User)}, !,
  facebook_follow_txt(User).
facebook_follow_txt --> [].


facebook_follow_txt(User) -->
  {lstring(like_us_on_x, ["Facebook"], Str)},
  tooltip(Str, \facebook_follow0(User, facebook_txt0(User))).

facebook_txt0(User) -->
  html([
    \html_lstring(follow),
    " ",
    User,
    " ",
    \lstring(on),
    "Facebook"
  ]).



%! facebook_like(+Uri)// is det.
%! facebook_like(+Style:oneof([large,small]), +Uri)// is det.

facebook_like(Uri) -->
  facebook_like(small, Uri).


facebook_like(Size, Uri) -->
  html(
    div([
      class='fb-like',
      'data-action'=like,
      'data-colorsheme'=dark,
      'data-href'=Uri,
      'data-layout'=button_count,
      'data-share'=true,
      'data-show-faces'=false,
      'data-size'=Size
    ], [])
  ).



%! figure(+Uri, +Caption)// is det.
%! figure(+Uri, +Alt, :Caption_0)// is det.

figure(Uri, Caption) -->
  figure(Uri, Caption, html(Caption)).


figure(Uri, Alt, Caption_0) -->
  html(figure([\image(Uri, [alt=Alt]),figcaption(Caption_0)])).



%! file_name(+File)// is det.
%
% Generates an HTML description of the given file name.

file_name(File) -->
  html(span(class=file, File)).



%! footnote_post(+State, :Html_0)// is det.

footnote_post(State, Html_0) -->
  {
    dict_inc(footnote, State, N),
    format(atom(Ref), "ref~d", [N]),
    format(atom(Note), "#node~d", [N])
  },
  html([
    sup(id=Ref, a(href=Note, ["[",N,"]"])),
    \html_post(footnote, \footnote_receive(N, Html_0))
  ]).



%! footnote_receive(+N, :Html_0)// is det.

footnote_receive(N, Html_0) -->
  {
    format(atom(Ref), "#ref~d", [N]),
    format(atom(Note), "node~d", [N])
  },
  html(li(id=Note, [a(href=Ref, "^"), " ", Html_0])).



%! footnotes// is det.

footnotes -->
  html([hr([]), ol(\html_receive(footnote))]).



%! form_submit_button// is det.
%! form_submit_button(:Html_0)// is det.

form_submit_button -->
  form_submit_button("Submit").


form_submit_button(Html_0) -->
  form_button0([type=submit], Html_0).



%! git_version// is det.

git_version -->
  {git_version(Version)},
  html(["GIT version ",Version,")"]).



%! google_font(+Name)// is det.

google_font(Name) -->
  {uri_comps(Uri, uri(https,'fonts.googleapis.com',[css],[family(Name)],_))},
  link([type='text/css'], stylesheet-Uri).



%! how(+Mod, +How)// is det.
%
% | `dynamic(Line)`      | Declared dynamic at Line       |
% | `thread_local(Line)` | Declared thread local at Line  |
% | `multifile(Line)`    | Declared multifile at Line     |
% | `local(Line)`        | First clause at Line           |
% | `foreign(Line)`      | Foreign library loaded at Line |
% | `constraint(Line)`   | CHR Constraint at Line         |
% | `imported(File)`     | Imported from File             |
%
% @see http://swi-prolog.org/pldoc/doc_for?object=xref_defined/3

how(Mod, How) -->
  {How =.. [imported,_]}, !,
  html(span(class=how, ["imported",\nonvar(Mod)])).
how(_, How) -->
  {How =.. [HowClass,Line]},
  html(span(class=how, [HowClass,span(class=line, \html_thousands(Line))])).



%! html_bracketed(:Html_0)// is det.

html_bracketed(Html_0) -->
  html(["(",Html_0,")"]).



%! html_call(:Html_4, +Arg1, +Arg2)// is det.
%! html_call(:Html_5, +Arg1, +Arg2, +Arg3)// is det.

html_call(Html_4, Arg1, Arg2, X, Y) :-
  call(Html_4, Arg1, Arg2, X, Y).


html_call(Html_5, Arg1, Arg2, Arg3, X, Y) :-
  call(Html_5, Arg1, Arg2, Arg3, X, Y).



%! html_callable(+Mod, +Html_2)// is det.

html_callable(Mod, Html_2) -->
  {compound_name_arity(Html_2, Functor, Arity)},
  predicate(Mod:Functor/Arity).



%! html_catch(:Html_0)// is det.
%
% Either generates Html_0 or an HTML representation of an exception
% thrown by Html_0.

html_catch(Html_0, X, Y) :-
  catch(
    call(Html_0, X, Y),
    E,
    call(error(exception(E)), X, Y)
  ).



%! html_dq(:Html_0)// is det.

html_dq(Html_0) -->
  html(["“",Html_0,"”"]).



%! html_float(+Float)// is det.

html_float(Float) -->
  html("~G"-[Float]).



%! html_http_error_page(+Style, +Req) is det.

html_http_error_page(Style, Req) :-
  memberchk(status(Status), Req),
  http_error(Status, title, Lang, Title),
  http_error(Status, main, Lang, Main),
  reply_html_page(
    Style,
    \title(["Error",Title]),
    html(article([header(Title),section(Main)]))
  ).



%! html_license(+Uri, Lbl)// is det.

html_license(Uri, Lbl) -->
  external_link(Uri, [rel=license], Lbl).



%! html_maplist(:Html_2, +Args1, +Args2)// is det.

html_maplist(_, [], []) --> [].
html_maplist(Html_2, [H1|T1], [H2|T2]) -->
  html_call(Html_2, H1, H2),
  html_maplist(Html_2, T1, T2).



%! html_pair(+Pair)// is det.
%! html_pair(+Arg1, +Arg2)// is det.
%! html_pair(:Html_1, +Arg1, +Arg2)// is det.
%
% Generates an HTML representation for a pair.

html_pair(Arg1-Arg2) -->
  html_pair(Arg1, Arg2).


html_pair(Arg1, Arg2) -->
  tuple([Arg1,Arg2]).


html_pair(Html_1, Arg1, Arg2) -->
  tuple(Html_1, [Arg1,Arg2]).



%! html_quad(+Arg1, +Arg2, +Arg3, +Arg4)// is det.
%! html_quad(:Html_1, +Arg1, +Arg2, +Arg3, +Arg4)// is det.
%
% Generates an HTML representation for a triple.

html_quad(Arg1, Arg2, Arg3, Arg4) -->
  html_tuple([Arg1,Arg2,Arg3,Arg4]).


html_quad(Html_1, Arg1, Arg2, Arg3, Arg4) -->
  html_tuple(Html_1, [Arg1,Arg2,Arg3,Arg4]).



%! html_select(:ItemGen_1, :Html_1)// is det.
%! html_select(+Attrs, :ItemGen_1, :Html_1)// is det.

html_select(ItemGen_1, Html_1) -->
  html_select([], ItemGen_1, Html_1).


html_select(Attrs1, ItemGen_1, Html_1) -->
  {
    merge_attrs(Attrs1, [class='form-control'], Attrs2),
    findall(Item, call(ItemGen_1, Item), Items)
  },
  html(select(Attrs2, \html_maplist(Html_1, Items))).



%! html_sq(:Html_0)// is det.

html_sq(Html_0) -->
  html(["‘",Html_0,"’"]).



%! html_to_atom(:Html_0, -A) is det.

html_to_atom(Html_0, A) :-
  phrase(html(Html_0), Tokens),
  with_output_to(atom(A), print_html(Tokens)).



%! html_triple(+Arg1, +Arg2, +Arg3)// is det.
%! html_triple(:Html_1, +Arg1, +Arg2, +Arg3)// is det.
%
% Generates an HTML representation for a triple.

html_triple(Arg1, Arg2, Arg3) -->
  html_tuple([Arg1,Arg2,Arg3]).


html_triple(Html_1, Arg1, Arg2, Arg3) -->
  html_tuple(Html_1, [Arg1,Arg2,Arg3]).



%! html_tuple(+Args)// is det.
%! html_tuple(:Html_1, +Args)// is det.
%
% Generates an HTML representation for a tuple.

html_tuple(Args) -->
  html_tuple(html, Args).


html_tuple(Html_1, Args) -->
  html([&(lang),\html_seplist(Html_1, html(","), Args),&(rang)]).



%! human_integer(+N)// is det.

human_integer(inf) --> !,
  html("∞").
human_integer(N) -->
  {N < 1024}, !,
  html("~d"-[N]).
human_integer(N) -->
  {
    N < 1024 * 1024, !,
    KB is N / 1024,
    digits0(KB, N0)
  },
  html("~*fK"-[N0,KB]).
human_integer(N) -->
  {
    N < 1024 ** 3, !,
    MB is N / (1024 ** 2),
    digits0(MB, N0)
  },
  html("~*fM"-[N0,MB]).
human_integer(N) -->
  {
    TB is N / (1024 ** 3),
    digits0(TB, N0)
  },
  html("~*fG"-[N0,TB]).

digits0(Count, 1) :-
  Count < 100, !.
digits0(_, 0).



%! idle(+Time)// is det.

idle(Time) -->
  {
    Secs is round(Time),
    Min is Secs // 60,
    Sec is Secs mod 60
  },
  html("~`0t~d~2|:~`0t~d~5|"-[Min, Sec]).



%! if_then(:If_0, :Then_0)// is det.

if_then(If_0, Then_0) -->
  if_then_else(If_0, Then_0, []).



%! if_then_else(:If_0, :Then_0, :Else_0)// is det.

if_then_else(If_0, Then_0, Else_0) -->
  ({call(If_0)} -> html_call(Then_0) ; html_call(Else_0)).



%! image_header(+Uri, :Html_0)// is det.

image_header(Uri, Html_0) -->
  {
    format(atom(Img0), 'url("~a")', [Uri]),
    atomic_list_concat(
      [
        'background:',
        Img0,
        'no-repeat',
        scroll,
        center,
        top,
        '/',
        auto,
        '200px',
        'rgba(0,0,0,0);'
      ],
      ' ',
      BackgroundImg
    )
  },
  html(header(style=BackgroundImg, Html_0)).



%! input_boolean(+Name)// is det.
%! input_boolean(+Name, +Def)// is det.

input_boolean(Name) -->
  input_boolean(Name, false).


input_boolean(Name, Def) -->
  {input_boolean_default(Def, CheckedA, CheckedB)},
  input_radio(
    Name,
    [option(true,"Yes",CheckedA),option(false,"No",CheckedB)]
  ).


input_boolean_default(true, true, false).
input_boolean_default(false, false, true).



%! input_checkbox(+Name, +Attrs)// is det.

input_checkbox(Name, Attrs) -->
  form_input0(checkbox, Name, Attrs).



%! input_file(+Name, +Attrs)// is det.
%! input_file(+Name, +Attrs, +Opts)// is det.

input_file(Name, Attrs) -->
  form_input0(file, Name, Attrs).


input_file(Name, Attrs, Opts) -->
  form_input0(file, Name, Attrs, Opts).



%! input_hidden(+Name, +Attrs)// is det.

input_hidden(Name, Attrs) -->
  form_input0(hidden, Name, Attrs).



%! input_password(+Name, +Attrs)// is det.
%! input_password(+Name, +Attrs, +Opts)// is det.

input_password(Name, Attrs) -->
  form_input0(password, Name, Attrs).


input_password(Name, Attrs, Opts) -->
  form_input0(password, Name, Attrs, Opts).



%! input_radio(+Name, +L)// is det.

input_radio(Name, L) -->
  html_maplist(input_radio_option(Name), L).


input_radio_option(Name1, option(Name2,Lbl,Checked)) -->
  {(Checked == true -> T = [checked=checked] ; T = [])},
  html(
    label(class='radio-inline', [
      input([id=Name2,name=Name1,type=radio,value=Name2|T]),
      Lbl
    ])
  ).
    


%! input_text(+Name, +Attrs)// is det.
%! input_text(+Name, +Attrs, +Opts)// is det.

input_text(Name, Attrs) -->
  form_input0(text, Name, Attrs).


input_text(Name, Attrs, Opts) -->
  form_input0(text, Name, Attrs, Opts).



%! insert_raw_body(+Spec)// is det.
%
% Insert the content of an HTML file into the current document.  Only
% the content of the `body` element is included.

insert_raw_body(Spec) -->
  {raw_page(Spec, _, Content)},
  html(Content).



%! ip(+Ip)// is det.

ip(ip(A,B,C,D)) --> !,
  html("~d.~d.~d.~d"-[A,B,C,D]).
ip(IP) -->
  html("~w"-[IP]).



%! link_button(+Uri, :Html_0)// is det.
%
% Generate an HTML link that looks like a button.

link_button(Uri, Html_0) -->
  html(a([class=[btn,'btn-default'],href=Uri], Html_0)).



%! linkedin_share// is det.

linkedin_share -->
  html([
    script([src='//platform.linkedin.com/in.js',type='text/javascript'],
      'lang: en_US'
    ),
    script(['data-counter'=right,type='IN/Share'], [])
  ]).



%! list(+Args)// is det.
%! list(:Html_1, +Args)// is det.

list(Args) -->
  list(html_hook, Args).


list(Html_1, Args) -->
  html([&(91),\html_seplist(Html_1, Args),&(93)]).



%! meta_charset// is det.

meta_charset -->
  html(meta(charset='utf-8', [])).



%! meta_license(+Uri)// is det.

meta_license(Uri) -->
  link(license-Uri).



%! nonvar(:Html_0)// is det.
%! nonvar(:Html_1, +Arg1)// is det.

nonvar(Html_0) -->
  ({var_goal(Html_0)} -> [] ; html_call(Html_0)).


nonvar(Html_1, Arg1) -->
  ({var_goal(Html_1)} -> [] ; html_call(Html_1, Arg1)).



%! number(+Format, +N)// is det.
%
% HTML component to emit a number.

number(Fmt, N) -->
  {number(N)}, !,
  html(Fmt-[N]).



%! once(:Html_1)// is det.

once(Html_1, X, Y) :-
  once(html_call(Html_1, X, Y)).



%! operator(+Mod, +Op)// is det.

operator(Mod, op(Pred,Type,Name)) -->
  html([
    \nonvar(module_prefix, Mod),
    "op(",Pred,",",Type,",",Name,")"
  ]).



%! panel(+Id, :Header_0, :Html_0)// is det.
%! panel(+Open:boolean, +Id, :Header_0, :Html_0)// is det.

panel(Id, Header_0, Html_0) -->
  panel(false, Id, Header_0, Html_0).


panel(Open, Id, Header_0, Html_0) -->
  {
    panel_mode(Open, OpenClasses),
    atomic_list_concat([collapse,Id], -, CollapseId),
    atomic_concat(#, CollapseId, CollapseTarget),
    atomic_list_concat([panel,Id], -, PanelId)
  },
  html(
    div([class=[panel,'panel-default'],id=PanelId], [
      div(class='panel-heading',
        h4(class='panel-title',
          a([
            'data-toggle'=collapse,
            'data-target'=CollapseTarget,
            href=CollapseTarget
          ], Header_0)
        )
      ),
      div([class=[collapse,'panel-collapse'|OpenClasses],id=CollapseId],
        div(class='panel-body', Html_0)
      )
    ])
  ).


panel_mode(false, []).
panel_mode(true, [in]).



%! panels(:Panels_0)// is det.
%
% Bootstrap panels are generated within, see panel//[3,4].

panels(Panels_0) -->
  html(div([class='panel-group',id=accordion], Panels_0)).



%! pl_version// is det.

pl_version -->
  {pl_version(Version)},
  html(
    span(class='pl-version', [
      span(class=major, Version.major),
      ".",
      span(class=minor, Version.minor),
      ".",
      span(class=patch, Version.patch)
    ])
  ).



%! postscriptum(:Content_0)// is det.

postscriptum(Content_0) -->
  html([hr([])|Content_0]).



%! ref(+Label, :Html_0)// is det.

ref(Label, Html_0) -->
  {atom_concat('#', Label, Link)},
  html(a(href=Link, Html_0)).



%! reply_raw_file(+Spec) is det.
%
% Present an HTML file embedded using the server styling.  This is
% achieved by parsing the HTML and passing the parsed DOM to
% reply_html_page/3.

reply_raw_file(Spec) :-
  raw_page(Spec, Title, Content),
  reply_html_page(title(Title), Content).



%! reset_button// is det.
%! reset_button(:Html_0)// is det.

reset_button -->
  reset_button("Reset").


reset_button(Html_0) -->
  form_button0([type=reset], Html_0).



%! row_2(:ContentA_0, :ContentB_0)// is det.
%! row_2(+Attrs, :ContentA_0, :ContentB_0)// is det.
%! row_2(+WidthsA, :ContentA_0, +WidthsB, :ContentB_0)// is det.
%! row_2(+Attrs, +WidthsA, :ContentA_0, +WidthsB, :ContentB_0)// is det.

row_2(ContentA_0, ContentB_0) -->
  row_2([], ContentA_0, ContentB_0).


row_2(Attrs, ContentA_0, ContentB_0) -->
  row_2(Attrs, 6, ContentA_0, 6, ContentB_0).


row_2(WidthsA, ContentA_0, WidthsB, ContentB_0) -->
  row_2([], WidthsA, ContentA_0, WidthsB, ContentB_0).


row_2(Attrs1, WidthsA, ContentA_0, WidthsB, ContentB_0) -->
  {
    merge_attrs(Attrs1, [class=['container-fluid']], Attrs2),
    maplist(widths, [WidthsA,WidthsB], [ClassesA,ClassesB])
  },
  html(
    div(Attrs2,
      div(class=row, [
        div(class=[col|ClassesA], ContentA_0),
        div(class=[col|ClassesB], ContentB_0)
      ])
    )
  ).



%! row_4(:ContentA_0, :ContentB_0, :ContentC_0, :ContentD_0)// is det.
%! row_4(
%!   +WidthsA,
%!   :ContentA_0,
%!   +WidthsB,
%!   :ContentB_0,
%!   +WidthsC,
%!   :ContentC_0,
%!   +WidthsD,
%!   :ContentD_0
%! )// is det.

row_4(ContentA_0, ContentB_0, ContentC_0, ContentD_0) -->
  row_4(3, ContentA_0, 3, ContentB_0, 3, ContentC_0, 3, ContentD_0).

row_4(
  WidthsA, ContentA_0,
  WidthsB, ContentB_0,
  WidthsC, ContentC_0,
  WidthsD, ContentD_0
) -->
  {
    maplist(
      widths,
      [WidthsA,WidthsB,WidthsC,WidthsD],
      [ClassesA,ClassesB,ClassesC,ClassesD]
    )
  },
  html(
    div(class='container-fluid',
      div(class=row, [
        div(class=[col|ClassesA], ContentA_0),
        div(class=[col|ClassesB], ContentB_0),
        div(class=[col|ClassesC], ContentC_0),
        div(class=[col|ClassesD], ContentD_0)
      ])
    )
  ).



%! search_box(+Spec)// is det.
%! search_box(+Attrs, +Spec)// is det.
%
% For inclusion in the navigation bar use
% `class=['navbar-form','navbar-left']`.

search_box(Spec) -->
  search_box([], Spec).


search_box(Attrs1, Spec) -->
  {merge_attrs(Attrs1, [role=search], Attrs2)},
  form(
    Spec,
    Attrs2,
    div(class='input-group', [
      input([
        class='form-control',
        name=pattern,
        placeholder="Search for…",
        type=text
      ]),
      span(class='input-group-btn', \submit_button("Go!"))
    ])
  ).



%! search_result(+Result, :Html_1)// is det.
%
% Result must be a regular pagination resul plus the ‘pattern’ key
% containing the original search term.

search_result(Result, Html_1) -->
  html([
    h1([
      \search_result_number(Result),
      "earch results for search string ",
      \html_sq(Result.pattern),
      ":"
    ]),
    \html_pagination_result(Result, Html_1)
  ]).


search_result_number(Result) -->
  {get_dict(total_number_of_results, Result, TotalNumResults)}, !,
  html("~:D s"-[TotalNumResults]).
search_result_number(_) -->
  html("S").



%! streamer(:Html_0)// is det.

streamer(Html_0) -->
  html(div(class=streamer, Html_0)).



%! table_tree(+Tree)// is det.
%! table_tree(:CellHtml_1, +Tree)// is det.

table_tree(Tree) -->
  table_tree(html_hook, Tree).


table_tree(CellHtml_1, Tree) -->
  table_tree(false, CellHtml_1, Tree).


table_tree(false, CellHtml_1, t(Node,SubTrees)) --> !,
  html(
    tr([
      \table_tree_cell(true, CellHtml_1, Node, SubTrees),
      \table_trees(true, CellHtml_1, SubTrees)
    ])
  ).
table_tree(true, CellHtml_1, t(Node,SubTrees)) --> !,
  table_tree_cell(true, CellHtml_1, Node, SubTrees),
  table_trees(true, CellHtml_1, SubTrees).
table_tree(_, CellHtml_1, H) -->
  html(td(\html_call(CellHtml_1, H))).



table_tree_cell(InRow, CellHtml_1, L, _) -->
  {is_list(L)}, !,
  table_trees(InRow, CellHtml_1, L).
table_tree_cell(_, CellHtml_1, Node, L) -->
  {length(L, N)},
  html(td(rowspan(N), \html_call(CellHtml_1, Node))).



%! table_trees(+Trees)// is det.
%! table_trees(:CellHtml_1, +Trees)// is det.

table_trees(Trees) -->
  table_trees(html, Trees).


table_trees(CellHtml_1, Trees) -->
  table_trees(false, CellHtml_1, Trees).


table_trees(InRow, CellHtml_1, [t(Node,[H|T])|Trees]) --> !,
  table_tree(InRow, CellHtml_1, t(Node,[H|T])),
  table_trees(false, CellHtml_1, Trees).
table_trees(InRow, CellHtml_1, [H|T]) -->
  {is_list(H)}, !,
  html(td(\html_set(CellHtml_1, H))),
  table_trees(InRow, CellHtml_1, T).
table_trees(InRow, CellHtml_1, [H|T]) --> !,
  table_tree(InRow, CellHtml_1, H),
  table_trees(InRow, CellHtml_1, T).
table_trees(_, _, []) --> !, [].



%! term(@Term)// is det.

% Atom
term(A) -->
  {atom(A)}, !,
  html(A).
% Dict
term(Dict) -->
  {is_dict(Dict)}, !,
  dict(Dict).
% Float
term(Float) -->
  {float(Float)}, !,
  html_float(Float).
% Integer
term(Integer) -->
  {integer(Integer)}, !,
  html(Integer).
% Pair
term(X-Y) --> !,
  html_pair(X, Y).
% Stream position
term(stream_position(_,Line,Col,Char)) --> !,
  html(
    span([
      \html_thousands(Line),
      \html_thousands(Col),
      \html_thousands(Char)
    ])
  ).
% String
term(Str) -->
  {string(Str)}, !,
  html(Str).
% Variable
term(X) -->
  {var(X)}, !,
  html("var").
% Catchall
term(Term) -->
  {with_output_to(string(Str), write_term(Term))},
  html(Str).



%! truncate(+Str, +MaxLen)// is det.

truncate(Str, MaxLen) -->
  {string_truncate(Str, MaxLen, Prefix)},
  ({Str == Prefix} -> html(Str) ; tooltip(Str, Prefix)).



%! twitter_follow_txt// is det.
%! twitter_follow_txt(+User)// is det.

twitter_follow_txt -->
  {setting_nonvar(html:twitter_profile, User)}, !,
  twitter_follow_txt(User).
twitter_follow_txt --> [].


twitter_follow_txt(User) -->
  {lstring(follow_us_on_x, ["Twitter"], Str)},
  tooltip(Str, \twitter_follow0(User, twitter_txt0(User))).

twitter_txt0(User) -->
  html([
    \html_lstring(follow),
    " ",
    User,
    " ",
    \lstring(on),
    " Twitter"
  ]).



%! twitter_grid(+Uri, +Title)// is det.

twitter_grid(Uri, Title) -->
  html(a([class='twitter-grid',href=Uri], Title)).



%! twitter_mention// is det.
%! twitter_mention(+User)// is det.

twitter_mention -->
  {setting_nonvar(html:twitter_profile, User)}, !,
  twitter_mention(User).
twitter_mention --> [].


twitter_mention(User) -->
  {uri_comps(Uri, uri(https,'twitter.com',[intent,tweet],[screen_name(User)],_))},
  html(
    a(
      [class='twitter-mention-button','data-show-count'=false,href=Uri],
      [\html_lstring(tweet_to)," @",User]
    )
  ).



%! twitter_profile// is det.
%! twitter_profile(+User)// is det.

twitter_profile -->
  {setting_nonvar(html:twitter_profile, User)}, !,
  twitter_profile(User).
twitter_profile --> [].


twitter_profile(User) -->
  {twitter_user_uri0(User, Uri)},
  html(
    a([class='twitter-timeline',href=Uri], [
      \html_lstring(tweets_by),
      " ",
      User
    ])
  ).



%! twitter_tweet(+Tweet)// is det.

twitter_tweet(Tweet) -->
  {
    http_open2(
      uri(
	https,
	'publish.twitter.com',
	[oembed],
	[omit_script(true),url(Tweet)],
	_
      ),
      In,
      [request_header('Accept'='application/json')]
    ),
    json_read_dict(In, Dict, [value_string_as(atom)])
  },
  [Dict.html].



%! unless(:Unless_0, :Then_0)// is det.

unless(Unless_0, Then_0) -->
  ({call(Unless_0)} -> [] ; html_call(Then_0)).



%! upload_form(+Spec)// is det.
%
% Generated a Bootstrap-styled HTML form for uploading files.

upload_form(Spec) -->
  form(
    Spec,
    [enctype='multipart/form-data',method=post],
    [
      \input_file(filename, [size=50], [label("File")]),
      \input_text(dataset, [size=75], [label("Dataset name:")]),
      \input_text(graph, [size=75], [label("Graph name:")]),
      \submit_button("Upload")
    ]
  ).





% HELPERS %

%! bar// is det.

bar -->
  html(span(class='icon-bar', [])).



%! exit_status_reason(+Status)// is det.
%! exit_code_reason(?Status, ?Text) is nondet.

exit_status_reason(Status) -->
  (   {exit_code_reason(Status, Reason)}
  ->  html(span(class='exit-status-reason', Reason))
  ;   []
  ).


exit_code_reason(1, "Catchall for general/miscellaneous errors.").
exit_code_reason(2, "Misuse for general errors.").
exit_code_reason(126, "Command cannot be executed. Permission problem or \c
                       command is not an executable.").
exit_code_reason(127, "Command not found.").
exit_code_reason(128, "Invalid argument to the exit command; \c
                       only takes integer args in the range 0-255.").
exit_code_reason(130, "Script terminated by Control-C.").



%! form_button0(+Attrs, :Html_0)// is det.
%! form_button0(+Attrs, :Html_0, +Opts)// is det.

form_button0(Attrs, Html_0) -->
  form_button0(Attrs, Html_0, []).


form_button0(Attrs, Html_0, Opts) -->
  {
    option(widths_a(WidthsA), Opts, 4),
    option(widths_b(WidthsB), Opts, 8),
    widths(true, WidthsA, ClassesA),
    widths(WidthsB, ClassesB),
    append(ClassesA, ClassesB, Classes)
  },
  html(
    div(class='form-group',
      div(class=Classes,
        \button(Attrs, Html_0)
      )
    )
  ).



%! form_input0(+Type, +Name, +Attrs)// is det.
%! form_input0(+Type, +Name, +Attrs, +Opts)// is det.
%
% The following options are supported:
%
%   * label(+string) Default is Name capitalized and with suffixed
%     colon.  If `none` then no label is generated at all.
%
%   * widths_a(+or([between(1,12),list(between(1,12))]))
%
%   * widths_b(+or([between(1,12),list(between(1,12))]))

form_input0(Type, Name, Attrs) -->
  form_input0(Type, Name, Attrs, []).


form_input0(file, Name, Attrs1, Opts) --> !,
  {merge_attrs(Attrs1, [id=Name,name=Name,type=file], Attrs2)},
  form_input_inner0(file, Name, Attrs2, Opts).
form_input0(Type, Name, Attrs1, Opts) -->
  {
    merge_attrs(
      Attrs1,
      [class='form-control',id=Name,name=Name,type=Type],
      Attrs2
    )
  },
  form_input_inner0(Type, Name, Attrs2, Opts).


form_input_inner0(Type, Name, Attrs, Opts1) -->
  {
    option(widths_a(WidthsA), Opts1, 4),
    option(widths_b(WidthsB), Opts1, 6),
    form_input_label0(Type, Name, Opts1, Opts2)
  },
  html(
    div(class='form-group', [
      \form_input_label0(WidthsA, WidthsB, Name, Opts2, ClassesB),
      div(class=ClassesB, input(Attrs, []))
    ])
  ).



%! form_input_label0(+Type, +Name, +Opts1, -Opts2) is det.

form_input_label0(hidden, _, Opts, Opts) :- !.
form_input_label0(_, _, Opts, Opts) :-
  option(label(_), Opts), !.
form_input_label0(_, Name, Opts1, Opts2) :-
  capitalize_atom(Name, Lbl0),
  format(string(Lbl), "~a:", [Lbl0]),
  merge_options(Opts1, [label(Lbl)], Opts2).



%! form_input_label0(+WidthsA, +WidthsB, +Name, +Opts, -Classes)// is det.

form_input_label0(WidthsA, WidthsB, Name, Opts, ClassesB) -->
  {
    option(label(Lbl), Opts),
    Lbl \== none, !,
    maplist(widths, [WidthsA,WidthsB], [ClassesA,ClassesB])
  },
  html(label([class=ClassesA,for=Name], Lbl)).
form_input_label0(WidthsA, WidthsB, _, _, Classes) -->
  {
    widths(true, WidthsA, ClassesA),
    widths(WidthsB, ClassesB),
    append(ClassesA, ClassesB, Classes)
  },
  [].



%! functor_and_arity(+Functor, +Arity)// is det.

functor_and_arity(Functor, Arity) -->
  html([Functor,"/",Arity]).



%! http_error(?Status, ?Context, ?LTag, ?Msg) is nondet.

http_error(400, title, nl, "Verkeerde vraag").
http_error(400, title, en, "Bad request").
http_error(400, main, nl, "De server kan de gegeven vraag niet begrijpen.").
http_error(400, main, en, "The server could not process the given request.").
http_error(404, title, nl, "Kan niet vinden").
http_error(404, title, en, "Could not find").
http_error(404, main, nl, "De opgevraagde pagina kan niet gevonden worden.").
http_error(404, main, en, "The requested page could not be found.").
http_error(500, title, nl, "Fout in server").
http_error(500, title, en, "Server error").
http_error(500, main, nl, "De server heeft een fout gemaakt.").
http_error(500, main, en, "The server encountered an internal error.").



%! module_prefix(+Mod)// is det.

module_prefix(Mod) -->
  html([Mod,":"]).



%! nested_predicate_sequence(+Ps)// is det.

nested_predicate_sequence([]) --> !, [].
nested_predicate_sequence([X]) --> !,
  predicate(X).
nested_predicate_sequence([H|T]) -->
  html([
    \predicate(H),
    " --> ",
    \nested_predicate_sequence(T)
  ]).



%! predicate(:Goal_N)// is det.

predicate(Mod:Functor/Arity) --> !,
  html([
    \nonvar(Mod),
    ":",
    \functor_and_arity(Functor, Arity)
  ]).
predicate(Functor/Arity) -->
  functor_and_arity(Functor, Arity).



%! raw_page(+Spec, -Title, -Content) is det.

raw_page(Spec, Title, Content) :-
  absolute_file_name(Spec, Page, [access(read)]),
  load_html_file(Page, Dom),
  contains_term(element(title, _, Title), Dom),
  contains_term(element(body, _, Body), Dom),
  Style = element(style, _, _),
  findall(Style, sub_term(Style, Dom), Styles),
  append(Styles, Body, Content).
