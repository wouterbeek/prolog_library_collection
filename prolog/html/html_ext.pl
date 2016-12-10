:- module(
  html_ext,
  [
    alert//2,                % +Mode:oneof([danger,info,success,warning])
                             % :Content_0
    anchor//2,               % +Name, :Header_0
    between//2,              % :Outer_0, :Middle_0
    between//3,              % :Begin_0, :Middle_0, :End_0
    button//1,               % :Content_0
    button//2,               % +Attrs, :Content_0
    card//2,                 % :Left_0, :Right_0
    card//3,                 % +Attrs, :Left_0, :Right_0
    html_caret//0,
    center_logo//1,          % +Spec
    code_link//1,            % +Iri
    collapse_content//2,     % +Id, :Content_0
    collapse_link//2,        % +Id, :Link_0
    copy_to_clipboard//1,    % +Txt
    data_link//1,            % +Iri
    data_link//2,            % +Iri, :Content_0
    data_link//3,            % +Iri, +Attrs, :Content_0
    date//1,                 % +DT
    deck//2,                 % :Card_1, +Items
    deck//3,                 % +Attrs, :Card_1, +Items
    default//3,              % :Writer_1, :DefContent_1, +Arg1
    definition_list//1,      % +L
    definition_list//2,      % :Writer_1, +L
    definition_list//3,      % +Attrs, :Writer_1, +L
    developed_with//0,
    dict//1,                 % +Dict
    dropdown_menu//3,        % :Top_0, :Writer_1, +Items
    dropdown_menu//4,        % +Attrs, :Top_0, :Writer_1, +Items
    ellipsis//2,             % +Str, +MaxLen
    endpoint_link//1,        % +HandleId
    endpoint_link//2,        % +HandleId, :Content_0
    error//1,                % +E
    external_link//1,        % +Iri
    external_link//2,        % +Iri, :Content_0
    external_link//3,        % +Iri, +Attrs, :Content_0
    external_link_icon//1,   % +Iri
    favicon//1,              % +Spec
    fb_app_id//0,
    fb_comments//1,          % +Iri
    fb_follow_img//0,
    fb_follow_img//1,        % +User
    fb_follow_txt//0,
    fb_follow_txt//1,        % +User
    fb_like//1,              % +Iri
    fb_like//2,              % +Size:oneof([large,small]), +Iri
    fb_share//2,             % +Iri, +Title
    figure//2,               % +Iri, +Caption
    figure//3,               % +Iri, +Alt, :Caption_0
    file_name//1,            % +File
    flag_icon//1,            % +LTag
    footer_panel//3,         % +Spec, :Top, :Bottom
    footnote_post//2,        % +State, :Content_0
    footnote_receive//2,     % +N, :Content_0
    form//2,                 % +Spec, :Content_0
    form//3,                 % +Spec, +Attrs, :Content_0
    form_submit_button//0,
    form_submit_button//1,   % :Content_0
    git_version//0,
    google_analytics//0,
    google_font//1,          % +Name
    grid//4,                 % +GridWidth, +TileWidth, :Writer_1, +Args
    hamburger//1,            % +Target
    html_bracketed//1,       % :Content_0
    html_call//1,            % :Content_0
    html_call//2,            % :Html_1, +Arg1
    html_call//3,            % :Html_2, +Arg1, +Arg2
    html_call//4,            % :Html_3, +Arg1, +Arg2, +Arg3
    html_catch//1,           % :Content_0
    html_code//1,            % :Content_0
    html_dq//1,              % :Content_0
    html_float//1,           % +Float
    html_hook//1,            % +Term
    html_hook//2,            % +Opts, +Term
    html_http_error_page/2,  % +Style, +Req
    html_license//2,         % +Uri, +Lbl
    html_list//3,            % +Ordered:boolean, :Writer_1, +Args
    html_list//4,            % +Attrs, +Ordered:boolean, :Writer_1, +Args
    html_lstring//1,         % +Name
    html_maplist//2,         % :Writer_1, +Args1
    html_maplist//3,         % :Writer_1, +Args1, +Args2
    html_pair//1,            % +Pair
    html_pair//2,            % +Arg1, Arg2
    html_pair//3,            % :Writer_1, +Arg1, +Arg2
    html_quad//4,            % +Arg1, +Arg2, +Arg3, +Arg4
    html_quad//5,            % :Writer_1, +Arg1, +Arg2, +Arg3, +Arg4
    html_select//2,          % :ItemGen_1, :Writer_1
    html_select//3,          % +Attrs, :ItemGen_1, :Writer_1
    html_seplist//2,         % :Content_0, :Sep_0
    html_seplist//3,         % :Writer_1, :Sep_0, +Args
    html_set//1,             % +Args
    html_set//2,             % :Writer_1, +Args
    html_space//0,
    html_sq//1,              % :Content_0
    html_thousands//1,       % +Integer
    html_triple//3,          % +Arg1, +Arg2, +Arg3
    html_triple//4,          % :Writer_1, +Arg1, +Arg2, +Arg3
    html_tuple//1,           % +Arg
    html_tuple//2,           % :Writer_1, +Args
    html_to_atom/2,          % :Content_0, -A
    human_integer//1,        % +N
    icon//1,                 % +Name
    icon_button//1,          % +Name
    icon_button//2,          % +Name, +Func
    idle//1,                 % +Time
    if_then//2,              % :If_0, :Then_0
    if_then_else//3,         % :If_0, :Then_0, :Else_0
    ignore//1,               % :Content_0
    image//1,                % +Spec
    image//2,                % +Spec, +Attrs
    image_header//2,         % +Img, :Content_0
    input_boolean//1,        % +Name
    input_checkbox//2,       % +Name, +Attrs
    input_file//2,           % +Name, +Attrs
    input_file//3,           % +Name, +Attrs, +Opts
    input_hidden//2,         % +Name, +Attrs
    input_password//2,       % +Name, +Attrs
    input_password//3,       % +Name, +Attrs, +Opts
    input_radio//2,          % +Name, +L
    input_text//2,           % +Name, +Attrs
    input_text//3,           % +Name, +Attrs, +Opts
    insert_raw_body//1,      % +Spec
    internal_link//1,        % +Spec
    internal_link//2,        % +Spec, :Content_0
    internal_link//3,        % +Spec, +Attrs, :Content_0
    ip//1,                   % +Ip
    language_menu//1,        % +LTags
    link//1,                 % +Pair
    link//2,                 % +Attrs, +Pair
    link_button//2,          % +Iri, :Content_0
    linkedin_share//0,
    list//1,                 % +Args
    list//2,                 % :Writer_1, +Args
    mail_icon//1,            % +Uri
    mail_link_and_icon//1,   % +Uri
    menu//0,
    merge_attrs/3,           % +Attrs1, +Attrs2, -Attrs3
    meta//2,                 % +Name, +Content
    meta_authors//0,
    meta_authors//1,         % +Authors:list(string)
    meta_charset//0,
    meta_description//1,     % +Desc
    meta_ie_latest//0,
    meta_license//1,         % +Uri
    meta_viewport//0,
    navbar//3,               % :Brand_0, :Menu_0, :Right_0
    navbar_brand_img//2,     % +Local, +Alt
    navbar_dropdown_menu//4, % +Name, +Lbl, :Writer_1, +L
    navbar_dropdown_menu//5, % +Attrs, +Name, +Lbl, :Writer_1, +L
    nonvar//1,               % :Content_0
    nonvar//2,               % :Writer_1, +Arg1
    nowrap//1,               % :Content_0
    number//2,               % +Format, +N
    once//1,                 % :Content_0
    ordered_list//1,         % +Items
    ordered_list//2,         % :Writer_1, +Items
    ordered_list//3,         % +Attrs, :Writer_1, +Items
    pagination_links//1,     % +Result
    pagination_result//2,    % +Result, :Writer_1
    pagination_result_nonempty//2, % +Result, :Writer_1
    panel//3,                % +In, :Header_0, :Body_0
    panel//4,                % +Open:boolean, +In, :Header_0, :Body_0
    panels//1,               % :Panels_0
    pipe//0,
    pl_link//0,
    pl_version//0,
    ref//2,                  % +Label, :Content_0
    reply_html_page/4,       % +Method, +Style, :Head, :Body
    reply_raw_file/1,        % +Spec
    reset_button//0,
    reset_button//1,         % :Content_0
    row_1//1,                % :ContentA_0
    row_1//2,                % +WidthsA, :ContentA_0
    row_1//3,                % +Attrs, +WidthsA, :ContentA_0
    row_2//2,                % :ContentA_0, :ContentB_0
    row_2//3,                % +Attrs, :ContentA_0, :ContentB_0
    row_2//4,                % +WidthsA, :ContentA_0, +WidthsB, :ContentB_0
    row_2//5,                % +Attrs, +WidthsA, :ContentA_0, +WidthsB, :ContentB_0
    row_3//3,                % :ContentA_0, :ContentB_0, :ContentC_0
    row_3//6,                % +WidthsA
                             % :ContentA_0
                             % +WidthsB
                             % :ContentB_0
                             % +WidthsC
                             % :ContentC_0
    row_4//4,                % :ContentA_0, :ContentB_0, :ContentC_0, :ContentD_0
    row_4//8,                % +WidthsA
                             % :ContentA_0
                             % +WidthsB
                             % :ContentB_0
                             % +WidthsC
                             % :ContentC_0
                             % +WidthsD
                             % :ContentD_0
    search_box//1,           % +Action
    search_box//2,           % +Attrs, +Action
    search_result//2,        % +Result, :Writer_1
    streamer//1,             % :Content_0
    submit_button//0,
    submit_button//1,        % :Content_0
    table//1,                % :BodyContent_0
    table//2,                % :HeaderContent_0, :BodyContent_0
    table//3,                % :CaptionContent_0, :HeaderContent_0, :BodyContent_0
    table_caption//1,        % :Content_0
    table_content//2,        % :Cell_1, +Rows
    table_data_row//1,       % +Row
    table_data_row//2,       % :CellWriter_1, +Row
    table_header_row//1,     % +Row
    table_header_row//2,     % :CellWriter_1, +Row
    table_tree//1,           % +Tree
    table_tree//2,           % :CellWriter_1, +Tree
    table_trees//1,          % +Trees
    table_trees//2,          % :CellWriter_1, +Trees
    term//1,                 % @Term
    title//1,                % +Strs
    tooltip//2,              % +Str, :Content_0
    truncate//2,             % +Str, +Max
    twitter_follow_img//0,
    twitter_follow_img//1,   % +User
    twitter_follow_txt//0,
    twitter_follow_txt//1,   % +User
    twitter_grid//2,         % +Iri, +Title
    twitter_mention//0,
    twitter_mention//1,      % +User
    twitter_profile//0,
    twitter_profile//1,      % +User
    twitter_share//2,        % +Iri, +Title
    twitter_tweet//1,        % +Iri
    unless//2,               % :Unless_0, :Then_0
    unordered_list//1,       % +Items
    unordered_list//2,       % :Writer_1, +Items
    unordered_list//3,       % +Attrs, :Writer_1, +Items
    upload_form//1,          % +Spec
    user_menu//2,            % :UserName_2, :UserImg_2
    vote_down//1,            % +Vote:integer
    vote_up//1,              % +Vote:integer
    widths/2                 % +Widths:or([list(between(1,12)),between(1,12)]), -Classes:list(atom)
  ]
).

/** <module> HTML extensions

Raw HTML can be included using quasi-quoting:

```
html({|html||...|}).
```

@author Wouter Beek
@version 2016/02-2016/12
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(html/html_resource)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_user)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/jquery)).
:- use_module(library(http/js_write)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(json_ext)).
:- use_module(library(licenses)).
:- use_module(library(list_ext)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(pagination)).
:- use_module(library(pair_ext)).
:- use_module(library(pl_ext)).
:- use_module(library(q/q_term)). %HACK
:- use_module(library(settings)).
:- use_module(library(string_ext)).
:- use_module(library(typecheck)).
:- use_module(library(uri)).

:- html_meta
   alert(+, html, ?, ?),
   anchor(+, html, ?, ?),
   between(html, html, ?, ?),
   between(html, html, html, ?, ?),
   button(html, ?, ?),
   button(+, html, ?, ?),
   card(html, html, ?, ?),
   card(+, html, html, ?, ?),
   collapse_content(+, html, ?, ?),
   collapse_link(+, html, ?, ?),
   data_link(+, html, ?, ?),
   data_link(+, +, html, ?, ?),
   definition_list(html, +, ?, ?),
   definition_list(+, html, +, ?, ?),
   dropdown_menu(html, :, +, ?, ?),
   dropdown_menu(+, html, :, +, ?, ?),
   endpoint_link(+, html, ?, ?),
   external_link(+, html, ?, ?),
   external_link(+, +, html, ?, ?),
   fb_follow0(html, ?, ?),
   fb_follow0(+, html, ?, ?),
   figure(+, +, html, ?, ?),
   footer_panel(+, html, html, ?, ?),
   footnote_post(+, html, ?, ?),
   footnote_receive(+, html, ?, ?),
   form(+, html, ?, ?),
   form(+, +, html, ?, ?),
   form_submit_button(html, ?, ?),
   grid(+, +, 3, +, ?, ?),
   html_bracketed(html, ?, ?),
   html_catch(html, ?, ?),
   html_code(html, ?, ?),
   html_dq(html, ?, ?),
   html_list(+, 3, +, ?, ?),
   html_list(+, +, 3, +, ?, ?),
   html_seplist(html, html, ?, ?),
   html_seplist(3, html, +, ?, ?),
   html_sq(html, ?, ?),
   html_to_atom(html, -),
   if_then(0, html, ?, ?),
   if_then_else(0, html, html, ?, ?),
   ignore(html, ?, ?),
   image_header(+, html, ?, ?),
   internal_link(+, html, ?, ?),
   internal_link(+, +, html, ?, ?),
   link_button(+, html, ?, ?),
   navbar(html, html, html, ?, ?),
   navbar_dropdown_menu(+, +, 3, +, ?, ?),
   navbar_dropdown_menu(+, +, +, 3, +, ?, ?),
   nonvar(html, ?, ?),
   nowrap(html, ?, ?),
   ordered_list(html, +, ?, ?),
   ordered_list(+, html, +, ?, ?),
   panel(+, html, html, ?, ?),
   panel(+, +, html, html, ?, ?),
   panels(html, ?, ?),
   ref(+, html, ?, ?),
   reply_html_page(+, +, :, :),
   reset_button(html, ?, ?),
   row_1(html, ?, ?),
   row_1(+, html, ?, ?),
   row_1(+, +, html, ?, ?),
   row_2(html, html, ?, ?),
   row_2(+, html, html, ?, ?),
   row_2(+, html, +, html, ?, ?),
   row_2(+, +, html, +, html, ?, ?),
   row_3(html, html, html, ?, ?),
   row_3(+, html, +, html, +, html, ?, ?),
   row_4(html, html, html, html, ?, ?),
   row_4(+, html, +, html, +, html, +, html, ?, ?),
   streamer(html, ?, ?),
   submit_button(html, ?, ?),
   table(html, ?, ?),
   table(html, html, ?, ?),
   table(html, html, html, ?, ?),
   table_caption(html, ?, ?),
   table_header(html, ?, ?),
   tile(3, +, ?, ?),
   tooltip(+, html, ?, ?),
   twitter_follow0(html, ?, ?),
   twitter_follow0(+, html, ?, ?),
   unless(0, html, ?, ?),
   unordered_list(html, +, ?, ?),
   unordered_list(+, html, +, ?, ?).

:- set_setting(jquery:version, '3.1.1.min').

:- if(debugging(css(bootstrap))).
  :- html_resource(
       css(bootstrap),
       [requires([css('bootstrap-3.3.7.css')]),virtual(true)]
     ).
:- else.
  :- html_resource(
       css(bootstrap),
       [requires([css('bootstrap-3.3.7.min.css')]),virtual(true)]
     ).
:- endif.

:- if(debugging(css('bootstrap-theme'))).
  :- html_resource(
       css('bootstrap-theme'),
       [
         ordered(true),
         requires([css(bootstrap),css('bootstrap-theme-3.3.7.css')]),
         virtual(true)
       ]
     ).
:- else.
  :- html_resource(
       css('bootstrap-theme'),
       [
         ordered(true),
         requires([css(bootstrap),css('bootstrap-theme-3.3.7.min.css')]),
         virtual(true)
       ]
     ).
:- endif.

:- if(debugging(js(bootstrap))).
  :- html_resource(
       js(bootstrap),
       [
         ordered(true),
         requires([jquery,js('bootstrap-3.3.7.js')]),
         virtual(true)
       ]
     ).
:- else.
  :- html_resource(
       js(bootstrap),
       [
         ordered(true),
         requires([jquery,js('bootstrap-3.3.7.min.js')]),
         virtual(true)
       ]
     ).
:- endif.

:- html_resource(
     bootstrap,
     [requires([css('bootstrap-theme'),js(bootstrap)]),virtual(true)]
   ).

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

:- html_resource(
     css(grid),
     [requires([css('grid.css')]),virtual(true)]
   ).

:- if(debugging(js(grid))).
  :- html_resource(
       js(grid),
       [ordered(true),requires([jquery,js('jquery.pinto.js')]),virtual(true)]
     ).
:- else.
  :- html_resource(
       js(grid),
       [ordered(true),requires([jquery,js('jquery.pinto.min.js')]),virtual(true)]
     ).
:- endif.

:- html_resource(grid, [requires([css(grid),js(grid)]),virtual(true)]).

:- html_resource(
     html_ext,
     [
       ordered(true),
       requires([bootstrap,css('html_ext.css')]),
       virtual(true)
     ]
   ).

:- meta_predicate
    deck(3, +, ?, ?),
    deck(+, 3, +, ?, ?),
    default(3, 3, +, ?, ?),
    dropdown_menu(2, 3, +, ?, ?),
    dropdown_menu(+, 2, 3, +, ?, ?),
    html_call(2, ?, ?),
    html_call(3, +, ?, ?),
    html_call(4, +, +, ?, ?),
    html_call(5, +, +, +, ?, ?),
    html_maplist(3, +, ?, ?),
    html_maplist(4, +, +, ?, ?),
    html_pair(3, +, +, ?, ?),
    html_quad(3, +, +, +, +, ?, ?),
    html_select(1, 3, ?, ?),
    html_select(+, 1, 3, ?, ?),
    html_set(3, +, ?, ?),
    html_triple(3, +, +, +, ?, ?),
    html_tuple(3, +, ?, ?),
    if_then(0, 2, ?, ?),
    if_then_else(0, 2, 2, ?, ?),
    list(3, +, ?, ?),
    nonvar(3, +, ?, ?),
    once(3, ?, ?),
    pagination_result(+, 3, ?, ?),
    pagination_result_nonempty(+, 3, ?, ?),
    search_result(+, 3, ?, ?),
    table_content(3, +, ?, ?),
    table_data_cell(3, +, ?, ?),
    table_data_row(3, +, ?, ?),
    table_header_cell(3, +, ?, ?),
    table_header_row(3, +, ?, ?),
    table_tree(3, +, ?, ?),
    table_tree(+, 3, +, ?, ?),
    table_tree_cell(+, 3, +, +, ?, ?),
    table_trees(3, +, ?, ?),
    table_trees(+, 3, +, ?, ?),
    unless(0, 2, ?, ?),
    user_menu(2, 2, ?, ?).

%! html:author(?Name) is nondet.

%! html:menu_item(?Major, ?Name, ?Lbl) is nondet.
%
% Adds a top-level menu item to the menu.  The menu item has a rank
% Major, an internal Name and a user-visible label Lbl.

%! html:menu_item(?Name, ?Minor, ?Spec, ?Lbl) is nondet.
%
% Adds a menu-item under a top-level menu item with the given internal
% Name.  Minor denotes the rank within the top-level menu item.  Spec
% denotes the HTTP handler that fires when this menu item is clicked.
% Lbl is a user-visible label.

%! html:html_hook(@Term)// is det.

%! html:html_hook(+Opts, @Term)// is det.

:- multifile
    html:author/1,
    html:menu_item/3,
    html:menu_item/4,
    html:html_hook//1,
    html:html_hook//2.

:- setting(
     html:fb_app_id,
     atom,
     '',
     "Facebook application identifier."
   ).
:- setting(
     html:fb_profile,
     atom,
     '',
     "Facebook profile name."
   ).
:- setting(
     html:google_analytics_id,
     atom,
     '',
     "Google Analytics ID."
   ).
:- setting(
     html:twitter_profile,
     atom,
     '',
     "Twitter profile name."
   ).

% string
html:html_hook(string(Str)) -->
  html(Str).
% code
html:html_hook(code(Str)) -->
  html_code(Str).
% empty
html:html_hook(empty) -->
  html([]).
% IRI
html:html_hook(iri(Iri)) -->
  external_link(Iri).
% set
html:html_hook(set(L)) -->
  html_set(L).
% thousands
html:html_hook(thousands(N)) -->
  html_thousands(N).
% atom
html:html_hook(A) -->
  {atom(A)},
  html(A).
% string
html:html_hook(Str) -->
  {string(Str)},
  html(Str).






%! alert(+Mode:oneof([danger,info,success,warning]), :Content_0)// is det.

alert(Mode, Content_0) -->
  {atomic_list_concat([alert,Mode], -, C)},
  html(div([class=[alert,C],role=alert], Content_0)).



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



%! button(:Content_0)// is det.
%! button(+Attrs, :Content_0)// is det.


button(Content_0) -->
  button([], Content_0).


button(Attrs1, Content_0) -->
  {merge_attrs([class=[btn,'btn-default'],type=button], Attrs1, Attrs2)},
  html(button(Attrs2, Content_0)).



%! card(:Left_0, :Right_0)// is det.
%! card(+Attrs, :Left_0, :Right_0)// is det.

card(Left_0, Right_0) -->
  card([], Left_0, Right_0).


card(Attrs1, Left_0, Right_0) -->
  {merge_attrs([class=[card,row]], Attrs1, Attrs2)},
  html(
    div(Attrs2, [
      div(class=['col-xs-0','col-sm-0','col-md-0','col-lg-1'], []),
      div(class=['col-xs-12','col-sm-4','col-md-4','col-lg-3',left],
        Left_0
      ),
      div(class=['col-xs-12','col-sm-8','col-md-8','col-lg-7',right],
        Right_0
      ),
      div(class=['col-xs-0','col-sm-0','col-md-0','col-lg-1'], [])
    ])
  ).



%! center_logo(+Spec)// is det.
%
% Generates a full-width SVG logo, typically directly beneath the
% navigation bar.

center_logo(Spec) -->
  html(
    div(class=row,
      div(class=[col,'col-md-12'],
        a([href='/',id=logo],
          \image(Spec, [class='center-block'])
        )
      )
    )
  ).



%! code_link(+Str)// is det.

code_link(Str0) -->
  {string_or_strings(Str0, Str)},
  html([
    \html_requires(js(clipboard)),
    \js_script({|javascript(_)||
new Clipboard('.btn');
    |}),
    \html_code(Str),
    " ",
    \copy_to_clipboard(Str)
  ]).



%! collapse_content(+Id, :Content_0)// is det.

collapse_content(Id, Content_0) -->
  html(
    div([class=collapse,id=Id],
      div(class=[card,'card-block'], Content_0)
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



%! data_link(+Iri)// is det.
%! data_link(+Iri, :Content_0)// is det.
%! data_link(+Iri, +Attrs, :Content_0)// is det.

data_link(Iri) -->
  data_link(Iri, \icon(internal_link)).


data_link(Iri, Content_0) -->
  data_link(Iri, [], Content_0).


data_link(Iri, Attrs1, Content_0) -->
  {
    iri_to_location(Iri, Loc),
    merge_attrs([href=Loc], Attrs1, Attrs2)
  },
  html(a(Attrs2, Content_0)).



%! date(+DT)// is det.

date(DT) -->
  {format_time(string(Str), "%+", DT)},
  html(Str).



%! deck(:Card_1, +Items)// is det.
%! deck(+Attrs, :Card_1, +Items)// is det.

deck(Card_1, L) -->
  deck([], Card_1, L).


deck(Attrs1, Card_1, L) -->
  {merge_attrs([class=[container,deck]], Attrs1, Attrs2)},
  html(div(Attrs2, \html_maplist(Card_1, L))).



%! default(:Writer_1, :DefContent_1, +Arg1)// is det.

default(Writer_1, DefContent_1, Arg1) -->
  (   {var_goal(Writer_1)}
  ->  html_call(DefContent_1, Arg1)
  ;   html_call(Writer_1, Arg1)
  ).



%! definition_list(+L)// is det.
%! definition_list(:Writer_1, +L)// is det.
%! definition_list(+Attrs, :Writer_1, +L)// is det.
%
% Generates an HTML definition list.  Definition terms are of the form
% def(Definiens,Definiendum).

definition_list(L) -->
  definition_list(html, L).


definition_list(Writer_1, L) -->
  definition_list([], Writer_1, L).


definition_list(Attrs, Writer_1, L) -->
  html(
    dl(Attrs,
      \html_maplist(definition_list_item(Writer_1), L)
    )
  ).


definition_list_item(Writer_1, L) -->
  {is_list(L)}, !,
  definition_list(Writer_1, L).
definition_list_item(Writer_1, def(Definiens,Definiendum)) -->
  html([
    dt(\html_call(Writer_1, Definiens)),
    dd(\html_call(Writer_1, Definiendum))
  ]).



developed_with -->
  html(
    div(class='developed-with', [
      \html_lstring(developed_with),
      " ",
      \pl_link,
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



%! dropdown_menu(:Top_0, :Writer_1, +Items)// is det.
%! dropdown_menu(+Attrs, :Top_0, :Writer_1, +Items)// is det.

dropdown_menu(Top_0, Writer_1, L) -->
  dropdown_menu([], Top_0, Writer_1, L).


dropdown_menu(Attrs1, Top_0, Writer_1, L) -->
  {merge_attrs(Attrs1, [class=dropdown], Attrs2)},
  html(
    li(Attrs2, [
      a([
        'aria-expanded'=false,
        'aria-haspopup'=true,
        class='dropdown-toggle',
        'data-toggle'=dropdown,
        role=button
      ], [
        Top_0,
        \html_caret
      ]),
      \unordered_list([class='dropdown-menu'], Writer_1, L)
    ])
  ).



%! ellipsis(+Str, +MaxLen)// is det.

ellipsis(Str, MaxLen) -->
  {string_ellipsis(Str, MaxLen, Ellipsis)},
  ({Str == Ellipsis} -> html(Str) ; tooltip(Str, Ellipsis)).



%! endpoint_link(+HandleId)// is det.
%! endpoint_link(+HandleId, :Content_0)// is det.

endpoint_link(HandleId) -->
  {http_link_to_id(HandleId, Iri)},
  internal_link(Iri, code(Iri)).


endpoint_link(HandleId, Content_0) -->
  {http_link_to_id(HandleId, Iri)},
  internal_link(Iri, Content_0).



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



%! external_link(+Iri)// is det.
%! external_link(+Iri, :Content_0)// is det.
%! external_link(+Iri, +Attrs, :Content_0)// is det.
%
% Generates an HTML link to an external resource.
%
% When Icon is `true` the fact that the link points to an external
% resource is indicated by a link icon (default is `false`).

external_link(Iri) -->
  external_link(Iri, Iri).


external_link(Iri, Content_0) -->
  external_link(Iri, [], Content_0).


external_link(Iri, Attrs1, Content_0) -->
  {merge_attrs(Attrs1, [href=Iri,target='_blank'], Attrs2)},
  html(a(Attrs2, Content_0)).



%! external_link_icon(+Iri)// is det.

external_link_icon(Iri) -->
  html(a([href=Iri,target='_blank'], \icon(external_link))).



%! favicon(+Spec)// is det.
%
% Generates an HTML link to a favicon.  This icon will show up in a
% Web browser's tab.

favicon(Spec) -->
  {spec_iri(Spec, Iri)},
  link([type='image/x-icon'], icon-Iri).



%! fb_app_id// is det.

fb_app_id -->
  {setting(html:fb_app_id, Id)},
  meta('fb:app_id', Id).



%! fb_comments(+Iri)// is det.

fb_comments(Iri) -->
  html(
    div([
      class='fb-comments',
      'data-colorscheme'=dark,
      'data-href'=Iri,
      'data-numposts'=5
    ], [])
  ).



%! fb_follow0(:Content_0)// is det.
%! fb_follow0(+User, :Content_0)// is det.

fb_follow0(Content_0) -->
  {setting(html:fb_profile, User)},
  fb_follow0(User, Content_0).


fb_follow0(User, Content_0) -->
  {fb_user_iri(User, Iri)},
  html(a(href=Iri, Content_0)).



%! fb_follow_img// is det.
%! fb_follow_img(+User)// is det.

fb_follow_img -->
  {setting(html:fb_profile, User)},
  fb_follow_img(User).


fb_follow_img(User) -->
  {lstring(like_us_on_x, ["Facebook"], Str)},
  tooltip(Str, \fb_follow0(User, \fb_img0)).



%! fb_follow_txt// is det.
%! fb_follow_txt(+User)// is det.

fb_follow_txt -->
  {setting(html:fb_profile, User)},
  fb_follow_txt(User).


fb_follow_txt(User) -->
  {lstring(like_us_on_x, ["Facebook"], Str)},
  tooltip(Str, \fb_follow0(User, fb_txt0(User))).

fb_txt0(User) -->
  html([
    \html_lstring(follow),
    " ",
    User,
    " ",
    \lstring(on),
    "Facebook"
  ]).



fb_img0 -->
  {http_absolute_location(img('facebook.png'), Loc)},
  html(img([alt="Facebook",src=Loc], [])).



%! fb_like(+Iri)// is det.
%! fb_like(+Style:oneof([large,small]), +Iri)// is det.

fb_like(Iri) -->
  fb_like(small, Iri).


fb_like(Size, Iri) -->
  html(
    div([
      class='fb-like',
      'data-action'=like,
      'data-colorsheme'=dark,
      'data-href'=Iri,
      'data-layout'=button_count,
      'data-share'=true,
      'data-show-faces'=false,
      'data-size'=Size
    ], [])
  ).



%! fb_share(+Iri, +Title)// is det.

fb_share(Iri, Title) -->
  {
    lstring(share_x_on_y, [Title,"Facebook"], Str),
    uri_query_components(Query, [title=Title,u=Iri]),
    uri_components(
      Url,
      uri_components(http,'www.facebook.com','/share.php',Query,_)
    )
  },
  tooltip(Str, a([href=Url,target='_blank'], \fb_img0)).



%! figure(+Iri, +Caption)// is det.
%! figure(+Iri, +Alt, :Caption_0)// is det.

figure(Iri, Caption) -->
  figure(Iri, Caption, html(Caption)).


figure(Iri, Alt, Caption_0) -->
  html(figure([\image(Iri, [alt=Alt]),figcaption(Caption_0)])).



%! file_name(+File)// is det.
%
% Generates an HTML description of the given file name.

file_name(File) -->
  html(span(class=file, File)).



%! flag_icon(+LTag)// is det.

flag_icon(LTag) -->
  {
    file_name_extension(LTag, svg, File),
    directory_file_path(flag_4x3, File, Path)
  },
  html(span(class=[label,'label-primary'], [\flag_icon_img(Path)," ",LTag])).


flag_icon_img(Path) -->
  {
    absolute_file_name(img(Path), _, [access(read)]), !,
    http_absolute_location(img(Path), Location)
  },
  html(span(class='flag-icon', img(src=Location))).
flag_icon_img(_) --> [].



%! footer_panel(+Spec, :Top_0, :Bottom_0)// is det.

footer_panel(Spec, Top_0, Bottom_0) -->
  html([
    div(style='display: table;', [
      div([class='footer-logo',style='display: table-cell;'],
        a(
          href='/',
          \image(Spec, [height=60,style='max-height: 60px;'])
        )
      ),
      div([class='brand-txt',style='display: table-cell; font-size: 120%; vertical-align: middle;'],
        a(href='/', Top_0)
      )
    ]),
    Bottom_0
  ]).




%! footnote_post(+State, :Content_0)// is det.

footnote_post(State, Content_0) -->
  {
    dict_inc(footnote, State, N),
    format(atom(Ref), "ref~d", [N]),
    format(atom(Note), "#node~d", [N])
  },
  html([
    sup(id=Ref, a(href=Note, ["[",N,"]"])),
    \html_post(footnote, \footnote_receive(N, Content_0))
  ]).



%! footnote_receive(+N, :Content_0)// is det.

footnote_receive(N, Content_0) -->
  {
    format(atom(Ref), "#ref~d", [N]),
    format(atom(Note), "node~d", [N])
  },
  html(li(id=Note, [a(href=Ref, "^"), " ", Content_0])).



%! form(+Spec, :Content_0)// is det.
%! form(+Spec, +Attrs, :Content_0)// is det.

form(Spec, Content_0) -->
  form(Spec, [], Content_0).


form(Spec, Attrs1, Content_0) -->
  {
    spec_iri(Spec, Iri),
    merge_attrs([method=get], Attrs1, Attrs2),
    merge_attrs(Attrs2, [action=Iri], Attrs3)
  },
  html(form(Attrs3, Content_0)).



%! form_submit_button// is det.
%! form_submit_button(:Content_0)// is det.

form_submit_button -->
  form_submit_button("Submit").


form_submit_button(Content_0) -->
  form_button0([type=submit], Content_0).



%! git_version// is det.

git_version -->
  {git_version(Version)},
  html(["GIT version ",Version,")"]).



%! google_analytics// is det.

google_analytics -->
  {
    setting(html:google_analytics_id, Id),
    Id \== '', !
  },
  js_script({|javascript(Id)||
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','//www.google-analytics.com/analytics.js','ga');
ga('create', Id, 'auto');
ga('send', 'pageview');
  |}).
google_analytics --> [].



%! google_font(+Name)// is det.

google_font(Name) -->
  {
    Dict = _{
      host: 'fonts.googleapis.com',
      path: [css],
      query: [family(Name)],
      scheme: https
    },
    iri_create(Dict, Iri)
  },
  link([type='text/css'], stylesheet-Iri).



%! grid(+GridWidth, +TileWidth, :Writer_1, +Args)// is det.
%
% Generates an HTML grid whose tiles implement widgets.  Widgets are
% represented by Prolog compound terms.  Widgets are defined outside
% of this module.

grid(GridWidth, TileWidth, Writer_1, L) -->
  {format(atom(Style), 'margin-top: 10px; max-width: ~dpx;', [GridWidth])},
  html([
    div([id=grid,style=Style], \html_maplist(tile(Writer_1), L)),
    \html_requires(grid),
    \js_script({|javascript(TileWidth)||
$('#grid').pinto({gapY: 10, itemWidth: TileWidth});
    |})
  ]).


%! tile(:Writer_1, +Arg)// is det.

tile(Writer_1, X) -->
  html(div(\html_call(Writer_1, X))).



%! hamburger(+Target)// is det.

hamburger(Target0) -->
  {atom_concat('#', Target0, Target)},
  html(
    button([
      'aria-expanded'=false,
      class=[collapsed,'navbar-toggle'],
      'data-target'=Target,
      'data-toggle'=collapse,
      type=button
    ], [
      span(class='sr-only', "Toggle navigation"),
      \bar,
      \bar,
      \bar
    ])
  ).


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



%! html_bracketed(:Content_0)// is det.

html_bracketed(Content_0) -->
  html(["(",Content_0,")"]).



%! html_call(:Content_0)// is det.
%! html_call(:Html_1, +Arg1)// is det.
%! html_call(:Html_4, +Arg1, +Arg2)// is det.
%! html_call(:Html_5, +Arg1, +Arg2, +Arg3)// is det.

html_call(Content_0, X, Y) :-
  call(Content_0, X, Y).


html_call(Html_1, Arg1, X, Y) :-
  call(Html_1, Arg1, X, Y).


html_call(Html_4, Arg1, Arg2, X, Y) :-
  call(Html_4, Arg1, Arg2, X, Y).


html_call(Html_5, Arg1, Arg2, Arg3, X, Y) :-
  call(Html_5, Arg1, Arg2, Arg3, X, Y).



%! html_callable(+Mod, +Html_2)// is det.

html_callable(Mod, Html_2) -->
  {compound_name_arity(Html_2, Functor, Arity)},
  predicate(Mod:Functor/Arity).



%! html_caret// is det.

html_caret -->
  html(span(class=caret, [])).



%! html_catch(:Content_0)// is det.
%
% Either generates Content_0 or an HTML representation of an exception
% thrown by Content_0.

html_catch(Content_0, X, Y) :-
  catch(
    call(Content_0, X, Y),
    E,
    call(error(exception(E)), X, Y)
  ).



%! html_code(:Content_0)// is det.

html_code(Content_0) -->
  html(code(Content_0)).



%! html_dq(:Content_0)// is det.

html_dq(Content_0) -->
  html(["“",Content_0,"”"]).



%! html_float(+Float)// is det.

html_float(Float) -->
  html("~G"-[Float]).



%! html_hook(+Term)// is det.
%! html_hook(+Opts, +Term)// is det.

html_hook(Term) -->
  html_hook(_{}, Term).


html_hook(Opts, Term) -->
  html:html_hook(Opts, Term), !.
html_hook(_, Term) -->
  html:html_hook(Term), !.
html_hook(_, Html_0) -->
  html_call(Html_0).



%! html_http_error_page(+Style, +Req) is det.

html_http_error_page(Style, Req) :-
  memberchk(status(Status), Req),
  http_error(Status, title, Lang, Title),
  http_error(Status, main, Lang, Main),
  reply_html_page(Style,
    \title(["Error",Title]),
    html(article([header(Title),section(Main)]))
  ).



%! html_license(+Iri, Lbl)// is det.

html_license(Uri, Lbl) -->
  external_link(Uri, [rel=license], Lbl).



%! html_list(+Ordered:boolean, :Writer_1, +Args)// is det.
%! html_list(+Attrs, +Ordered:boolean, :Writer_1, +Args)// is det.

html_list(Ordered, Writer_1, L) -->
  html_list([], Ordered, Writer_1, L).

html_list(Attrs, false, Writer_1, L) --> !,
  html(ul(Attrs, \html_maplist(html_list_item(false, Writer_1), L))).
html_list(Attrs, true, Writer_1, L) -->
  html(ol(Attrs, \html_maplist(html_list_item(true, Writer_1), L))).

html_list_item(Ordered, Writer_1, L) -->
  {is_list(L)}, !,
  html(li(\html_list(Ordered, Writer_1, L))).
html_list_item(_, Writer_1, X) --> !,
  html(li(\html_call(Writer_1, X))).



%! html_lstring(+Name)// is det.

html_lstring(Name) -->
  {lstring(Name, Str)},
  html(Str).



%! html_maplist(:Writer_1, +Args1) .
%! html_maplist(:Writer_1, +Args1, +Args2)// is det.

html_maplist(_, []) --> !, [].
html_maplist(Writer_1, [H|T]) -->
  html_call(Writer_1, H),
  html_maplist(Writer_1, T).


html_maplist(_, [], []) --> [].
html_maplist(Goal_4, [H1|T1], [H2|T2]) -->
  html_call(Goal_4, H1, H2),
  html_maplist(Goal_4, T1, T2).



%! html_pair(+Pair)// is det.
%! html_pair(+Arg1, +Arg2)// is det.
%! html_pair(:Writer_1, +Arg1, +Arg2)// is det.
%
% Generates an HTML representation for a pair.

html_pair(Arg1-Arg2) -->
  html_pair(Arg1, Arg2).


html_pair(Arg1, Arg2) -->
  tuple([Arg1,Arg2]).


html_pair(Writer_1, Arg1, Arg2) -->
  tuple(Writer_1, [Arg1,Arg2]).



%! html_quad(+Arg1, +Arg2, +Arg3, +Arg4)// is det.
%! html_quad(:Writer_1, +Arg1, +Arg2, +Arg3, +Arg4)// is det.
%
% Generates an HTML representation for a triple.

html_quad(Arg1, Arg2, Arg3, Arg4) -->
  html_tuple([Arg1,Arg2,Arg3,Arg4]).


html_quad(Writer_1, Arg1, Arg2, Arg3, Arg4) -->
  html_tuple(Writer_1, [Arg1,Arg2,Arg3,Arg4]).



%! html_select(:ItemGen_1, :Writer_1)// is det.
%! html_select(+Attrs, :ItemGen_1, :Writer_1)// is det.

html_select(ItemGen_1, Writer_1) -->
  html_select([], ItemGen_1, Writer_1).


html_select(Attrs1, ItemGen_1, Writer_1) -->
  {
    merge_attrs(Attrs1, [class='form-control'], Attrs2),
    findall(Item, call(ItemGen_1, Item), Items)
  },
  html(select(Attrs2, \html_maplist(Writer_1, Items))).



%! html_seplist(:Content_0, :Sep_0)// is det.
%! html_seplist(:Writer_1, :Sep_0, +L)// is det.

html_seplist(Content_0, Sep_0) -->
  Content_0,
  Sep_0,
  html_seplist(Content_0, Sep_0).
html_seplist(Content_0, _) --> !,
  Content_0.
html_seplist(_, _) --> !, [].


html_seplist(_, _, []) --> !, [].
html_seplist(Writer_1, _, [H]) --> !,
  html_call(Writer_1, H).
html_seplist(Writer_1, Sep_0, [H1,H2|T]) -->
  html_call(Writer_1, H1),
  Sep_0,
  html_seplist(Writer_1, Sep_0, [H2|T]).



%! html_set(+Args)// is det.
%! html_set(:Writer_1, +Args)// is det.

html_set(Args) -->
  html_set(html_hook, Args).


html_set(Writer_1, Args) -->
  html([&(123),\html_seplist(Writer_1, html(","), Args),&(125)]).



%! html_space// is det.

html_space -->
  html(span(class=space, [])).



%! html_sq(:Content_0)// is det.

html_sq(Content_0) -->
  html(["‘",Content_0,"’"]).



%! html_to_atom(:Content_0, -A) is det.

html_to_atom(Content_0, A) :-
  phrase(html(Content_0), Tokens),
  with_output_to(atom(A), print_html(Tokens)).



%! html_thousands(+Integer)// is det.

html_thousands(inf) --> !,
  html("∞").
html_thousands(Integer) -->
  html("~:D"-[Integer]).



%! html_triple(+Arg1, +Arg2, +Arg3)// is det.
%! html_triple(:Writer_1, +Arg1, +Arg2, +Arg3)// is det.
%
% Generates an HTML representation for a triple.

html_triple(Arg1, Arg2, Arg3) -->
  html_tuple([Arg1,Arg2,Arg3]).


html_triple(Writer_1, Arg1, Arg2, Arg3) -->
  html_tuple(Writer_1, [Arg1,Arg2,Arg3]).



%! html_tuple(+Args)// is det.
%! html_tuple(:Writer_1, +Args)// is det.
%
% Generates an HTML representation for a tuple.

html_tuple(Args) -->
  html_tuple(html, Args).


html_tuple(Writer_1, Args) -->
  html([&(lang),\html_seplist(Writer_1, html(","), Args),&(rang)]).



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



%! icon(+Name)// is det.

icon(pen) --> !,
  html(
    svg([width=14,height=14,viewBox=[0,0,300,300]],
      path([fill='#777777',d='M253 123l-77-77 46-46 77 77-46 46zm-92-61l77 77s-35 16-46 77c-62 62-123 62-123 62s-24 36-46 15l93-94c55 12 50-39 37-52s-62-21-52 37L7 277c-21-21 15-46 15-46s0-62 62-123c51-5 77-46 77-46z'], [])
    )
  ).
icon(Name) -->
  {glyphicon(Name, Class, _)},
  html(span(class([glyphicon,Class]), [])).



%! icon_button(+Name)// is det.
%! icon_button(+Name, +Func)// is det.

icon_button(Name) -->
  icon_button(Name, _).


icon_button(Name, Func) -->
  {
    glyphicon(Name, Class, Title),
    (var(Func) -> Attrs = [] ; Attrs = [onclick=Func])
  },
  html(
    button([class=[btn,'btn-default',glyphicon,Class],title=Title|Attrs], [])
  ).



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



%! ignore(:Content_0)// is det.

ignore(Content_0) -->
  html_call(Content_0), !.
ignore(_) --> [].



%! image(+Spec)// is det.
%! image(+Spec, +Attrs)// is det.

image(Spec) -->
  image(Spec, []).


image(Spec, Attrs1) -->
  {
    spec_iri(Spec, Iri),
    merge_attrs(Attrs1, [src=Iri], Attrs2)
  },
  html(img(Attrs2, [])).



%! image_header(+Iri, :Content_0)// is det.

image_header(Iri, Content_0) -->
  {
    format(atom(Img0), 'url("~a")', [Iri]),
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
  html(header(style=BackgroundImg, Content_0)).



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



%! internal_link(+Spec)// is det.
%! internal_link(+Spec, :Content_0)// is det.
%! internal_link(+Spec, +Attrs, :Content_0)// is det.

internal_link(Spec) -->
  internal_link(Spec, _).


internal_link(Spec, Content_0) -->
  internal_link(Spec, [], Content_0).


internal_link(Spec, Attrs1, Content0_0) -->
  {
    spec_iri(Spec, Iri),
    merge_attrs(Attrs1, [href=Iri], Attrs2),
    (var_goal(Content0_0) -> Content_0 = Iri ; Content_0 = Content0_0)
  },
  html(a(Attrs2, Content_0)).



%! ip(+Ip)// is det.

ip(ip(A,B,C,D)) --> !,
  html("~d.~d.~d.~d"-[A,B,C,D]).
ip(IP) -->
  html("~w"-[IP]).



%! language_menu(+LTags)// is det.

language_menu(LTags) -->
  {
    setting(nlp:lrange, [LTag|_]),
    lstring(language, Lbl)
  },
  navbar_dropdown_menu(
    'language-menu',
    Lbl,
    language_menu_item(LTag),
    LTags
  ),
  js_script({|javascript(_)||
$( "#language-menu" ).change(function() {
  var str = "";
  $("select option:selected").each(function() {
    $.get("/change_language", {ltag: $(this).val()});
  });
});
  |}).

language_menu_item(LTag0, LTag) -->
  {(LTag0 == LTag -> T = [selected=selected] ; T = [])},
  html(option([value=LTag|T], \html_lstring(LTag))).



%! link(+Pair)// is det.
%! link(+Attrs, +Pair)// is det.
%
% Pair is of the form `Rel-Iri`, where Iri is based on Spec.

link(Pair) -->
  link([], Pair).


link(Attrs1, Rel-Spec) -->
  {
    spec_iri(Spec, Iri),
    merge_attrs(Attrs1, [href=Iri,rel=Rel], Attrs2)
  },
  html(link(Attrs2, [])).



%! link_button(+Iri, :Content_0)// is det.
%
% Generate an HTML link that looks like a button.

link_button(Iri, Content_0) -->
  html(a([class=[btn,'btn-default'],href=Iri], Content_0)).



%! linkedin_share// is det.

linkedin_share -->
  html([
    script([src='//platform.linkedin.com/in.js',type='text/javascript'],
      'lang: en_US'
    ),
    script(['data-counter'=right,type='IN/Share'], [])
  ]).



%! list(+Args)// is det.
%! list(:Writer_1, +Args)// is det.

list(Args) -->
  list(html_hook, Args).


list(Writer_1, Args) -->
  html([&(91),\html_seplist(Writer_1, Args),&(93)]).



%! mail_icon(+Uri)// is det.

mail_icon(Uri) -->
  external_link(Uri, [property='foaf:mbox'], [" ",\icon(mail)]).



%! mail_link_and_icon(+Uri)// is det.

mail_link_and_icon(Uri) -->
  external_link(Uri, [property='foaf:mbox'], [\icon(mail)," ",code(Uri)]).



%! menu// is det.
%
% This needs to be plugged into navbar//3 for argument Menu_0.

menu -->
  {
    http_current_request(Req),
    http_base_location_iri(Req, Loc),
    major_menus(MajorMenus)
  },
  html_maplist(major_menu(Loc), MajorMenus).


% Flat menu item.
major_menu(Loc, menu_item(Handle,Lbl)-[]) --> !,
  {
    http_link_to_id(Handle, Iri),
    (atom_postfix(Loc, Iri) -> Attrs = [class=active] ; Attrs = [])
  },
  html(li(Attrs, \internal_link(Iri, Lbl))).
% Nested menu items.
major_menu(_, MajorItem-MinorItems) -->
  dropdown_menu(menu_item(MajorItem), menu_item, MinorItems).


major_menus(MajorTrees) :-
  findall(
    Major-menu_item(Handle,Lbl),
    (html:menu_item(Major, Handle, Lbl), Handle \== user),
    Pairs
  ),
  asc_pairs_values(Pairs, MajorNodes),
  maplist(major_node_to_menu, MajorNodes, MajorTrees).


major_node_to_menu(
  menu_item(Handle1,Lbl1),
  menu_item(Handle1,Lbl1)-MinorNodes
) :-
  findall(
    Minor-menu_item(Handle2,Lbl2),
    html:menu_item(Handle1, Minor, Handle2, Lbl2),
    Pairs
  ),
  asc_pairs_values(Pairs, MinorNodes).


menu_item(menu_item(Handle,Lbl)) -->
  internal_link(link_to_id(Handle), Lbl).



%! meta(+Name, +Content)// is det.

meta(Name, Content) -->
  html(meta([name=Name,content=Content], [])).



%! meta_authors// is det.
%! meta_authors(+Authors:list(string))// is det.

meta_authors -->
  {findall(Str, html:author(Str), Strs)},
  meta_authors(Strs).


meta_authors(Strs) -->
  {atomics_to_string(Strs, ",", Str)},
  meta(author, Str).



%! meta_charset// is det.

meta_charset -->
  html(meta(charset='utf-8', [])).



%! meta_description(+Str)// is det.

meta_description(Str0) -->
  {string_or_strings(Str0, Str)},
  meta(description, Str).



%! meta_ie_latest// is det.
%
% Non-standard HTTP-like header that tells Internet Explorer to use
% the most recent version of its rendering engine.

meta_ie_latest -->
  html(meta(['http-equiv'='X-UA-Compatible',content='IE=edge'], [])).



%! meta_license(+Uri)// is det.

meta_license(Uri) -->
  link(license-Uri).



%! meta_viewport// is det.
%
% `width=device-width` instructs the page to match the screen’s width
% in device-independent pixels.  This allows the page to reflow
% content to match different screen sizes.
%
% Some browsers will keep the page’s width constant when rotating to
% landscape mode, and zoom rather than reflow to fill the screen.
% Adding the attribute `initial-scale=1` instructs browsers to
% establish a 1:1 relationship between CSS pixels and
% device-independent pixels regardless of device orientation, and
% allows the page to take advantage of the full landscape width.
%
% `user-scalable=yes` allows a user to zoom in/out on the viewport for
% accessibility.
%
% @compat Use a comma to separate attributes to ensure older browsers
%         can properly parse the attributes.

meta_viewport -->
  meta(viewport, 'width=device-width,initial-scale=1.0,user-scalable=yes').



%! navbar(:Brand_0, :Menu_0, :Right_0)// is det.

navbar(Brand_0, Menu_0, Right_0) -->
  {Target = target},
  html([
    nav(class=[navbar,'navbar-default','navbar-fixed-top'],
      div(class='container-fluid', [
        div(class='navbar-header', [
          \hamburger(Target),
          a([class='navbar-brand',href='/'], Brand_0)
        ]),
        div([class=[collapse,'navbar-collapse'],id=Target], [
          \navbar_menu(Menu_0),
          Right_0
        ])
      ])
    ),
    \navbar_resize
  ]).


navbar_menu(Content_0) -->
  html(ul(class=[nav,'navbar-nav'], Content_0)).


navbar_resize -->
  js_script({|javascript(_)||
var onResize = function() {
  // Apply dynamic padding at the top of the body according to
  // the fixed navbar height.
  $("body").css("padding-top", $(".navbar-fixed-top").height());
};
// Attach the function to the window resize event.
$(window).resize(onResize);
// Call it also when the page is ready after load or reload.
$(function() { onResize(); });
  |}).



%! navbar_brand_img(+Spec, +Attrs)// is det.

navbar_brand_img(Spec, Attrs1) -->
  {merge_attrs(Attrs1, [style='max-height:100%;'], Attrs2)},
  image(Spec, Attrs2).



%! navbar_dropdown_menu(+Name, +Lbl, :Writer_1, +Items)// is det.
%! navbar_dropdown_menu(+Attrs, +Name, +Lbl, :Writer_1, +Items)// is det.
%
% @tbd What does `role(search)` do?

navbar_dropdown_menu(Name, Lbl, Writer_1, L) -->
  navbar_dropdown_menu([], Name, Lbl, Writer_1, L).


navbar_dropdown_menu(Attrs1, Name, Lbl, Writer_1, L) -->
  {merge_attrs(Attrs1, [class=['navbar-form'],id=Name,role=search], Attrs2)},
  html(
    form(Attrs2,
      div(class='form-group', [
        label(for=Name, [Lbl,": "]),
        select([class=['form-control',selectpicker],id=Name],
          \html_maplist(Writer_1, L)
        )
      ])
    )
  ).



%! nonvar(:Content_0)// is det.
%! nonvar(:Writer_1, +Arg1)// is det.

nonvar(Content_0) -->
  ({var_goal(Content_0)} -> [] ; html_call(Content_0)).


nonvar(Writer_1, Arg1) -->
  ({var_goal(Writer_1)} -> [] ; html_call(Writer_1, Arg1)).



%! nowrap(:Content_0)// is det.

nowrap(Content_0) -->
  html(span(style='white-space: nowrap;', Content_0)).



%! number(+Format, +N)// is det.
%
% HTML component to emit a number.

number(Fmt, N) -->
  {number(N)}, !,
  html(Fmt-[N]).



%! once(:Writer_1)// is det.

once(Writer_1, X, Y) :-
  once(html_call(Writer_1, X, Y)).



%! operator(+Mod, +Op)// is det.

operator(Mod, op(Pred,Type,Name)) -->
  html([
    \nonvar(module_prefix, Mod),
    "op(",Pred,",",Type,",",Name,")"
  ]).



%! ordered_list(+Items)// is det.
%! ordered_list(:Writer_1, +Items)// is det.
%! ordered_list(+Attrs, :Writer_1, +Items)// is det.
%
% Generates an ordered HTML list.

ordered_list(L) -->
  ordered_list(html, L).


ordered_list(Writer_1, L) -->
  ordered_list([], Writer_1, L).


ordered_list(Attrs, Writer_1, L) -->
  html_list(Attrs, true, Writer_1, L).



%! pagination_links(+Result)// is det.

pagination_links(Result) -->
  {pagination:pagination_iris(Result, Pairs)},
  html_maplist(link, Pairs).



%! pagination_pager(+Result)// is det.

pagination_pager(Result) -->
  html(
    nav(
      ul(class=pager, [
        \pagination_pager_prev(Result),
        " ",
        \pagination_range(Result),
        " ",
        \pagination_pager_next(Result)
      ])
    )
  ).



%! pagination_pager_prev(+Result)// is det.

pagination_pager_prev(Result) -->
  {
    (   pagination_iri(Result, prev, Prev)
    ->  AAttrs = [href=Prev],
        LiAttrs = []
    ;   AAttrs = [],
        LiAttrs = [class=disabled]
    )
  },
  html(li(LiAttrs, a(AAttrs, "Previous"))).



%! pagination_pager_next(+Result)// is det.

pagination_pager_next(Result) -->
  {
    (   pagination_iri(Result, next, Next)
    ->  AAttrs = [href=Next],
        LiAttrs = []
    ;   AAttrs = [],
        LiAttrs = [class=disabled]
    )
  },
  html(li(LiAttrs, a(AAttrs, "Next"))).



%! pagination_range(+Result)// is det.

pagination_range(Result) -->
  {pagination_range(Result, Low-High)},
  html([\html_thousands(Low)," ⎯⎯⎯ ",\html_thousands(High)]).



%! pagination_result(+Result, :Html_1)// is det.
%
% Opts are required because it contains the `iri` based on which the
% backward/forward request IRIs are build.
%
% Result contains the following keys: ‘number_of_results’,
% ‘page’, ‘page_size’.
%
% Opts contains the following keys: ‘iri’, ‘query’.

pagination_result(Result, Html_1) -->
  html_call(Html_1, Result.results),
  pagination_pager(Result).



%! pagination_result_nonempty(+Result, :Content_1)// is det.

pagination_result_nonempty(Result, _) -->
  {pagination_is_empty(Result)}, !, [].
pagination_result_nonempty(Result, Content_1) -->
  pagination_result(Result, Content_1).



%! panel(+Id, :Header_0, :Content_0)// is det.
%! panel(+Open:boolean, +Id, :Header_0, :Content_0)// is det.

panel(Id, Header_0, Content_0) -->
  panel(false, Id, Header_0, Content_0).


panel(Open, Id, Header_0, Content_0) -->
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
        div(class='panel-body', Content_0)
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



%! pipe// is det.

pipe -->
  html([" ",span(class=pipe, "|")," "]).



%! pl_link// is det.

pl_link -->
  external_link('http://www.swi-prolog.org', "SWI-Prolog").



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



%! ref(+Label, :Content_0)// is det.

ref(Label, Content_0) -->
  {atom_concat('#', Label, Link)},
  html(a(href=Link, Content_0)).



%! reply_html_page(+Method:oneof([get,head]), +Style, :Head, :Body) is det.

reply_html_page(get, Style, Head, Body) :- !,
  reply_html_page(head, Style, Head, Body),
  phrase(page(Style, Head, Body), HTML),
  print_html(HTML).
reply_html_page(head, _, _, _) :-
  html_current_option(content_type(Type)),
  format('Content-type: ~w~n~n', [Type]).



%! reply_raw_file(+Spec) is det.
%
% Present an HTML file embedded using the server styling.  This is
% achieved by parsing the HTML and passing the parsed DOM to
% reply_html_page/3.

reply_raw_file(Spec) :-
  raw_page(Spec, Title, Content),
  reply_html_page(
    cp(default),
    title(Title),
    Content
  ).



%! reset_button// is det.
%! reset_button(:Content_0)// is det.

reset_button -->
  reset_button("Reset").


reset_button(Content_0) -->
  form_button0([type=reset], Content_0).



%! row_1(:ContentA_0)// is det.
%! row_1(+WidthsA, :ContentA_0)// is det.
%! row_1(+Attrs, +WidthsA, :ContentA_0)// is det.

row_1(ContentA_0) -->
  row_1(12, ContentA_0).


row_1(WidthsA, ContentA_0) -->
  row_1([], WidthsA, ContentA_0).


row_1(Attrs1, WidthsA, ContentA_0) -->
  {
    merge_attrs(Attrs1, [class='container-fluid'], Attrs2),
    widths(WidthsA, ClassesA)
  },
  html(
    div(Attrs2,
      div(class=row,
        div(class=[col|ClassesA], ContentA_0)
      )
    )
  ).



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



%! row_3(:ContentA_0, :ContentB_0, :ContentC_0)// is det.
%! row_3(
%!   +WidthsA,
%!   :ContentA_0,
%!   +WidthsB,
%!   :ContentB_0,
%!   +WidthsC,
%!   :ContentC_0
%! )// is det.

row_3(ContentA_0, ContentB_0, ContentC_0) -->
  row_3(4, ContentA_0, 4, ContentB_0, 4, ContentC_0).


row_3(WidthsA, ContentA_0, WidthsB, ContentB_0, WidthsC, ContentC_0) -->
  {
    maplist(widths, [WidthsA,WidthsB,WidthsC], [ClassesA,ClassesB,ClassesC])
  },
  html(
    div(class='container-fluid',
      div(class=row, [
        div(class=[col|ClassesA], ContentA_0),
        div(class=[col|ClassesB], ContentB_0),
        div(class=[col|ClassesC], ContentC_0)
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
  WidthsA,
  ContentA_0,
  WidthsB,
  ContentB_0,
  WidthsC,
  ContentC_0,
  WidthsD,
  ContentD_0
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



%! search_result(+Result, :Writer_1)// is det.
%
% Result must be a regular pagination resul plus the ‘pattern’ key
% containing the original search term.

search_result(Result, Writer_1) -->
  html([
    h1([
      \search_result_number(Result),
      "earch results for search string ",
      \html_sq(Result.pattern),
      ":"
    ]),
    \pagination_result(Result, Writer_1)
  ]).


search_result_number(Result) -->
  {get_dict(total_number_of_results, Result, TotalNumResults)}, !,
  html([\html_thousands(TotalNumResults)," s"]).
search_result_number(_) -->
  html("S").



%! streamer(:Content_0)// is det.

streamer(Content_0) -->
  html(div(class=streamer, Content_0)).



%! submit_button// is det.
%! submit_button(:Content_0)// is det.

submit_button -->
  submit_button("Submit").


submit_button(Content_0) -->
  button([type=submit], Content_0).



%! table(:BodyContent_0)// is det.
%! table(:Header_0, :Body_0)// is det.
%! table(:CaptionContent_0, :HeaerRow_0, :BodyContent_0)// is det.

table(Body_0) -->
  table(_, Body_0).


table(Header_0, Body_0) -->
  table(_, Header_0, Body_0).


table(Caption_0, Header_0, Body_0) -->
  html(
    table(class=[block,table,'table-condensed','table-striped'], [
      \table_caption(Caption_0),
      \table_header(Header_0),
      tbody(Body_0)
    ])
  ).



%! table_caption(:Content_0)// is det.

table_caption(Content_0) -->
  {var_goal(Content_0)}, !, [].
table_caption(Content_0) -->
  html(Content_0).



%! table_data_cell(+Term)// is det.
%! table_data_cell(:CellWriter_1, +Term)// is det.

table_data_cell(Term) -->
  table_data_cell(html_hook, Term).


table_data_cell(CellWriter_1, Term) -->
  html(td(\html_call(CellWriter_1, Term))).



%! table_data_row(+Row)// is det.
%! table_data_row(:CellWriter_1, +Row)// is det.

table_data_row(Row) -->
  table_data_row(html_hook, Row).


table_data_row(CellWriter_1, Row) -->
  html(tr(\html_maplist(table_data_cell(CellWriter_1), Row))).



%! table_header(:HeaderContent_0)// is det.

table_header(HeaderContent_0) -->
  {var_goal(HeaderContent_0)}, !, [].
table_header(HeaderContent_0) -->
  html(thead(HeaderContent_0)).



%! table_header_cell(:CellWriter_1, +Term)// is det.

table_header_cell(CellWriter_1, Term) -->
  html(th(\html_call(CellWriter_1, Term))).



%! table_header_row(+Row)// is det.
%! table_header_row(:CellWriter_1, +Row)// is det.

table_header_row(Row) -->
  table_header_row(html_hook, Row).


table_header_row(CellWriter_1, Row) -->
  html(tr(\html_maplist(table_header_cell(CellWriter_1), Row))).



%! table_tree(+Tree)// is det.
%! table_tree(:CellWriter_1, +Tree)// is det.

table_tree(Tree) -->
  table_tree(html_hook, Tree).


table_tree(CellWriter_1, Tree) -->
  table_tree(false, CellWriter_1, Tree).


table_tree(false, CellWriter_1, t(Node,SubTrees)) --> !,
  html(
    tr([
      \table_tree_cell(true, CellWriter_1, Node, SubTrees),
      \table_trees(true, CellWriter_1, SubTrees)
    ])
  ).
table_tree(true, CellWriter_1, t(Node,SubTrees)) --> !,
  table_tree_cell(true, CellWriter_1, Node, SubTrees),
  table_trees(true, CellWriter_1, SubTrees).
table_tree(_, CellWriter_1, H) -->
  html(td(\html_call(CellWriter_1, H))).



table_tree_cell(InRow, CellWriter_1, L, _) -->
  {is_list(L)}, !,
  table_trees(InRow, CellWriter_1, L).
table_tree_cell(_, CellWriter_1, Node, L) -->
  {length(L, N)},
  html(td(rowspan(N), \html_call(CellWriter_1, Node))).



%! table_trees(+Trees)// is det.
%! table_trees(:CellWriter_1, +Trees)// is det.

table_trees(Trees) -->
  table_trees(html, Trees).


table_trees(CellWriter_1, Trees) -->
  table_trees(false, CellWriter_1, Trees).


table_trees(InRow, CellWriter_1, [t(Node,[H|T])|Trees]) --> !,
  table_tree(InRow, CellWriter_1, t(Node,[H|T])),
  table_trees(false, CellWriter_1, Trees).
table_trees(InRow, CellWriter_1, [H|T]) -->
  {is_list(H)}, !,
  html(td(\html_set(CellWriter_1, H))),
  table_trees(InRow, CellWriter_1, T).
table_trees(InRow, CellWriter_1, [H|T]) --> !,
  table_tree(InRow, CellWriter_1, H),
  table_trees(InRow, CellWriter_1, T).
table_trees(_, _, []) --> !, [].



%! table_content(:Cell_1, +Rows)// is det.

table_content(Cell_1, [head(HeaderRow)|DataRows]) -->
  table(
    \table_header_row(Cell_1, HeaderRow),
    \html_maplist(table_data_row(Cell_1), DataRows)
  ).



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



%! title(+Strs)// is det.

title(Strs) -->
  {atomics_to_string(Strs, " ⎯ ", Str)},
  html(title(Str)).



%! tooltip(+Str, :Content_0)// is det.

tooltip(Str, Content_0) -->
  html(span(['data-toggle'=tooltip,title=Str], Content_0)).



%! truncate(+Str, +MaxLen)// is det.

truncate(Str, MaxLen) -->
  {string_truncate(Str, MaxLen, Prefix)},
  ({Str == Prefix} -> html(Str) ; tooltip(Str, Prefix)).



%! twitter_follow0(:Content_0)// is det.
%! twitter_follow0(+User, :Content_0)// is det.

twitter_follow0(Content_0) -->
  {setting(html:twitter_profile, User)},
  twitter_follow0(User, Content_0).


twitter_follow0(User, Content_0) -->
  {twitter_user_iri(User, Iri)},
  html(a(href=Iri, html_call(Content_0))).



%! twitter_follow_img// is det.
%! twitter_follow_img(+User)// is det.

twitter_follow_img -->
  {setting(html:twitter_profile, User)},
  twitter_follow_img(User).


twitter_follow_img(User) -->
  {lstring(follow_us_on_x, ["Twitter"], Str)},
  tooltip(Str, \twitter_follow0(User, \twitter_img0)).



%! twitter_follow_txt// is det.
%! twitter_follow_txt(+User)// is det.

twitter_follow_txt -->
  {setting(html:twitter_profile, User)},
  twitter_follow_txt(User).


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



%! twitter_grid(+Iri, +Title)// is det.

twitter_grid(Iri, Title) -->
  html(a([class='twitter-grid',href=Iri], Title)).



%! twitter_img0// is det.

twitter_img0 -->
  {http_absolute_location(img('twitter.png'), Loc)},
  html(img([alt="Twitter",src=Loc], [])).



%! twitter_mention// is det.
%! twitter_mention(+User)// is det.

twitter_mention -->
  {setting(html:twitter_profile, User)},
  twitter_mention(User).


twitter_mention(User) -->
  {
    uri_query_components(Query, [screen_name(User)]),
    uri_components(
      Iri,
      uri_components(https,'twitter.com','/intent/tweet',Query,_)
    )
  },
  html(
    a(
      [class='twitter-mention-button','data-show-count'=false,href=Iri],
      [\html_lstring(tweet_to)," @",User]
    )
  ).



%! twitter_profile// is det.
%! twitter_profile(+User)// is det.

twitter_profile -->
  {setting(html:twitter_profile, User)},
  twitter_profile(User).


twitter_profile(User) -->
  {twitter_user_iri(User, Iri)},
  html(
    a([class='twitter-timeline',href=Iri], [
      \html_lstring(tweets_by),
      " ",
      User
    ])
  ).



%! twitter_share(+Iri, +Title)// is det.

twitter_share(Iri, Title) -->
  {
    lstring(share_x_on_y, [Title,"Twitter"], Str),
    uri_query_components(Query, [text(Title),url(Iri)]),
    uri_components(Url, uri_components(https,'twitter.com','/share',Query,_))
  },
  tooltip(Str, a(href=Url, \twitter_img0)).



%! twitter_tweet(+Tweet)// is det.

twitter_tweet(Tweet) -->
  {
    uri_query_components(Query, [omit_script(true),url(Tweet)]),
    uri_components(
      Iri,
      uri_components(https,'publish.twitter.com','/oembed',Query,_)
    ),
    json_read_any(Iri, Dict),
    atom_string(A, Dict.html)
  },
  [A].



%! unless(:Unless_0, :Then_0)// is det.

unless(Unless_0, Then_0) -->
  ({call(Unless_0)} -> [] ; html_call(Then_0)).



%! unordered_list(+Items)// is det.
%! unordered_list(:Writer_1, +Items)// is det.
%! unordered_list(+Attrs, :Writer_1, +Items)// is det.
%
% Generates an unordered HTML list.

unordered_list(L) -->
  unordered_list(html, L).


unordered_list(Writer_1, L) -->
  unordered_list([], Writer_1, L).


unordered_list(Attrs, Writer_1, L) -->
  html_list(Attrs, false, Writer_1, L).



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



%! user_menu(:UserName_2, :UserImg_2)// is det.

user_menu(UserName_2, UserImg_2) -->
  {
    current_user(User), !,
    logout_link(Link)
  },
  html(
    ul(class=[nav,'navbar-nav','navbar-right'],
      \dropdown_menu(
        [id='user-menu'],
        \user_menu_top(User, UserName_2, UserImg_2),
        \internal_link(Link, [id='logout-button'], "Logout")
      )
    )
  ).
user_menu(_, _) -->
  {login_link(Link)},
  html(
    form(
      Link,
      [class=['navbar-form','navbar-right'],id='user-menu'],
      div(class='form-group',
        \submit_button(\internal_link(Link, "Login"))
      )
    )
  ).


user_menu_top(User, UserName_2, UserImg_2) -->
  {
    call(UserName_2, User, Name),
    call(UserImg_2, User, Img)
  },
  html([img([alt=Name,src=Img], []),Name]).



%! vote_down(+Vote:integer)// is det.

% @tbd Show as inactive and selected.
vote_down(Vote) -->
  {Vote < 0}, !,
  icon_button(vote_down).
vote_down(Vote) -->
  {Vote =:= 0}, !,
  icon_button(vote_down).
% @tbd Show as inactive and not selected.
vote_down(Vote) -->
  {Vote > 0}, !,
  icon_button(vote_down).



%! vote_up(+Vote:integer)// is det.

% @tbd Show as inactive and selected.
vote_up(Vote) -->
  {Vote < 0}, !,
  icon_button(vote_up).
vote_up(Vote) -->
  {Vote =:= 0}, !,
  icon_button(vote_up).
% @tbd Show as inactive and not selected.
vote_up(Vote) -->
  {Vote > 0}, !,
  icon_button(vote_up).





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



%! fb_user_iri(+User, -Iri) is det.

fb_user_iri(User, Iri) :-
  atomic_list_concat(['',User], /, Path),
  uri_components(Iri, uri_components(https,'facebook.com',Path,_,_)).



%! form_button0(+Attrs, :Content_0)// is det.
%! form_button0(+Attrs, :Content_0, +Opts)// is det.

form_button0(Attrs, Content_0) -->
  form_button0(Attrs, Content_0, []).


form_button0(Attrs, Content_0, Opts) -->
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
        \button(Attrs, Content_0)
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



%! glyphicon(+Name, -Class, -Title) is det.

glyphicon(Name, Class, Title) :-
  glyphicon0(Name, Class0, Title),
  atomic_list_concat([glyphicon,Class0], -, Class).



%! glyphicon0(?Name, ?Class, ?Title) is nondet.

% CRUD = Create, Read, Update, Delete.
glyphicon0(cancel,         erase,          "Cancel").
glyphicon0(copy,           copy,           "Copy").
glyphicon0(create,         pencil,         "Create").
glyphicon0(delete,         trash,          "Delete").
glyphicon0(download,      'save-file',     "Download").
glyphicon0(external_link,  link,           "Follow link").
glyphicon0(internal_link, 'chevron-right', "Follow link").
glyphicon0(mail,           envelope,       "Send email").
glyphicon0(tag,            tags,           "").
glyphicon0(time,           time,           "Date/time").
glyphicon0(user,           user,           "Log me in").
glyphicon0(vote_down,     'thumbs-down',   "Vote up").
glyphicon0(vote_up,       'thumbs-up',     "Vote down").
glyphicon0(web,            globe,          "Visit Web site").



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



%! merge_attrs(+Attrs1, +Attrs2, -Attrs3) is det.
%
% Merge two lists of HTML attributes into one.

merge_attrs([], L, L) :- !.
% HTTP attribute with multiple values.
merge_attrs([Key=Val1|T1], L2a, [Key=Val3|T3]):-
  attr_multi_val(Key),
  selectchk(Key=Val2, L2a, L2b), !,
  maplist(ensure_list, [Val1,Val2], [Val1L,Val2L]),
  append(Val1L, Val2L, ValL),
  sort(ValL, Val3),
  merge_attrs(T1, L2b, T3).
% HTTP attribute with single value.
merge_attrs([Key=_|T1], L2a, [Key=Val2|T3]) :-
  selectchk(Key=Val2, L2a, L2b), !,
  merge_attrs(T1, L2b, T3).
merge_attrs([H|T1], L2, [H|T3]):-
  merge_attrs(T1, L2, T3).


attr_multi_val(class).



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



%! spec_iri(+Spec, -Iri) is det.

spec_iri(link_to_id(HandleId), Iri) :- !,
  http_link_to_id(HandleId, Iri).
spec_iri(link_to_id(HandleId,Query0), Iri) :- !,
  maplist(q_query_term, Query0, Query), %HACK
  http_link_to_id(HandleId, Query, Iri).
spec_iri(Spec, Iri) :-
  http_absolute_location(Spec, Iri).



%! twitter_user_iri(+User, -Iri) is det.

twitter_user_iri(User, Iri) :-
  atomic_list_concat(['',User], /, Path),
  uri_components(Iri, uri_components(https,'twitter.com',Path,_,_)).



%! widths(
%!   +Widths:or([list(between(1,12)),between(1,12)]),
%!   -Classes:list(atom)
%! ) is det.
%! widths(
%!   +Offset:boolean,
%!   +Widths:or([list(between(1,12)),between(1,12)]),
%!   -Classes:list(atom)
%! ) is det.

widths(Widths, Classes) :-
  widths(false, Widths, Classes).


widths(Offset, Widths, Classes) :-
  is_list(Widths), !,
  maplist(width0(Offset), [lg,md,sm,xs], Widths, Classes).
widths(Offset, Width, Classes) :-
  widths(Offset, [Width,Width,Width,Width], Classes).


%! width0(
%!   +Offset:boolean,
%!   +Mode:oneof([lg,md,sm,xs]),
%!   +Widths:or([list(between(1,12)),between(1,12)]),
%!   -Class:atom
%! ) is det.

width0(Offset, Mode, Width, Class) :-
  (   Offset == true
  ->  Comps = [col,Mode,offset,Width]
  ;   Comps = [col,Mode,Width]
  ),
  atomic_list_concat(Comps, -, Class).
