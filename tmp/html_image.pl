:- module(
  html_image,
  [
    html_image_thumbnail//2, % +Image:compound
                             % +Options:list(compound)
    html_image_thumbnail_box//2, % +Images:list(compound)
                                 % +Options:list(compound)
    html_image_thumbnail_box_grid//4 % +Columns:positive_integer
                                     % +Rows:positive_integer
                                     % +Images:list(compound)
                                     % +Options:list(compound)
  ]
).

/** <module> HTML image

Support for displaying an image gallery in HTML.

@author Wouter Beek
@version 2015/08, 2015/10
*/

:- use_module(library(html/html_resource)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(image_ext)).
:- use_module(library(math/dimension)).
:- use_module(library(option)).

:- html_resource(css(image), [requires([css('image.css')]),virtual(true)]).

:- predicate_options(html_image_thumbnail/2, 2, [
     height(+positive_integer),
     width(+positive_integer)
   ]).
:- predicate_options(html_image_thumbnail_box/2, 2, [
     pass_to(html_image_thumbnail/2, 2)
   ]).
:- predicate_options(html_image_thumbnail_box_grid/4, 4, [
     pass_to(html_image_thumbnail_box_rows/4, 4)
   ]).
:- predicate_options(html_image_thumbnail_box_row/4, 4, [
     pass_to(html_image_thumbnail_box/2, 2),
     pass_to(html_image_thumbnail_box_row/4, 4)
   ]).
:- predicate_options(html_image_thumbnail_box_rows/4, 4, [
     pass_to(html_image_thumbnail_box_row/4, 4)
   ]).





%! html_image_thumbnail(+Image:compound, +Options:list(compound))// is det.

html_image_thumbnail(Image, Opts) -->
  {
    % Whether to include an alternative image description or not.
    ImgAttrs0 = [height=H3,src=RelUri,width=W3],
    (   Image = image(RelFile,Alt)
    ->  merge_options([alt=Alt], ImgAttrs0, ImgAttrs)
    ;   Image = image(RelFile)
    ->  ImgAttrs = ImgAttrs0
    ),
    
    % Image file.
    http_absolute_location(img(RelFile), RelUri, []),
    absolute_file_name(img(RelFile), File, [access(read)]),
    
    % Container cube.
    option(width(W1), Opts, 100),
    option(height(H1), Opts, 100),
    
    % Content cube.
    image_dimensions(File, W2, H2),
    
    % Scaling.
    dimension_scale([W2,H2], [W1,H1], _, [W3,H3]),
    format(atom(Style), 'height: ~a; width: ~a;', [H3,W3])
  },
  % Make the image clickable, linking to the image file.
  html(
    div([class=image_thumbnail,style=Style],
      a([href=RelUri,target='_blank'], img(ImgAttrs, []))
    )
  ).


%! html_image_thumbnail_box(+Image:compound, +Options:list(compound))// is det.
% Generates an HTML thumbnail box for an image.

html_image_thumbnail_box(Image, Opts) -->
  % Construe the DIV containing the image, the link, and the description.
  html(
    div(class=image_thumbnail_box, [
      \html_image_thumbnail(Image, Opts),
      \html_image_thumbnail_caption(Image)
    ])
  ).


%! html_image_thumbnail_box_grid(
%!   +Columns:positive_integer,
%!   +Rows:positive_integer,
%!   +Images:list(compound),
%!   +Options:list(compound)
%! )// is det.
% Generates an HTML grid of boxes containing image thumbnails.

html_image_thumbnail_box_grid(Cols, Rows, L, Opts) -->
  html(
    div(class=image_thumbnail_box_grid,
      \html_image_thumbnail_box_rows(Cols, Rows, L, Opts)
    )
  ).


%! html_image_thumbnail_box_row(
%!   +Columns:positive_integer,
%!   +Images:list(compound)
%!   -Rest:list(compound),
%!   +Options:list(compound)
%! )// is det.
% Generates an HTML row of boxes containing image thumbnails.
% The rows are part of a grid.

html_image_thumbnail_box_row(_, [], [], _) --> !.
html_image_thumbnail_box_row(0, Rest, Rest, _) --> !.
html_image_thumbnail_box_row(Cols1, [H|T], Rest, Opts) -->
  {Cols2 is Cols1 - 1},
  html([
    \html_image_thumbnail_box(H, Opts),
    \html_image_thumbnail_box_row(Cols2, T, Rest, Opts)
  ]).


%! html_image_thumbnail_box_rows(
%!   +Columns:positive_integer,
%!   +Rows:positive_integer,
%!   +Images:list(compound),
%!   +Options:list(compound)
%! )// is det.
% Generates HTML rows of boxes containing image thumbnails.
% The rows are part of a grid.

html_image_thumbnail_box_rows(_, _, [], _) --> !.
html_image_thumbnail_box_rows(_, 0, _, _) --> !.
html_image_thumbnail_box_rows(Cols, Rows1, L1, Opts) -->
  {Rows2 is Rows1 - 1},
  html([
    div(class=image_row,
      \html_image_thumbnail_box_row(Cols, L1, L2, Opts)
    ),
    \html_image_thumbnail_box_rows(Cols, Rows2, L2, Opts)
  ]).


%! html_image_thumbnail_caption(+Image:compound)// is det.
% Generates an HTML image caption.

html_image_thumbnail_caption(image(_,Caption)) --> !,
  html(div(class=image_caption, Caption)).
html_image_thumbnail_caption(_) --> [].
