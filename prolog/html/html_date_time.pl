:- module(
  html_date_time,
  [
    html_date_time//1, % +Something
    html_date_time//2  % +Something, +Opts
  ]
).

/** <module> HTML date/time

Generates human- and machine-readable HTML for date/times.

@author Wouter Beek
@version 2015/08, 2015/12, 2016/02, 2016/04, 2016/06
*/

:- use_module(library(date_time/date_time)).
:- use_module(library(default)).
:- use_module(library(dict_ext)).
:- use_module(library(html/html_date_time_human)).
:- use_module(library(html/html_date_time_machine)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(settings)).





%! html_date_time(+Something)// is det.
%! html_date_time(+Something, +Opts)// is det.
%
% The following options are supported:
%
%   * ltag(+oneof([en,nl]) The language tag denoting the natural
%     language that is used to display human-readable content in.
%     Default is `en`.
%
%   * masks(+list(atom)) The following masks are supported: `none`,
%     `year`, `month`, `day`, `hour`, `minute`, `second`, `offset`.
%     Default is `[]`.
%
%   * month_abbr(+boolean) Whether the human-readable representation
%   of month names should use abbreviated names or not.  Default is
%   `false`.

html_date_time(Something) -->
  {current_ltag([en,nl], LTag)}, !,
  html_date_time(Something, _{ltag: LTag}).


html_date_time(Something, Opts) -->
  {
    something_to_date_time(Something, DT),
    html_machine_date_time(DT, MS),
    dict_get(masks, Opts, [], Masks),
    date_time_masks(Masks, DT, MaskedDT)
  },
  html(time(datetime=MS, \html_human_date_time(MaskedDT, Opts))).
