:- module(
  r_ext,
  [
    r_plot/1, % +Rows
    r_plot/2, % +Rows,        +Opts
    r_plot/3  % +Rows, -File, +Opts
  ]
).

/** <module> humR

@author Wouter Beek
@version 2013/10, 2016/05
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(default)).
:- use_module(library(list_ext)).
:- use_module(library(option)).
:- use_module(library(os_ext)).
:- use_module(library(real)).
:- use_module(library(yall)).
:- use_module(library(q/rdf_print)).





%! r_plot(+Rows) is det.
%! r_plot(+Rows, +Opts) is det.
%! r_plot(+Rows, -File, +Opts) is det.
%
% Plots binary rows (X and Y).
%
% A *bucket* is a consecutive collection of X values.
%
% The following options are supported:
%   - caption(+string)
%   - max(+positive_integer)
%     Default is 15.
%   - xlabel(+string)
%     Default is "X".
%   - ylabel(+string)
%     Default is "Y".

r_plot(Rows) :-
  r_plot(Rows, []).


r_plot(Rows, Opts) :-
  r_plot(Rows, File, Opts),
  run_process(eog, [file(File)]).


r_plot(Rows, File, Opts) :-
  option(caption(Caption), Opts, ""),
  option(max(Max), Opts, 15),
  option(xlabel(XLbl), Opts, "X"),
  option(ylabel(YLbl), Opts, "Y"),
  maplist(list_split, Rows, Xs0, Ys),
  maplist([X0,X]>>rdf_print_term(X0, _{out: string(X)}), Xs0, Xs),
  absolute_file_name(test, File, [access(write),extensions([svg])]),
  <- svg(+File),
  <- barplot(
       Ys,
       % Columns are not stacked on top of each other, but are placed
       % beside each other.
       beside='TRUE',
       % Scaling of the font size of x-axis labels.
       'cex.names'=0.8,
       % Colors help distinguish between the compared properties.
       col=rainbow(Max),
       % Labels perpendicular to axis.
       las=2,
       % Logarithmic scale.
       %%%%log=+y,
       % Caption.
       main=+Caption,
       % Text labels for x-axis.
       'names.arg'=Xs,
       ylab=+YLbl
     ),
  % Label for x-axis needs special positioning.
  <- title(xlab=+XLbl, line=5),
  %<- legend(
  %     +topleft,
  %     bg=+white,
  %     fill=rainbow(Max),
  %     legend=Caption
  %   ),
  <- 'dev.off'().


%! discretize(+Rows, +Max, -Xs, -Ys) is det.

discretize(Rows, Max, Xs, Ys) :- 
  length(Rows, Len),
  Size is ceil(Len / Max),
  discretize0(Rows, Size, Xs, Ys).

discretize0([], _, [], []) :- !.
discretize0(L1, N, [XLbl|T2], [YSum|T3]) :-
  (length(L1a, N), append(L1a, L1b, L1) -> true ; L1a = L1, L1b = []),
  maplist(list_split, L1a, Xs, Ys),
  interval_label(Xs, XLbl),
  sum_list(Ys, YSum),
  discretize0(L1b, N, T2, T3).



%! interval_label(+Interval, -Lbl) is det.
% Returns a descriptive label for the given set of values.
%
% Single values are considered to be intervals of length 1.
% In these cases the label of this single value is given.

interval_label(Interval, Lbl) :-
  string_phrase(interval_label(Interval), Lbl).


interval_label([X]) --> !,
  interval_label(X).
interval_label(L) -->
  {
    is_list(L), !,
    first(L, First),
    last(L, Last)
  },
  "[",
  interval_label(First),
  ",",
  interval_label(Last),
  "]".
interval_label(Term) -->
  dcg_rdf_print_term(Term).
