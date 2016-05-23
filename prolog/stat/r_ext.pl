:- module(
  r_ext,
  [
    r_plot/3 % +Pairs, -File, +Opts
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
:- use_module(library(real)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(pairs)).





%! r_plot(+Pairs, -File, +Opts) is det.
%
% Plots binary rows (X and Y).
%
% A *bucket* is a consecutive collection of X values.
%
% The following options are supported:
%   - caption(+stirng)
%   - max(+positive_integer)
%     Default is 15.
%   - xlabel(+string)
%     Default is "X".
%   - ylabel(+string)
%     Default is "Y".

r_plot(Pairs1, File, Opts) :-
  option(caption(Caption), Opts, ""),
  option(max(Max), Opts, 15),
  option(xlabel(XLbl), Opts, "X"),
  option(ylabel(YLbl), Opts, "Y"),
  discretize(Pairs1, Max, Pairs2),
  pairs_keys_values(Pairs2, Ys, Xss),
  maplist(interval_label, Xss, Xs),
  writeln(Xs),
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


%! discretize(+Pairs, +Max, -DiscretizedPairs) is det.

discretize(Pairs1, Max, Pairs2) :- 
  length(Pairs1, Len),
  (   Len =< Max
  ->  Pairs2 = Pairs1
  ;   Size is ceil(Len / Max),
      discretize0(Pairs1, Size, Pairs2)
  ).

discretize0([], _, []) :- !.
discretize0(L1, N, [Y-Xs|T2]) :-
  length(L1a, N),
  append(L1a, L1b, L1), !,
  pairs_keys_values(L1a, Xs, Ys),
  sum_list(Ys, Y),
  discretize0(L1b, N, T2).
discretize0(L, _, L).



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
  dcg_print_term(Term).
