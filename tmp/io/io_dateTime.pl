:- module(
  io_dateTime,
  [
    latest_dateTime_file/2, % +DirectorySpec:compound
                            % -File:atom
    latest_dateTime_file/3, % +DirectorySpec:compound
                            % ?Extension:atom
                            % -File:atom
    latest_file/2 % +Files:list(atom)
                  % -File:atom
  ]
).

/** <module> I/O date

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(filesex)).
:- use_module(library(lists)).





%! latest_dateTime_file(+DirectorySpec:compound, -File:atom) is det.

latest_dateTime_file(DirSpec, Latest):-
  latest_dateTime_file(DirSpec, _, Latest).

%! latest_dateTime_file(
%!   +DirectorySpec:compound,
%!   ?Extension:atom,
%!   -File:atom
%! ) is det.

latest_dateTime_file(DirSpec, Ext, Latest):-
  absolute_file_name(DirSpec, Dir, [access(read),file_type(directory)]),

  % Year
  directory_files(Dir, Years0),
  include(atom_phrase(integer(_)), Years0, Years),
  max_member(Year, Years),
  directory_file_path(Dir, Year, YearDir),

  % Month
  directory_files(YearDir, Months0),
  include(atom_phrase(integer(_)), Months0, Months),
  max_member(Month, Months),
  directory_file_path(YearDir, Month, MonthDir),

  % Day
  directory_files(MonthDir, Days0),
  include(atom_phrase(integer(_)), Days0, Days),
  max_member(Day, Days),
  directory_file_path(MonthDir, Day, DayDir),

  % Time
  directory_files(DayDir, DayFiles0),
  include(atom_phrase(date_or_time), DayFiles0, DayFiles),
  max_member(LatestBase, DayFiles),
  (   var(Ext)
  ->  LatestFile = LatestBase
  ;   file_name_extension(LatestBase, Ext, LatestFile)
  ),
  directory_file_path(DayDir, LatestFile, Latest).

date_or_time -->
  integer(_),
  "_",
  integer(_),
  "_",
  integer(_),
  ....



%! latest_file(+Files:list(atom), -Latest:atom) is det.
% Returns the most recently created or altered file from within a list of
% files.

latest_file([H|T], Latest):-
  dateTime_file(H, Time),
  latest_file(T, Time-H, Latest).

latest_file([], _-Latest, Latest).
latest_file([H|T], Time1-File1, Latest):-
  dateTime_file(H, NewTime),
  (   NewTime > Time1
  ->  Time2 = NewTime,
      File2 = H
  ;   Time2 = Time1,
      File2 = File1
  ),
  latest_file(T, Time2-File2, Latest).

