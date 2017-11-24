:- module(
  os_ext,
  [
    exists_program/1,    % +Program
    file_format/2,       % ?Format, ?Programs
    file_format/3,       % ?Format, ?MediaType, ?Programs
    format_media_type/2, % ?Format, ?MediaType
    open_format/2,       % +Format, +File
    os/1,                % ?Os
    os_path/1,           % ?Directory
    process_flags/3      % :Goal_2, +Args, -Flags
  ]
).

/** <module> OS extensions

@author Wouter Beek
@version 2017/04-2017/11
*/

:- use_module(library(process)).

:- discontiguous
    file_format/2,
    file_format/3.

:- meta_predicate
    process_flags(2, +, -).





%! exists_program(+Program:atom) is semidet.
%
% Succeeds if the given program can be run from PATH.

exists_program(Program) :-
  os_path(Prefix),
  atomic_list_concat([Prefix,Program], /, Exe),
  access_file(Exe, execute), !.



%! file_format(?Format:atom, ?Programs:list(atom)) is nondet.
%! file_format(?Format:atom, ?MediaType:compound,
%!             ?Programs:list(atom)) is nondet.

file_format(Format, Programs) :-
  file_format(Format, _, Programs).


% Windows Bitmap [Microsoft]
file_format(bmp, image/bmp, [eog]).
file_format(dot, text/'vnd.graphviz', []).
% Graphics Interchange Format (GIF) [CompuServe]
file_format(gif, image/gif, [eog]).
% Windows Icon [Microsoft]
% Microsoft uses Media Type ‘image/x-icon’.
file_format(ico, image/'vnd.microsoft.icon', [eog]).
% Joint Photographic Experts Group (JPEG) [ISO]
file_format(jpeg, image/jpeg, [eog]).
% JavaScript Object Notation (JSON)
file_format(json, application/json, []).
% PiCture EXchange [ZSoft Corporation]
% Native file format of PC Paintbrush.
file_format(pcx, image/'vnd.zbrush.pcx', [eog]).
% Portable Network Graphics (PNG)
file_format(png, image/png, [eog]).
% Portable Bitmap Format (PBM)
file_format(pbm, image/'x-portable-bitmap', []).
% Portable Graymap Format (PGM)
file_format(pgm, image/'x-portable-graymap', []).
% Portable Pixmap Format (PPM)
file_format(ppm, image/'x-portable-pixmap', []).
% Portable Anymap Format (PNM)
file_format(pnm, image/'x-portable-anymap', [eog]).
% Portable Document Format (PDF)
file_format(pdf, application/pdf, [evince,xpdf]).
% PostScript (PS)
file_format(ps, application/postscript, []).
% Sun Raster [Sun Microsystems]
file_format(ras, [eog]).
% Scalable Vector Graphics (SVG) [W3C]
file_format(svg, image/'svg+xml', [eog]).
% Truevision Advanced Raster Graphics Adapter (TARGA) [Truevision
% Inc.]
file_format(tga, image/'x-targa', [eog]).
% Tagged Image File Format (TIFF) [Aldus]
file_format(tiff, image/tiff, [eog]).
% Wireless Application Protocol Bitmap Format (Wireless Bitmap) [WAP
% Forum]
file_format(wbmp, image/'vnd.wap.bmp', [eog]).
% X BitMap (XBM)
file_format(xbm, image/'x-bitmap', [eog]).
% X PixMap (XPM) [BULL Research]
file_format(xpm, image/'x-xpixmap', [eog]).
% WebP [Google]
file_format(webp, image/webp, []).



%! format_media_type(?Format:atm, ?MediaType:compound) is nondet.

format_media_type(Format, MediaType) :-
  file_format(Format, MediaType, _).



%! open_format(+Format:atom, +File:atom) is det.
%
% Opens the given PDF file.

open_format(Format, File) :-
  file_format(Format, Programs),
  member(Program, Programs),
  exists_program(Program), !,
  process_create(
    path(Program),
    [file(File)],
    [process(Pid),stderr(pipe(ProcErr)),stdout(pipe(ProcOut))]
  ),
  thread_create(copy_stream_data(ProcErr, user_error), _, [detached(true)]),
  thread_create(copy_stream_data(ProcOut, user_output), _, [detached(true)]),
  process_wait(Pid, exit(Status)),
  (Status =:= 0 -> true ; print_message(warning, process_status(Status))).



%! os(+Os:oneof([mac,unix,windows])) is semidet.
%! os(-Os:oneof([mac,unix,windows])) is det.
%
% Succeeds if Os denotes the current Operating System.

os(mac) :-
  current_prolog_flag(apple, true), !.
os(unix) :-
  current_prolog_flag(unix, true), !.
os(windows) :-
  current_prolog_flag(windows, true), !.



%! os_path(+Directory:atom) is semidet.
%! os_path(-Directory:atom) is nondet.
%
% Succeeds if Directory is on the OS PATH.

os_path(OsDir) :-
  getenv('PATH', Path),
  os_path_separator(Sep),
  atomic_list_concat(Dirs, Sep, Path),
  member(Dir, Dirs),
  prolog_to_os_filename(OsDir, Dir).



%! os_path_separator(-Separator:oneof([:,;])) is det.

os_path_separator(Sep) :-
  os(Os),
  os_path_separator(Os, Sep).

os_path_separator(mac, :).
os_path_separator(unix, :).
os_path_separator(windows, ;).



%! process_flags(:Goal_2, +Args:list(compound), -Flags:list(atom)) is det.

process_flags(_, [], []).
process_flags(Goal_2, [H1|T1], [H2|T2]) :-
  call(Goal_2, H1, H2), !,
  process_flags(Goal_2, T1, T2).
process_flags(Goal_2, [_|T], L) :-
  process_flags(Goal_2, T, L).
