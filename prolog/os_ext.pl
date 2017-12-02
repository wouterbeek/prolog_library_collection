:- module(
  os_ext,
  [
    exists_program/1,    % +Program
    format_media_type/2, % ?Format, ?MediaType
    format_label/2,      % ?Format, -Label
    format_program/2,    % ?Format, ?Program
    media_type_label/2,  % ?MediaType, -Label
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

:- meta_predicate
    process_flags(2, +, -).





%! exists_program(+Program:atom) is semidet.
%
% Succeeds if the given program can be run from PATH.

exists_program(Program) :-
  os_path(Prefix),
  atomic_list_concat([Prefix,Program], /, Exe),
  access_file(Exe, execute), !.



%! format_label(?Format:atom, ?Label:string) is nondet.

format_label(Format, Label) :-
  file_format(Format, _, _, Label).



%! format_media_type(?Format:atom, ?MediaType:compound) is nondet.

format_media_type(Format, MediaType) :-
  file_format(Format, MediaType, _, _).



%! format_program(?Format:atom, ?Programs:list(atom)) is nondet.

format_program(Format, Program) :-
  file_format(Format, _, Programs, _),
  member(Program, Programs).



file_format(bmp, image/bmp, [eog], "Windows Bitmap").
file_format(csv, text/csv, [gedit], "Comma-separated values (CSV)").
file_format(dot, text/'vnd.graphviz', [gedit], "GraphViz DOT").
file_format(gif, image/gif, [eog], "Graphics Interchange Format (GIF)").
file_format(html, text/html, [firefox], "Hyper Text Markup Language (HTML)").
% Microsoft uses Media Type ‘image/x-icon’.
file_format(ico, image/'vnd.microsoft.icon', [eog], "Windows Icon").
file_format(jgf, application/'vnd.jgf+json', [eog], "JSON Graph Format (JGF)").
file_format(jpeg, image/jpeg, [eog], "Joint Photographic Experts Group (JPEG)").
file_format(json, application/json, [], "JavaScript Object Notation (JSON)").
% Native file format of PC Paintbrush.
file_format(pcx, image/'vnd.zbrush.pcx', [eog], "PiCture EXchange").
file_format(pdf, application/pdf, [evince,xpdf], "Portable Document Format (PDF)").
file_format(png, image/png, [eog], "Portable Network Graphics (PNG)").
file_format(pbm, image/'x-portable-bitmap', [], "Portable Bitmap Format (PBM)").
file_format(pgm, image/'x-portable-graymap', [], "Portable Graymap Format (PGM)").
file_format(ppm, image/'x-portable-pixmap', [], "Portable Pixmap Format (PPM)").
file_format(pnm, image/'x-portable-anymap', [eog], "Portable Anymap Format (PNM)").
file_format(ps, application/postscript, [evince,xpdf], "PostScript (PS)").
file_format(ras, _, [eog], "Sun Raster").
file_format(svg, image/'svg+xml', [firefox,eog], "Scalable Vector Graphics (SVG)").
file_format(tga, image/'x-targa', [eog], "Truevision Advanced Raster Graphics Adapter (TARGA)").
file_format(tiff, image/tiff, [eog], "Tagged Image File Format (TIFF)").
file_format(wbmp, image/'vnd.wap.bmp', [eog], "Wireless Application Protocol Bitmap Format (Wireless Bitmap)").
file_format(xbm, image/'x-bitmap', [eog], "X BitMap (XBM)").
file_format(xpm, image/'x-xpixmap', [eog], "X PixMap (XPM)").
file_format(webp, image/webp, [], "WebP").



%! media_type_label(?MediaType:compound, -Label:string) is nondet.

media_type_label(MediaType, Label) :-
  format_media_type(Format, MediaType),
  format_label(Format, Label).



%! open_format(+Format:atom, +File:atom) is det.
%
% Opens the given PDF file.

open_format(Format, File) :-
  format_program(Format, Program),
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
