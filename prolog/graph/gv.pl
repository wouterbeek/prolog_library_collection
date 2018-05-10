%! gv_reply(+Method:atom, +MediaType:compound, :Goal_1) is det.

gv_reply(Method, MediaType, Goal_1) :-
  atom_phrase(media_type(MediaType), Atom),
  format("Content-Type: ~a\n\n", [Atom]),
  media_type_extension(MediaType, Format),
  assertion(gv_extension(Format)),
  gv_open(Method, Format, ProcIn, ProcOut),
  call(Goal_1, ProcIn),
  close(ProcIn),
  copy_stream_data(ProcOut, current_output),
  close(ProcOut).



%! is_gv_media_type(+MediaType:compound) is semidet.

is_gv_media_type(MediaType) :-
  media_type_extension(MediaType, Format),
  gv_format(Format).
