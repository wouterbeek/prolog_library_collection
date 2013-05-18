:- module(
  audio_ext,
  [
    atom_to_mp3/2, % +Line:atom
                   % ?File:atom
    atom_to_mp3/3 % +Line:atom
                  % +Directory:atom
                  % -File:atom
  ]
).

/** <module> Audio extensions

# TTS conversion

Currently, the limited but free TTS feature of Google translate is used.

In the furture services like Google translate and Microfost translate may be
added.

# Sample text

The following sample text can be used to test predicates in this module.

Like a wandering route and a hasty diary
I lost my mind and sold my soul
That's something only an idiot would do
But if I told you all the dirty things he's always whispering in my ear in bed
I can't think of any better motivation than
Staring at his naked body stretched out while he sleeps
He once consumed every crevice of my living life
I craved the beast in him that night
My heart pounding and my blood rushing
His muscles flexed, sweat pouring out, power unleashed
I could feel his gaze burn into my flesh
He and I made it our duty to screw as often as possible
A dive, pursuing the rhythm with increased concentration
Before turning away with an unnoticed sigh—
My soul was feeble when his back was turned
And my bowels moved for him a sigh
As a sieve of the night in a low dive
Bad to the bone but fine as wine

@author Wouter Beek
@tbd Catch Unicode characters.
@tbd Do not fail on empty line.
@version 2012/10
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(file_ext)).
:- use_module(library(http/http_open)).
:- use_module(library(uri)).



%! atom_to_mp3(+Line:atom, +MP3:atom) is det.
%! atom_to_mp3(+Line:atom, -MP3:atom) is det.
% Creates an MP3 file with the given line's text in spoken form.
% If no file name is given, then one is generated using =Line='s MD5 hash.
%
% @arg Line An atom, preferable a natural language expression.
% @arg MP3 The atomic name of an MP3 file.
% @tbd Currently only works for English.

atom_to_mp3(Line, MP3):-
  var(MP3),
  !,
  atom_to_mp3(Line, audio(.), MP3).
atom_to_mp3(Line, MP3):-
  open(MP3, write, Write, [type(binary)]),
  google_tts(Line, URI),
  %%%%microsoft_translate(Line, URI),
  http_open(URI, Temp, []),
  copy_stream_data(Temp, Write),
  close(Write).

atom_to_mp3(Line, Directory, MP3):-
  %%%%sha_hash(Line, Hash, [algorithm(sha1), encoding(utf8)]),
  %%%%hash_atom(Hash, AtomicHash),
  uri_normalized(Line, FileName),

  create_file(Directory, FileName, mp3, MP3),
  atom_to_mp3(Line, MP3).

google_tts(Query, URI):-
  google_tts(en, Query, URI).

google_tts(Language, Query, URI):-
  google_tts('UTF-8', Language, Query, URI).

%! google_tts(
%!   +Encoding:atom,
%!   +Language:atom,
%!   +Query:atom,
%!   -URI:atom
%! ) is det.
% Returns the URI for the request to Google for returning the speech
% variant of =Query=.

google_tts(_Encoding, _Language, '', _URI):-
  !,
  fail.
google_tts(Encoding, Language, Query, URI):-
%%%!  % Replace spaces with '%20'-s.
%%%!  % But Google translate uses plusses.
%%%!  uri_normalized(Query, NormalizedQuery),
  % Replace spaces with '+'-es.
  atom_replace(Query, [' '-'+'], NormalizedQuery),

  % Create the search components for the URI.
%%%!  % The swipl builtin uri_query_components/3 turns spaces and
%%%!  % plusses into their symbolic equivalents.
%%%!  uri_query_components(Search, [ie=Encoding, tl=Language, q=NormalizedQuery]),
  format(
    atom(Search),
    'ie=~w&tl=~w&q=~w',
    [Encoding, Language, NormalizedQuery]
  ),

  % Create the URI.
  uri_components(
    URI,
    uri_components(http, 'translate.google.com', '/translate_tts', Search, '')
  ).

/*
google_api_key('AIzaSyAw_5wsbe_42MlkmrBskRJmifozh9iGIY4').

google_translate(Text, URI):-
  google_translate(en, de, Text, URI).

google_translate(From, To, Text, URI):-
  google_api_key(API_Key),
  google_translate_scheme(Scheme),
  google_translate_authority(Authority),
  google_translate_path(Path),
  uri_query_components(Search, [key=API_Key, q=Text, source=From, target=To]),
  uri_components(URI, uri_components(Scheme, Authority, Path, Search, '')).

% https://www.googleapis.com/language/translate/v2?key=AIzaSyAw_5wsbe_42MlkmrBskRJmifozh9iGIY4&q=hello&source=en&target=ar

google_translate_scheme(https).

google_translate_authority('www.googleapis.com').

google_translate_path('/language/translate/v2').

load:-
  assert(user:file_search_path(audio, data(audio))),
  absolute_file_name(data(audio), AudioDirectory),
  create_directory(AudioDirectory).

microsoft_translate(From, To, Query, URI):-
  microsoft_app_id(AppID),
  % Replace spaces with '+'-es.
  atom_replace(Query, [' '-'+'], NormalizedQuery),
  format(
    atom(Search),
    'oncomplete=doneCallback&appId=~w&from=~w&to=~w&text=~w',
    [AppID, From, To, NormalizedQuery]
  ),
  uri_components(
    URI,
    uri_components(
      http,
      'api.microsofttranslator.com',
      '/V2/Ajax.svc/Translate',
      Search,
      ''
    )
  ).

microsoft_app_id('0vTrcQBoNnyZ0wd3Zw3ZAPy6hDLS2O+VPfjzGlvAjqw=').
*/
