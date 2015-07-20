:- module(
  audio_ext,
  [
    lines_to_mp3/2 % +Lines:list(atom)
                   % +File:atom
  ]
).

/** <module> Audio extensions

# TTS conversion

The limited but free TTS feature of Google translate is used.

In the furture services like Google translate
 and Microfost translate may be added.

@author Wouter Beek
@tbd Catch Unicode characters.
@tbd Do not fail on empty line.
@version 2012/10, 2014/01-2014/02, 2014/05, 2015/02
*/

:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(http/http_open)).
:- use_module(library(uri)).

:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_replace)).
:- use_module(plc(generics/atom_ext)).
:- use_module(plc(generics/db_ext)).
:- use_module(plc(io/file_ext)).

:- db_add_novel(user:prolog_file_type(mp3, audio)).





%! lines_to_mp3(+Lines:list(atom), +File:atom) is det.
% Creates an MP3 file with the given line's text in spoken form.
% If no file name is given, one is generated using `Line`'s MD5 hash.
%
% The lines must be sufficiently small (exact upper limit?).
%
% The lines must not contain non-ASCII characters (?).
%
% @arg Line An atom, preferable a natural language expression.
% @arg File The atomic name of an MP3 file.

lines_to_mp3(Lines, File):-
  setup_call_cleanup(
    open(File, write, Stream, [type(binary)]),
    maplist(line_to_mp3(Stream), Lines),
    close(Stream)
  ).
line_to_mp3(Stream, Line):-
  google_tts(Line, URI),
  http_open(URI, Temp, []),
  copy_stream_data(Temp, Stream).

google_tts(Line, URI):-
  google_tts('UTF-8', en, Line, URI).

%! google_tts(
%!   +Encoding:atom,
%!   +Language:atom,
%!   +Line:atom,
%!   -URI:atom
%! ) is det.
% Returns the URI for the request to Google for returning
%  the verbalization of `Line`.

google_tts(Encoding, Language, Line, Uri):-
  % Replace spaces by plus signs.
  atom_phrase(dcg_replace([32], [43]), Line, NormalizedQuery),
  uri_query_components(Search, [ie=Encoding,tl=Language,q=NormalizedQuery]),
  uri_components(
    Uri,
    uri_components(http,'translate.google.com','/translate_tts',Search,'')
  ).





% TEST %

test('One art', 'The art of losing isn\'t hard to master;').
test('One art', 'so many things seem filled with the intent').
test('One art', 'to be lost that their loss is no disaster.').

test('One art', 'Lose something every day. Accept the fluster').
test('One art', 'of lost door keys, the hour badly spent.').
test('One art', 'The art of losing isn\'t hard to master.').

test('One art', 'Then practice losing farther, losing faster:').
test('One art', 'places, and names, and where it was you meant').
test('One art', 'to travel. None of these will bring disaster.').

test('One art', 'I lost my mother\'s watch. And look! my last, or').
test('One art', 'next-to-last, of three loved houses went.').
test('One art', 'The art of losing isn\'t hard to master.').

test('One art', 'I lost two cities, lovely ones. And, vaster,').
test('One art', 'some realms I owned, two rivers, a continent.').
test('One art', 'I miss them, but it wasn\'t a disaster.').

test('One art', 'Even losing you (the joking voice, a gesture').
test('One art', 'I love) I shan\'t have lied.  It\'s evident').
test('One art', 'the art of losing\'s not too hard to master').
test('One art', 'though it may look like (Write it!) like disaster.').

test('Biden', 'I have two shotguns at my home.').
test('Biden', 'They\'re locked in a safe.').
test('Biden', 'There\'s a metal gun case.').
test('Biden', 'We live in an area that\'s wooded, somewhat secluded.').
test('Biden', 'Somewhat secluded.').
test('Biden', 'And I\'ve said Jill, if there\'s ever a problem, just walk out').
test('Biden', 'on the balcony and fire two blasts outside the house.').
test('Biden', 'Buy a shotgun, buy a shotgun...').
test('Biden', 'Buy a shotgun, buy a shotgun...').
test('Biden', 'We don\'t need a machine gun, we don\'t need 30 rounds').
test('Biden', 'buy a shotgun, buy a double-barreled shotgun.').

test:-
  % Collect the lines to verbalize.
  Poem = 'One art',
  %Poem = 'Biden',
  findall(Line, test(Poem, Line), Lines),
  
  % Create a test file.
  new_atom(test, Base),
  create_file(data(.), Base, mp3, MP3),
  
  % Do it.
  lines_to_mp3(Lines, MP3).

