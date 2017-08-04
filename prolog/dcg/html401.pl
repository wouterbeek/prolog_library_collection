:- module(
  html401,
  [
    'MediaDesc'//1 % -MediaDescriptions:list(string)
  ]
).

/** <module> HTML 4.01

@author Wouter Beek
@version 2017/05
*/

:- use_module(library(dcg/dcg_ext)).





%! 'MediaDesc'(-MediaDescriptions:list(string))// is det.
%
% The following media descriptions are supported by HTML 4.01:
%
%   * all
%
%     Suitable for all devices.
%
%   * aural
%
%     Intended for speech synthesizers.
%
%   * braille
%
%     Intended for braille tactile feedback devices.
%
%   * handheld
%
%     Intended for handheld devices (small screen, monochrome,
%     bitmapped graphics, limited bandwidth).
%
%   * print
%
%     Intended for paged, opaque material and for documents viewed on
%     screen in print preview mode.
%
%   * projection
%
%     Intended for projectors.
%
%   * screen
%
%     Intended for non-paged computer screens.
%
%   * tty
%
%     Intended for media using a fixed-pitch character grid, such as
%     teletypes, terminals, or portable devices with limited display
%     capabilities.
%
%   * tv
%
%     Intended for television-type devices (low resolution, color,
%     limited scrollability).
%
% To facilitate the introduction of media descriptions in the future,
% conforming UAs must parse as follows:
%
%   1. The value is a comma-separated list of entries [stripping
%      spaces?].
%
%   2. Each entry is truncated just before the first character that is
%      not ALPHA, DIGIT or hyphen.
%
%   3. Case-sensitive match with the above enumerated media
%      descriptions.  UAs may ignore entries that do not match.

'MediaDesc'(L) -->
  *(media_desc_sep),
  ...(Cs),
  media_desc_post, !,
  {
    string_codes(H, Cs),
    (known_media_desc(H) -> L = [H|T] ; L = T)
  },
  'MediaDesc'(T).
'MediaDesc'([]) --> "".

media_desc_sep --> " ".
media_desc_sep --> ",".

media_desc_post --> ",", !.
media_desc_post --> " ", ..., ",".

known_media_desc("all").
known_media_desc("aural").
known_media_desc("braille").
known_media_desc("handheld").
known_media_desc("print").
known_media_desc("projection").
known_media_desc("screen").
known_media_desc("tty").
known_media_desc("tv").
