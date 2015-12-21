:- module(
  google_headers,
  [
    'x-robots-tag'//1 % -Robots:dict
  ]
).

/** <module> Google-specific HTTP headers

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/dcg_http)).
:- use_module(library(http/rfc2616_code)).
:- use_module(library(http/rfc2616_date)).
:- use_module(library(http/rfc2616_token)).





%! 'x-robots-tag'(-Something)// .
% The `X-Robots-Tag' can be used as an element of the HTTP header response for
% a given URL.  Any directive that can [be] used in [a] robots meta tag can
% also be specified as an `X-Robots-Tag'.
%
% Multiple `X-Robots-Tag' headers can be combined within the HTTP response, or
% you can specify a comma-separated list of directives.
%
% The `X-Robots-Tag' may optionally specify a user-agent before the directives.
%
% The following directives are supported:
%   * `all'
%     There are no restrictions for indexing or serving.  Note: this directive
%     is the default value and has no effect if explicitly listed.
%   * `noindex'
%     Do not show this page in search results and do not show a "Cached" link
%     in search results.
%   * `nofollow'
%     Do not follow the links on this page.
%   * `none'
%     Equivalent to `noindex, nofollow'.
%   * `noarchive'
%     Do not show a "Cached" link in search results.
%   * `nosnippet'
%     Do not show a snippet in the search results for this page.
%   * `noodp'
%     Do not use metadata from the Open Directory project for titles or
%     snippets shown for this page.
%   * `notranslate'
%     Do not offer translation of this page in search results.
%   * `noimageindex'
%     Do not index images on this page.
%   * `unavailable_after: [RFC-850 date/time]'
%     Do not show this page in search results after the specified date/time.
%     The date/time must be specified in the RFC 850 format.
%
% @see https://developers.google.com/webmasters/control-crawl-index/docs/robots_meta_tag

'x-robots-tag'(robots{user_agent: UA, directives: L}) -->
  user_agent(UA), ":", !, ?('LWS'), '+#'(directive, L).
'x-robots-tag'(robots{directives: L}) --> '+#'(directive, L).


user_agent(S) --> token(S).


directive(all) --> "all".
directive(noindex) --> "noindex".
directive(nofollow) --> "nofollow".
directive(none) --> "none".
directive(noarchive) --> "noarchive".
directive(nosnippet) --> "nosnippet".
directive(noodp) --> "noodp".
directive(notranslate) --> "notranslate".
directive(noimageindex) --> "noimageindex".
directive(unavailable_after-DT) -->
  "unavailable_after:",
  ?('LWS'),
  'rfc850-date'(DT).
