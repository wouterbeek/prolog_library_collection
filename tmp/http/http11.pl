:- module(
  http11,
  [
    charset//1,         % -Charset:atom
    'content-type'//1,  % -Mime:compound
    'delta-seconds'//1, % -Seconds:nonneg
    'field-name'//1,    % -Name:atom
    'header-field'//1,  % -Header:pair
    http_known/1,       % ?Key:atom
    'media-type'//1,    % -MT:compound
    method//1,          % -Method:atom
    qvalue//1,          % -Val:between(0.0,1.0)
    'rfc850-date'//1    % -DT:compound
  ]
).

/** <module> HTTP 1.1: Hypertext Transfer Protocol (HTTP/1.1)

# Common mistakes

## Access-Control-Allow-Credentials

Value `*' i.o. `true'.

```http
Access-Control-Allow-Credentials: *
```


## ETag

No double quotes (`DQUOTE') surrounding the opaque identifier.

```http
ETag: e90ec0728cc9d1a7dd2c917923275fb9
```


## Expires

Timezone other than `"GMT"'.

```http
Expires: Sat, 26 Dec 2015 010:30:29 UTC
```

The numer zero i.o. a date-time value.

```http
Expires: 0
```


## Last-modified

Timezone other than `"GMT"'.

```http
Last-Modified: Sat, 26 Dec 2015 010:30:29 UTC
```


## Link

The query component of an URI cannot contain unescaped angular brackets.

```http
Link: <http://el.dbpedia.org/data/Linux.rdf>; rel="alternate"; type="application/rdf+xml"; title="Structured Descriptor Document (RDF/XML format)", <http://el.dbpedia.org/data/Linux.n3>; rel="alternate"; type="text/n3"; title="Structured Descriptor Document (N3/Turtle format)", <http://el.dbpedia.org/data/Linux.json>; rel="alternate"; type="application/json"; title="Structured Descriptor Document (RDF/JSON format)", <http://el.dbpedia.org/data/Linux.atom>; rel="alternate"; type="application/atom+xml"; title="OData (Atom+Feed format)", <http://el.dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fel.dbpedia.org&query=DESCRIBE+<http://el.dbpedia.org/resource/Linux>&format=text%2Fcsv>; rel="alternate"; type="text/csv"; title="Structured Descriptor Document (CSV format)", <http://el.dbpedia.org/data/Linux.ntriples>; rel="alternate"; type="text/plain"; title="Structured Descriptor Document (N-Triples format)", <http://el.dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fel.dbpedia.org&query=DESCRIBE+<http://el.dbpedia.org/resource/Linux>&output=application/microdata+json>; rel="alternate"; type="application/microdata+json"; title="Structured Descriptor Document (Microdata/JSON format)", <http://el.dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fel.dbpedia.org&query=DESCRIBE+<http://el.dbpedia.org/resource/Linux>&output=text/html>; rel="alternate"; type="text/html"; title="Structured Descriptor Document (Microdata/HTML format)", <http://el.dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fel.dbpedia.org&query=DESCRIBE+<http://el.dbpedia.org/resource/Linux>&output=application/ld+json>; rel="alternate"; type="application/ld+json"; title="Structured Descriptor Document (JSON-LD format)", <http://el.dbpedia.org/resource/Linux>; rel="http://xmlns.com/foaf/0.1/primaryTopic", <http://el.dbpedia.org/resource/Linux>; rev="describedby", <http://mementoarchive.lanl.gov/dbpedia/timegate/http://el.dbpedia.org/page/Linux>; rel="timegate"
```

## Location

Lowercase hexadecimal digits in percent encoded characters of URIs
(disallowed by RFC 3986).

```http
Location: https://login2.gnoss.com/obtenerCookie.aspx?UQ5IKlMXioK1tiCnK0oh7y%2fPdqTnyAdHPDV0j1Ox5tEdM14pGmhdaSTZhT3hezOjI4lDAwx%2fOghE%2fLJk7Ce%2ff%2ft%2bsC%2bywTcFPSXaYZhh2Wg9q5mWlDWDvLHMReIWmqWzGhnxNpc4DnbUDjewEMV5vfTVzKD%2bx3gMLq9vZvV%2fL8aIAQOcWkSRam0OyOCkc2KV2zUx24WqdUo9oS5od1ILHDmDwYJxq7WwRgFnHP73WfYJuQJNhwolaTkH7%2blbmx7V4K7bF12Van5ArXjz6umEpg%3d%3d
```


## Server

No linear white space (`LWS') between product and comment.

```http
Server: Jetty(8.1.1.v20120215)
```


## Set-Cookie

Omitting the requires space (`SP') between the separator (`;')
and the cookie parameter.

```http
set-cookie: NSC_tfep-83+63+5+220-91=ffffffff516a73d445525d5f4f58455e445a4a423660;path=/;httponly
```


## Via

Via components must consist of two parts ('protocol' and 'by')
that must be separated by white space (`RWS').

```http
Via: mt-s6, fs4
```


## X-Frame-Options

A valid value that appears multiple times.
(RFC 7034 does not allow comma-separated values.)

```http
X-Frame-Options: SAMEORIGIN, SAMEORIGIN
```

---

@author Wouter Beek
@compat RFC 7230
@compat RFC 7231
@compat RFC 7232
@compat RFC 7233
@compat RFC 7234
@compat RFC 7235
@see https://tools.ietf.org/html/rfc7230
@see https://tools.ietf.org/html/rfc7231
@see https://tools.ietf.org/html/rfc7232
@see https://tools.ietf.org/html/rfc7233
@see https://tools.ietf.org/html/rfc7234
@see https://tools.ietf.org/html/rfc7235
@version 2015/11-2017/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1,  % ?C
     'CHAR'//1,   % ?C
     'CR'//0,
     'CRLF'//0,
     'CTL'//0,
     'CTL'//1,    % ?C
     'DIGIT'//1,  % ?Weight:nonneg
     'DIGIT'//2,  % ?Weight:nonneg, ?C
     'DQUOTE'//0,
     'HEXDIG'//1, % ?Weight:nonneg
     'HTAB'//0,
     'HTAB'//1,   % ?C
     'LF'//0,
     'OCTET'//1,  % ?C
     'SP'//0,
     'SP'//1,     % ?C
     'VCHAR'//1   % ?C
   ]).
:- use_module(library(dict)).
:- use_module(library(http/cors)).
:- use_module(library(http/csp2)).
:- use_module(library(http/dcg_http)).
:- use_module(library(http/http_alternative_services)).
:- use_module(library(http/rfc2295)).
:- use_module(library(http/rfc5988)).
:- use_module(library(http/rfc6265)).
:- use_module(library(http/rfc6266)).
:- use_module(library(http/rfc6797)).
:- use_module(library(http/rfc7034)).
:- use_module(library(lists)).
:- use_module(library(ltag/rfc4647), [
     'language-range'//1 % -LRange:list(atom)
   ]).
:- use_module(library(ltag/rfc5646), [
     'Language-Tag'//1 as 'language-tag' % -LTag:list(atom)
   ]).
:- use_module(library(pair_ext)).
:- use_module(library(sgml)).
:- use_module(library(uri/rfc3986), [
     'absolute-URI'//1,     % -Uri:compound
     fragment//1,           % -Frag:atom
     host//1 as 'uri-host', % -Host:compound
     port//1,               % -Port:nonneg
     query//1,              % -Query:atom
     'relative-part'//2,    % -Auth:compound, -Segments:list(atom)
     'URI-reference'//1     % -Uri:compound
   ]).
:- use_module(library(uri_ext)).

:- discontiguous
    http_known_known/1.

http_known_known('access-control-allow-credentials').
http_known_known('access-control-allow-headers').
http_known_known('access-control-allow-methods').
http_known_known('access-control-allow-origin').
http_known_known('access-control-expose-headers').%@tbd
http_known_known('access-control-max-age').
http_known_known('access-control-request-method').
http_known_known('alt-svc').
http_known_known(alternates).
http_known_known('content-disposition').
http_known_known('content-security-policy').
http_known_known('content-security-policy-report-only').
http_known_known(csp).
http_known_known(link).
http_known_known(origin).
http_known_known('set-cookie').
http_known_known('set-cookie2').
http_known_known('strict-transport-security').
http_known_known(tcn).
http_known_known('x-content-type-options').
http_known_known('x-frame-options').
http_known_known('x-powered-by').
http_known_known('x-xss-protection').



%! http_known(+Key) is semidet.
%! http_known(-Key) is multi.

http_known(Key) :-
  var(Key), !,
  http_known0(Key).
http_known(Key) :-
  once(http_known0(Key)).

http_known0(Key) :-
  http_known_known(Key).
% @bug Dynamic/multifile does not work.
http_known0(link).
http_known0(Key) :-
  http_known_unknown(Key).



%! http_known_unknown(+Key) is semidet.

http_known_unknown(ccdeac).
http_known_unknown('cf-ray').
http_known_unknown('context-path').
http_known_unknown('epke-alive').
http_known_unknown('es.index').
http_known_unknown('es.index_uuid').
http_known_unknown('es.resource.id').
http_known_unknown('es.resource.type').
http_known_unknown('fastly-debug-digest').
http_known_unknown('fastly-debug-path').
http_known_unknown('fastly-debug-ttl').
http_known_unknown('fastly-no-shield').
http_known_unknown('fuseki-request-id').
http_known_unknown('host-header').
http_known_unknown(p3p).
http_known_unknown('proxy-connection').
http_known_unknown('public-key-pins').
http_known_unknown('r01-domain-origin').
http_known_unknown(serverhost).
http_known_unknown(servidor).
http_known_unknown('source-age').
http_known_unknown(status).
http_known_unknown(tcn).
http_known_unknown(warning).
http_known_unknown('x-acre-source-url').
http_known_unknown('x-adblock-key').
http_known_unknown('x-amz-id-2').
http_known_unknown('x-amz-request-id').
http_known_unknown('x-app').
http_known_unknown('x-aspnet-version').
http_known_unknown('x-backend').
http_known_unknown('x-cache').
http_known_unknown('x-cache-action').
http_known_unknown('x-cache-age').
http_known_unknown('x-cache-hits').
http_known_unknown('x-cache-info').
http_known_unknown('x-cache-lookup').
http_known_unknown('x-cache-operation').
http_known_unknown('x-cache-rule').
http_known_unknown('x-cacheable').
http_known_unknown('x-cloud-trace-context').
http_known_unknown('x-content-type-options'). % Has grammar; implemented.
http_known_unknown('x-dropbox-http-protocol').
http_known_unknown('x-dropbox-request-id').
http_known_unknown('x-drupal-cache').
http_known_unknown('x-drupal-dynamic-cache').
http_known_unknown('x-ec-custom-error').
http_known_unknown('x-fastly-request-id').
http_known_unknown('x-frame-options').
http_known_unknown('x-generator').
http_known_unknown('x-geo-block-list').
http_known_unknown('x-github-request-id').
http_known_unknown('x-goog-generation').
http_known_unknown('x-goog-hash').
http_known_unknown('x-goog-meta-uploaded-by').
http_known_unknown('x-goog-metageneration').
http_known_unknown('x-goog-storage').
http_known_unknown('x-goog-storage-class').
http_known_unknown('x-goog-stored-content-encoding').
http_known_unknown('x-goog-stored-content-length').
http_known_unknown('x-guploader-uploadid').
http_known_unknown('x-http-host').
http_known_unknown('x-hosted-by').
http_known_unknown('x-metaweb-cost').
http_known_unknown('x-metaweb-tid').
http_known_unknown('x-ms-request-id').
http_known_unknown('x-ms-version').
http_known_unknown('x-origin-route').
http_known_unknown('x-pad').
http_known_unknown('x-page').
http_known_unknown('x-pal-host').
http_known_unknown('x-per-page').
http_known_unknown('x-permitted-cross-domain-policies').
http_known_unknown('x-pingback').
http_known_unknown('x-powered-by').
http_known_unknown('x-productontology-limit').
http_known_unknown('x-productontology-offset').
http_known_unknown('x-productontology-results').
http_known_unknown('x-proxy-cache').
http_known_unknown('x-purl').
http_known_unknown('x-robots-tag'). % Has grammar.  Implemented.
http_known_unknown('x-rack-cache').
http_known_unknown('x-render-time').
http_known_unknown('x-request-count').
http_known_unknown('x-request-id').
http_known_unknown('x-response-id').
http_known_unknown('x-runtime').
http_known_unknown('x-served-by').
http_known_unknown('x-served-from-cache').
http_known_unknown('x-server').
http_known_unknown('x-sparql').
http_known_unknown('x-sparql-default-graph').
http_known_unknown('x-sparql-maxrows').
http_known_unknown('x-static-version').
http_known_unknown('x-timer').
http_known_unknown('x-total').
http_known_unknown('x-total-results').
http_known_unknown('x-ua-compatible').
http_known_unknown('x-uniprot-release').
http_known_unknown('x-varnish').
http_known_unknown('x-varnish-cache').
http_known_unknown('x-varnish-caching-rule-id').
http_known_unknown('x-varnish-header-set-id').
http_known_unknown('x-vcap-request-id').
http_known_unknown('x-version').
http_known_unknown('x-xss-protection'). % Has grammar.  Implemented.
