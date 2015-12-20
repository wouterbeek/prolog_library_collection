:- module(
  ie_headers,
  [
    'x-content-type-options'//1, % -Status:oneof([nosniff])
    'x-powered-by'//1, % -Value:string
    'x-xss-protection'//1 % -Status:oneof([block,optin,optout])
  ]
).

/** <module> Internet Exporer-specific HTTP headers

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/rfc2616_code)).
:- use_module(library(http/rfc2616_token)).





%! 'x-content-type-options'(-Status:oneof([nosniff]))// .
% Sites hosting untrusted content can use the `X-Content-Type-Options: nosniff'
% header to ensure that text/plain files are not sniffed to anything else.
%
% @see https://msdn.microsoft.com/library/gg622941%28v=vs.85%29.aspx

'x-content-type-options'(nosniff) --> "nosniff".



%! 'x-powered-by'(-Value:string)// .
% `X-Powered-By' specifies the technology (e.g. ASP.NET, PHP, JBoss) supporting
% the Web application (version details are often in `X-Runtime', `X-Version',
% or `X-AspNet-Version').
%
% # Examples
%
% ```http
% X-Powered-By: ASP.NET
% X-Powered-By: PHP/5.4.0
% ```

'x-powered-by'(S) --> token(S).
  



%! 'x-xss-protection'(-Status:oneof([block,optin,optout]))// ,
% Internet Explorer 8 included a novel new feature to help prevent reflected
% cross-site scripting attacks, known as the XSS Filter.  This filter runs by
% default in the Internet, Trusted, and Restricted security zones.  Local
% Intranet zone pages may opt-in to the protection using the same header:
%
% ```http
% X-XSS-Protection: 1
% ```
%
% If a cross-site scripting attack is detected, Internet Explorer 8 and 9
% will attempt to make the smallest possible modification to the returned
% web page in order to block the attack.  In most cases, the modification is to
% change one or more characters in the returned page into the hash character
% (“#”) breaking any script that may have been reflected from the outbound HTTP
% request.
%
% Pages that have been secured against XSS via server-side logic may opt-out of
% this protection using a HTTP response header:
%
% ```http
% X-XSS-Protection: 0
% ```
%
% In March of 2010, we added to IE8 support for a new token in the
% X-XSS-Protection header, `mode=block'.
%
% ```http
% X-XSS-Protection: 1; mode=block
% ```
%
% When this token is present, if a potential XSS Reflection attack is detected,
% Internet Explorer will prevent rendering of the page.  Instead of attempting
% to sanitize the page to surgically remove the XSS attack, IE will render only
% “#”.
%
% @see http://blogs.msdn.com/b/ieinternals/archive/2011/01/31/controlling-the-internet-explorer-xss-filter-with-the-x-xss-protection-http-header.aspx

'x-xss-protection'(optout) --> "0", !.
'x-xss-protection'(block) --> "1;", !, ?('LWS'), "mode=block".
'x-xss-protection'(optin) --> "1".
