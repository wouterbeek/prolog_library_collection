:- module(
  rfc2616_header,
  [
    'accept-ranges'//1, % -Ranges:list(or([oneof([bytes]),string]))
    'access-control-allow-credentials'//1, % -AllowCredentials:boolean
    'access-control-allow-headers'//1, % ?HeaderNames:list(string)
    'access-control-allow-origin'//1, % -Origins:list(dict)
    'cache-control'//1, % -Directives:list(compound)
    connection//1, % -ConnectionTokens:list(string)
    'content-disposition'//1, % -ContentDisposition:dict
    'content-language'//1, % -LanguageTags:list(list(string)))
    'content-length'//1, % -Length:nonneg
    'content-type'//1, % -MediaType:dict
    date//1, % -DateTime:compound
    expires//1, % ?DateTime:compound
    etag//1, % -ETag:dict
    'field-content'//2, % :Name
                        % -Value:compound
    'field-name'//1, % -Name:string
    'field-value'//2, % :Name
                      % -Value:compound
    'message-header'//1, % -Header:pair(string,compound)
    'last-modified'//1, % -DateTime:compound
    server//1, % -Value:list([dict,string])
    'transfer-encoding'//1 % -TransferEncoding:list(or([oneof([chunked]),dict])))
  ]
).

/** <module> RFC 2616: Headers

@author Wouter Beek
@compat RFC 2616
@compat Cross-Origin Resource Sharing (W3C Recommendation)
@deprecated
@see http://www.w3.org/TR/cors/
@version 2015/11
*/

:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(http/dcg_http)).
:- use_module(library(http/rfc2616_code)).
:- use_module(library(http/rfc2616_date)).
:- use_module(library(http/rfc2616_token)).
:- use_module(library(http/rfc2617)).
:- use_module(library(http/rfc6266)).
:- use_module(library(http/rfc6454)).

:- meta_predicate('field-content'(3,-,?,?)).
:- meta_predicate('field-value'(3,-,?,?)).





%! 'accept-ranges'(-Ranges:list(or([oneof([bytes]),string])))// .
% ```abnf
% Accept-Ranges = "Accept-Ranges" ":" acceptable-ranges
% ```

'accept-ranges'(L) --> 'acceptable-ranges'(L).



%! 'access-control-allow-credentials'(?AllowCredentials:boolean)// .
% ```abnf
% Access-Control-Allow-Credentials: "Access-Control-Allow-Credentials" ":" true
% ```

'access-control-allow-credentials'(true) --> "true".



%! 'access-control-allow-headers'(?HeaderNames:list(string))// .
% ```abnf
% Access-Control-Allow-Headers: "Access-Control-Allow-Headers" ":" #field-name
% ```

'access-control-allow-headers'(L) --> *#('field-name', L).



%! 'access-control-allow-origin'(-Origins:list(dict))// .
% ```abnf
% Access-Control-Allow-Origin = "Access-Control-Allow-Origin"
%                               ":" origin-list-or-null
%                             | "*"
% ```

'access-control-allow-origin'(L) --> 'origin-list-or-null'(L).
'access-control-allow-origin'(*) --> "*".



%! 'cache-control'(?Directives:list(compound))// .
% ```abnf
% Cache-Control = "Cache-Control" ":" 1#cache-directive
% ```

'cache-control'(L) --> +#('cache-directive', L).



%! 'connection'(-ConnectionTokens:list(string))// .
% ```abnf
% 'Connection'(S) --> "Connection:", 1#(connection-token)
% ```

'connection'(L) --> +#('connection-token', L).



%! 'content-disposition'(-Value:compound)// .

'content-disposition'(content_disposition{type: Type, params: Params}) -->
 'content-disposition'(Type, Params).



%! 'content-language'(-LanguageTags:list(list(string)))// .
% ```abnf
% Content-Language = "Content-Language" ":" 1#language-tag
% ```

'content-language'(L) --> +#('language-tag', L).



%! 'content-length'(-Length:nonneg)// .
% ```abnf
% Content-Length = "Content-Length" ":" 1*DIGIT
% ```

'content-length'(N) --> '+digit'(N).



%! 'content-type'(-MediaType:dict)// .
% ```abnf
% Content-Type = "Content-Type" ":" media-type
% ```

'content-type'(MT) --> 'media-type'(MT).




%! date(-DateTime:compound)// .
% ```
% Date = "Date" ":" HTTP-date
% ```

date(DT) --> 'HTTP-date'(DT).



%! etag(-ETag:compound)// .
% ```abnf
% ETag = "ETag" ":" entity-tag
% ```

etag(eTag{weak: Weak, 'opaque-tag': OTag}) --> 'entity-tag'(Weak, OTag).



%! expires(?DateTime:compound)// .
% ```abnf
% Expires = "Expires" ":" HTTP-date
% ```

expires(DT) --> 'HTTP-date'(DT).



%! 'field-content'(+Name:atom, -Value:compound)// .
% ```abnf
% field-content = <the OCTETs making up the field-value
%                  and consisting of either *TEXT or combinations
%                  of token, separators, and quoted-string>
% ```

'field-content'(Pred, Value) --> dcg_call(Pred, Value), !.
'field-content'(Pred, Value) -->
  dcg_rest(Cs),
  {
    string_codes(Value, Cs),
    debug(http(parse), "Cannot parse HTTP header ~a: ~s", [Pred,Value])
  }.



%! 'field-name'(-Name:string)// .
% ```abnf
% field-name = token
% ```

'field-name'(LowerName) --> token(Name), {string_lower(Name, LowerName)}.



%! 'field-value'(+Name:atom, -Value:compound)// .
% ```abnf
% field-value = *( field-content | LWS )
% ```

'field-value'(Name, Value) --> 'field-content'(Name, Value).



%! 'message-header'(-Header:pair(string,compound))// .
% ```abnf
% message-header = field-name ":" [ field-value ]
% ```

'message-header'(Pred-Value) -->
  'field-name'(Name),
  {atom_string(Pred, Name)},
  ":",
  % The field-content does not include any leading or trailing LWS:
  % linear white space occurring before the first non-whitespace
  % character of the field-value or after the last non-whitespace
  % character of the field-value. Such leading or trailing LWS MAY be
  % removed without changing the semantics of the field value.
  'LWS',
  ('field-value'(Pred, Value), ! ; "").



%! 'last-modified'(-DateTime:compound)// .
% ```abnf
% Last-Modified = "Last-Modified" ":" HTTP-date
% ```

'last-modified'(DT) --> 'HTTP-date'(DT).



%! server(-Server:list(or[dict,string]))// .
% ```abnf
% Server = "Server" ":" 1*( product | comment )
% ```
%
% Sub-product tokens are separated by white space.

server([H|T]) --> server0(H), *(sep_server0, T).
server0(D) --> product(D).
server0(S) --> comment(S).
sep_server0(X) --> 'LWS', server0(X).



%! 'transfer-encoding'(-TransferEncoding:list(or([oneof([chunked]),dict])))// .
% ```abnf
% Transfer-Encoding = "Transfer-Encoding" ":" 1#transfer-coding
% ```

'transfer-encoding'(L) --> +#('transfer-coding', L).



%! 'www-authenticate'(?Challenges:list(dict))// .
% ```abnf
% WWW-Authenticate = "WWW-Authenticate" ":" 1#challenge
% ```

'www-authenticate'(L) --> +#(challenge, L).
