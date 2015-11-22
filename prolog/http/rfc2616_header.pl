:- module(
  rfc2616_header,
  [
    'Accept-Ranges'//1, % -Ranges:list(or([oneof([bytes]),string]))
    'Access-Control-Allow-Credentials'//1, % -AllowCredentials:boolean
    'Access-Control-Allow-Headers'//1, % ?HeaderNames:list(string)
    'Access-Control-Allow-Origin'//1, % -Origins:list(dict)
    'Cache-Control'//1, % -Directives:list(compound)
    'Connection'//1, % -ConnectionTokens:list(string)
    'Content-Disposition'//1, % -ContentDisposition:dict
    'Content-Language'//1, % -LanguageTags:list(list(string)))
    'Content-Length'//1, % -Length:nonneg
    'Content-Type'//1, % -MediaType:dict
    'Date'//1, % -DateTime:compound
    'Expires'//1, % ?DateTime:compound
    'ETag'//1, % -ETag:dict
    'field-content'//2, % :Name
                        % -Value:compound
    'field-name'//1, % -Name:string
    'field-value'//2, % :Name
                      % -Value:compound
    'message-header'//1, % -Header:pair(string,compound)
    'Last-Modified'//1, % -DateTime:compound
    'Server'//1, % -Value:list([dict,string])
    'Transfer-Encoding'//1 % -TransferEncoding:list(or([oneof([chunked]),dict])))
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
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/rfc2234_re)).
:- use_module(library(debug)).
:- use_module(library(http/rfc2616_code)).
:- use_module(library(http/rfc2616_date)).
:- use_module(library(http/rfc2616_helpers)).
:- use_module(library(http/rfc2616_token)).
:- use_module(library(http/rfc2617)).
:- use_module(library(http/rfc6266)).
:- use_module(library(http/rfc6454)).

:- meta_predicate('field-content'(3,-,?,?)).
:- meta_predicate('field-value'(3,-,?,?)).





%! 'Accept-Ranges'(-Ranges:list(or([oneof([bytes]),string])))// .
% ```abnf
% Accept-Ranges = "Accept-Ranges" ":" acceptable-ranges
% ```

'Accept-Ranges'(L) --> 'acceptable-ranges'(L).



%! 'Access-Control-Allow-Credentials'(?AllowCredentials:boolean)// .
% ```abnf
% Access-Control-Allow-Credentials: "Access-Control-Allow-Credentials" ":" true
% ```

'Access-Control-Allow-Credentials'(true) --> "true".



%! 'Access-Control-Allow-Headers'(?HeaderNames:list(string))// .
% ```abnf
% Access-Control-Allow-Headers: "Access-Control-Allow-Headers" ":" #field-name
% ```

'Access-Control-Allow-Headers'(L) --> *#('field-name', L).



%! 'Access-Control-Allow-Origin'(-Origins:list(dict))// .
% ```abnf
% Access-Control-Allow-Origin = "Access-Control-Allow-Origin"
%                               ":" origin-list-or-null
%                             | "*"
% ```

'Access-Control-Allow-Origin'(L) --> 'origin-list-or-null'(L).
'Access-Control-Allow-Origin'(*) --> "*".



%! 'Cache-Control'(?Directives:list(compound))// .
% ```abnf
% Cache-Control = "Cache-Control" ":" 1#cache-directive
% ```

'Cache-Control'(L) --> +#('cache-directive', L).



%! 'Connection'(-ConnectionTokens:list(string))// .
% ```abnf
% 'Connection'(S) --> "Connection:", 1#(connection-token)
% ```

'Connection'(L) --> +#('connection-token', L).



%! 'Content-Disposition'(-Value:compound)// .

'Content-Disposition'(content_disposition{type: Type, params: Params}) -->
  'content-disposition'(Type, Params).



%! 'Content-Language'(-LanguageTags:list(list(string)))// .
% ```abnf
% Content-Language = "Content-Language" ":" 1#language-tag
% ```

'Content-Language'(L) --> +#('language-tag', L).



%! 'Content-Length'(-Length:nonneg)// .
% ```abnf
% Content-Length = "Content-Length" ":" 1*DIGIT
% ```

'Content-Length'(N) --> '+DIGIT'(N).



%! 'Content-Type'(-MediaType:dict)// .
% ```abnf
% Content-Type = "Content-Type" ":" media-type
% ```

'Content-Type'(MT) --> 'media-type'(MT).




%! 'Date'(-DateTime:compound)// .
% ```
% Date = "Date" ":" HTTP-date
% ```

'Date'(DT) --> 'HTTP-date'(DT).



%! 'ETag'(-ETag:compound)// .
% ```abnf
% ETag = "ETag" ":" entity-tag
% ```

'ETag'(eTag{weak: Weak, 'opaque-tag': OTag}) --> 'entity-tag'(Weak, OTag).



%! 'Expires'(?DateTime:compound)// .
% ```abnf
% Expires = "Expires" ":" HTTP-date
% ```

'Expires'(DT) --> 'HTTP-date'(DT).



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

'field-name'(Name) --> token(Name).



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



%! 'Last-Modified'(-DateTime:compound)// .
% ```abnf
% Last-Modified = "Last-Modified" ":" HTTP-date
% ```

'Last-Modified'(DT) --> 'HTTP-date'(DT).



%! 'Server'(-Server:list(or[dict,string]))// .
% ```abnf
% Server = "Server" ":" 1*( product | comment )
% ```

'Server'(L) --> '+server_comp'(L).
'+server_comp'([H|T]) --> server_comp(H), !, '*server_comp'(T).
% Sub-product tokens are separated by white space.
'*server_comp'([H|T]) --> 'LWS', server_comp(H), !, '*server_comp'(T).
'*server_comp'([])    --> "".
server_comp(D) --> product(D).
server_comp(S) --> comment(S).



%! 'Transfer-Encoding'(-TransferEncoding:list(or([oneof([chunked]),dict])))// .
% ```abnf
% Transfer-Encoding = "Transfer-Encoding" ":" 1#transfer-coding
% ```

'Transfer-Encoding'(L) --> +#('transfer-coding', L).



%! 'WWW-Authenticate'(?Challenges:list(dict))// .
% ```abnf
% WWW-Authenticate = "WWW-Authenticate" ":" 1#challenge
% ```

'WWW-Authenticate'(L) --> +#(challenge, L).
