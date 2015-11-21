:- module(
  rfc2616_header,
  [
    'Access-Control-Allow-Origin'//1, % ?Origins:list(dict)
    'Connection'//1, % ?ConnectionTokens:list(string)
    'Content-Disposition'//1, % ?Value:dict
    'Content-Language'//1, % ?LanguageTags:list(list(string)))
    'Content-Type'//1, % ?MediaType:dict
    'Date'//1, % ?Value:compound
    'field-content'//2, % :Name
                        % -Value:compound
    'field-name'//1, % ?Name:string
    'field-value'//2, % :Name
                      % -Value:compound
    'message-header'//1, % ?Header:pair(string,compound)
    'Server'//1, % ?Value:list([dict,string])
    'Transfer-Encoding'//1 % ?TransferEncoding:list(or([oneof([chunked]),dict])))
  ]
).

/** <module> RFC 2616: Headers

@author Wouter Beek
@compat RFC 2616
@deprecated
@version 2015/11
*/

:- use_module(library(dcg/dcg_call)).
:- use_module(library(http/rfc2616_code)).
:- use_module(library(http/rfc2616_date)).
:- use_module(library(http/rfc2616_token)).
:- use_module(library(http/rfc6266)).
:- use_module(library(http/rfc6454)).

:- meta_predicate(abnf_list(3,-,?,?)).
:- meta_predicate('field-content'(3,-,?,?)).
:- meta_predicate('field-value'(3,-,?,?)).





%! 'Access-Control-Allow-Origin'(?Origins:list(dict))// .
% ```abnf
% Access-Control-Allow-Origin = "Access-Control-Allow-Origin"
%                               ":" origin-list-or-null
%                             | "*"
% ```

'Access-Control-Allow-Origin'(L) --> 'origin-list-or-null'(L).
'Access-Control-Allow-Origin'(*) --> "*".



%! 'Connection'(?ConnectionTokens:list(string))// .
% ```abnf
% 'Connection'(S) --> "Connection:", +(connection-token)
% ```

'Connection'(L) --> abnf_list('connection-token', L).



%! 'connection-token'(?ConnectionToken:string)// .
% ```abnf
% connection-token = token
% ```

'connection-token'(T) --> token(T).



%! 'Content-Disposition'(?Value:compound)// .

'Content-Disposition'(content_disposition{type: Type, params: Params}) -->
  'content-disposition'(Type, Params).



%! 'Content-Language'(?LanguageTags:list(list(string)))// .
% ```abnf
% Content-Language = "Content-Language" ":" 1#language-tag
% ```

'Content-Language'(L) --> abnf_list('language-tag', L).



%! 'Content-Type'(?MediaType:dict)// .
% ```abnf
% Content-Type = "Content-Type" ":" media-type
% ```

'Content-Type'(MT) --> 'media-type'(MT).




%! 'Date'(?DateTime:compound)// .
% ```
% Date = "Date" ":" HTTP-date
% ```

'Date'(DT) --> 'HTTP-date'(DT).



%! 'field-content'(+Name:atom, ?Value:compound)// .
% ```abnf
% field-content = <the OCTETs making up the field-value
%                  and consisting of either *TEXT or combinations
%                  of token, separators, and quoted-string>
% ```

'field-content'(Pred, Value) -->
  dcg_call(Pred, Value).



%! 'field-name'(?Name:string)// .
% ```abnf
% field-name = token
% ```

'field-name'(Name) --> token(Name).



%! 'field-value'(+Name:atom, ?Value:compound)// .
% ```abnf
% field-value = *( field-content | LWS )
% ```

'field-value'(Name, Value) --> 'field-content'(Name, Value).



%! 'message-header'(?Header:pair(string,compound))// .
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



%! 'Server'(?Server:list(or[dict,string]))// .
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



%! 'Transfer-Encoding'(?TransferEncoding:list(or([oneof([chunked]),dict])))// .
% ```abnf
% Transfer-Encoding = "Transfer-Encoding" ":" 1#transfer-coding
% ```

'Transfer-Encoding'(L) --> abnf_list('transfer-coding', L).




% HELPERS %

abnf_list(Dcg_1, [H|T]) -->
  dcg_call(Dcg_1, H), !,
  abnf_list_sep,
  abnf_list(Dcg_1, T).
abnf_list(_, [])        --> "".
abnf_list_sep --> 'LWS', !, abnf_list_sep.
abnf_list_sep --> ",",   !, abnf_list_sep.
abnf_list_sep --> "".
