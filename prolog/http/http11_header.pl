:- meta_predicate('field-content'(3,-,?,?)).
:- meta_predicate('field-value'(3,-,?,?)).



%! 'accept-ranges'(-Ranges:list(or([oneof([bytes]),string])))// .
% ```abnf
% Accept-Ranges = "Accept-Ranges" ":" acceptable-ranges
% ```

'accept-ranges'(L) --> 'acceptable-ranges'(L).



%! age(-Age:nonneg)// is det.
% ```abnf
% Age = "Age" ":" age-value
% ```

age(N) --> 'age-value'(N).



%! allow(-Methods:list(atom))// is det.
% ```abnf
% Allow = "Allow" ":" #Method
% ```

allow(L) --> *#('Method', L).



%! 'cache-control'(?Directives:list(compound))// .
% ```abnf
% Cache-Control = "Cache-Control" ":" 1#cache-directive
% ```

'cache-control'(L) --> +#('cache-directive', L).



%! 'content-disposition'(-Value:compound)// .

'content-disposition'(content_disposition{type: Type, params: Params}) -->
 'content-disposition'(Type, Params).



%! 'content-md5'(-Md5:atom)// is det.
% ```abnf
% Content-MD5 = "Content-MD5" ":" md5-digest
% ```

'content-md5'(Md5) --> 'md5-digest'(Md5).



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



%! 'last-modified'(-DateTime:compound)// .
% ```abnf
% Last-Modified = "Last-Modified" ":" HTTP-date
% ```

'last-modified'(DT) --> 'HTTP-date'(DT).



%! location(?Location:dict)// .
% ```abnf
% Location = "Location" ":" absoluteURI
% ```

location(Uri) --> absoluteURI(Uri).



%! 'message-header'(-Header:pair(string,dict))// .
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
  ?('LWS'),
  ('field-value'(Pred, Value), ! ; "").



%! pragma(?Directives:list)// .
% ```abnf
% Pragma = "Pragma" ":" 1#pragma-directive
% ```

pragma(L) --> '+#'('pragma-directive', L).



%! server(-Server:list(or[dict,string]))// .
% ```abnf
% Server = "Server" ":" 1*( product | comment )
% ```
%
% Sub-product tokens are separated by white space.

server(L) --> +(sep_server_word, L).
sep_server_word(X) --> ?('LWS'), server_word(X).
server_word(D) --> product(D).
server_word(S) --> comment(S).



%! vary// .
% ```abnf
% Vary  = "Vary" ":" ( "*" | 1#field-name )
% ```

vary("*") --> "*".
vary(L)   --> '+#'('field-name', L).



%! 'www-authenticate'(?Challenges:list(dict))// .
% ```abnf
% WWW-Authenticate = "WWW-Authenticate" ":" 1#challenge
% ```

'www-authenticate'(L) --> +#(challenge, L).
