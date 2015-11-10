:- module(
  access_control,
  [
  ]
).

/** <module> Access control HTTP headers

@author Wouter Beek
@version 2015/11
*/


%! 'Access-Control-Allow-Origin'// .
%! 'Access-Control-Allow-Origin0'// .
% ```abnf
% Access-Control-Allow-Origin = "Access-Control-Allow-Origin"
%                               ":" origin-list-or-null
%                             | "*"
% ```

'Access-Control-Allow-Origin' -->
  "Access-Control-Allow-Origin:",
  'Access-Control-Allow-Origin0'.
'Access-Control-Allow-Origin0' --> 'origin-list-or-null'.
'Access-Control-Allow-Origin0' --> "*".
