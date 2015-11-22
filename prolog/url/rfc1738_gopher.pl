:- module(
  rfc1738_gopher,
  [
    'gopher+_string'//1, % ?String:string
    gopherurl//6, % ?Host
                  % ?Port
                  % ?Type
                  % ?Selector
                  % ?Search
                  % ?String:string
    gtype//1, % ?Type:string
    selector//1 % ?Selector:string
  ]
).

/** <module> RFC 1738: Gopher protocol

@author Wouter Beek
@compat RFC 1738
@deprecated
@version 2015/11
*/

:- use_module(library(dcg/dcg_re)).
:- use_module(library(url/rfc1738_code)).
:- use_module(library(url/rfc1738_component)).





%! 'gopher+_string'(?String:string) //
% ```abnf
% gopher+_string = *xchar
% ```

'gopher+_string'(S) --> *(xchar, Cs), {string_codes(S, Cs)}.



%! gopherurl(
%!   ?Host,
%!   ?Port,
%!   ?Type:string,
%!   ?Selector:string,
%!   ?Search,
%!   ?String:string
%! )// .
% ```abnf
% gopherurl = "gopher://" hostport [ / [ gtype [ selector
%             [ "%09" search [ "%09" gopher+_string ] ] ] ] ]
% ```

gopherurl(Host, Port, Type, Selector, Search, String) -->
  "gopher://",
  hostport(Host, Port),
  (   "/"
  ->  (   gtype(Type)
      ->  (   selector(Selector)
          ->  (   "%09"
              ->  search(Search),
	          (   "%09"
                  ->  'gopher+_string'(String)
                  ;   ""
                  )
              ;   ""
              )
          ;   ""
          )
      ;   ""
      )
  ;   ""
  ).



%! gtype(?Type:string)// .
% ```abnf
% gtype = xchar
% ```

gtype(S) --> xchar(C), {string_codes(S, [C])}.



%! selector(?Selector:string)// .
% ```abnf
% selector = *xchar
% ```

selector(S) --> *(xchar, Cs), {string_codes(S, Cs)}.
