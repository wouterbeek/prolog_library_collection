:- module(http_generic, []).

/** <module> HTTP generic

@author Wouter Beek
@version 2017/08
*/

:- multifile
    uri:default_port/2.

uri:default_port(http, 80).
uri:default_port(https, 443).
