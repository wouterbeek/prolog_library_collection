:- module(
  web_console,
  [
    clear_web/1, % -Markup:list
    console_input//0, % -Markup:list
    deregister_module/1, % +Module:atom
    documentation_web/1, % -Markup:list
    help_web/1, % -Markup:list
    input_ui/1, % -Markup:list
    register_module/1, % +Module:atom
    registered_module/1, % ?Module:atom
    registered_modules/1, % -Modules:list(atom)
    registered_modules_web/1, % -Markup:list
    web_console/4 % +Command:atom
                  % -DTD_Name:atom
                  % -StyleName:atom
                  % -DOM
  ]
).

/** <module> Web console

The Web-based console for PraSem.

@author Wouter Beek
@version 2012/10, 2013/02-2013/03
*/

:- use_module(generic(list_ext)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(server(error_web)).
:- use_module(server(web_message)).
:- use_module(server(wallace)).
:- use_module(standards(standards)).

:- dynamic history/2.

%% registered_module(?Module:atom) is nondet.
% Modules that are currently registered with the web console.
% Only web modules can be sensibly registered, since the web console
% looks for =|_web|=-predicates exclusively.
% Web modules must be registered before their web methods can be accessed
% from the web console.
%
% @param Module The atomic name of a Prolog module.

:- dynamic registered_module/1.

:- html_resource(
  http_www_css('console_input.css'),
  [requires(http_www_css('prasem.css'))]
).



%% clear_web(-Markup:list) is det.
% Clears the output region of the PraSem Web interface.

clear_web([]).

%% command_input// is det.
% The input field for the Web console.

command_input -->
  html(input(
    [maxlength=200, name=web_command, size=62, type=text, value=''])).

%% console_input// is det.
% Returns the markup for the web-based console.
% This can be inserted in (X)HTML web pages.
%
% @param Markup A list of compound terms representing (X)HTML markup.

console_input -->
  {findall(
     Command,
     history(_Time, Command),
     Commands
   ),
   history_length(HistoryLength),
   first(Commands, HistoryLength, History_),
   atomic_list_concat(History_, '\n', History),
   wallace_uri(URI)},
  html([
    div(id(console_input), [
      form([
        action=URI,
        enctype='application/x-www-form-urlencoded',
        method=post
      ], [
        \history(History, HistoryLength),
        br([]),
        \command_input,
        \submit_button,
        \html_requires(http_www_css('console_input.css'))])])]).

%% deregister_module(+Module:atom) is det.
% Deregisters the given module. This means that the =|_web|=-predicates
% of this module will no longer be accessible from the web console.

deregister_module(Module):-
  registered_module(Module),
  !,
  retract(registered_module(Module)).
% Fails silently.
deregister_module(_Module).

%% documentation_web(-Markup:list) is det.
% Opens a client browser for the documentation server (plDoc).

documentation_web([element(p, [], ['Documentation was opened.'])]):-
  doc_browser.

fail_web([element(h1, [], ['False'])]).

help_web([element(ul, [], ModuleItems)]):-
  setoff(
    element(li, [], [
      element(p, [],
        [element(b, [], [Module]), ':', element(ol, [], PredicateItems)])]),
    (
      registered_module(Module),
      module_property(Module, exports(WebPredicates)),
      setoff(
        element(li, [], [Label]),
        (
          member(WebPredicate/WebArity, WebPredicates),
          atom_concat(Predicate, '_web', WebPredicate),
          DisplayArity is WebArity - 1,
          format(atom(Label), '~w/~w', [Predicate, DisplayArity])
        ),
        PredicateItems
      )
    ),
    ModuleItems
  ).

history(History, HistoryLength) -->
  html(textarea([cols=80, name=history, rows=HistoryLength], History)).

history_length(5).

%% input_ui(-Markup:list) is det.
% HTML markup for an input form.

input_ui([
  element(form, [
    action=URI,
    enctype='application/x-www-form-urlencoded',
    method=post
  ], [
    element(textarea,
      [cols=100, name=web_input, rows=40, type=text, value=''],
      ['']),
    element(button,
      [name=submit, type=submit, value='Submit'],
      ['Submit'])])]
):-
  wallace_uri(URI).

markup_mold(DTD_Name/StyleName/DOM, DTD_Name, StyleName, DOM):-
  !.
markup_mold(StyleName/DOM, html, StyleName, DOM):-
  !.
markup_mold(DOM, html, prasem, DOM):-
  !.

%% register_module(+Module:atom) is det.
% Registers the given module for the web console.
% If the module is a web module, i.e. contains =|_web|=-predicates,
% then these can now be accessed from the web console.
%
% @param Module The atomic name of a module.

% The module is already registered, do nothing.
register_module(Module):-
  registered_module(Module),
  !.
% Register the module.
register_module(Module):-
  assert(registered_module(Module)).

registered_module(web_console).

%% registered_modules(-Modules:list(atom)) is det.
% Returns all modules that are currently registered with the web console.
%
% @param Modules A list of atomic names of modules.

registered_modules(Modules):-
  findall(
    Module,
    registered_module(Module),
    Modules
  ).

registered_modules_web(
  [
    element(
      table,
      [border=1, summary='The currently registered modules.'],
      [
        element(caption, [], ['The currently registered modules.'])
      |
        Rows
      ]
    )
  ]
):-
  registered_modules(Modules),
  findall(
    element(tr, [], [element(td, [], [Module])]),
    member(Module, Modules),
    Rows
  ).

%% request_web(+Request:list, -Markup:list) is det.
% Returns a table markup element representing the header of
% the given request.
%
% @param Request A compound term representing an HTTP header.
% @param Markup A compound term encoding an (X)HTML table.

request_web(
  Request,
  [
    element(
      table,
      [border=1, summary='This table shows an HTTP header.'],
      [
        element(caption, [], ['An HTTP header'])
      |
        Rows
      ]
    )
  ]
):-
  findall(
    element(tr, [], [element(td, [], [AName]), element(td, [], [AValue])]),
    (
      member(NameValuePair, Request),
      NameValuePair =.. [Name, Value],
      maplist(term_to_atom, [Name, Value], [AName, AValue])
    ),
    Rows
  ).

submit_button -->
  html(button([name=submit, type=submit, value='Submit'], 'Submit')).

%% web_console(+Command:atom, -DTD_Name:atom, -StyleName:atom, -DOM) is det.
% This returns either the markup that results from the execution of =Command=,
% or it returns the markup for an error messahe that occured.

web_console(Command, DTD_Name, StyleName, DOM):-
  % Catch errors and display their error messages in the Web browser.
  catch_web(web_console0(Command), Markup),
  markup_mold(Markup, DTD_Name, StyleName, DOM).

% Lets see if we can figure out the predicate
% indicated by the command issued via the Web console interface.
web_console0(Command, Markup):-
  atom_to_term(Command, Compound, _Bindings),
  Compound =.. [Predicate | Arguments_],
  atom_concat(Predicate, '_web', Predicate0),
  functor(Compound, Predicate, Arity),
  WebArity is Arity + 1,
  (
    registered_module(Module),
    current_predicate(Module:Predicate0/WebArity)
  ->
    get_time(Time),
    % Assert to the beginning, so running a findall will automatically
    % retrieve the commands in the order in which they were given.
    asserta(history(Time, Command)),
    append(Arguments_, [Markup], Arguments),
    Call =.. [Predicate0 | Arguments],
    (
      call(Module:Call)
    ->
      true
    ;
      fail_web(Markup)
    )
  ;
    throw(
      error(
        existence_error(predicate, Predicate),
        context(
          web_console:web_console/4,
          'Unrecognized predicate entered in Web console.'
        )
      )
    )
  ).

