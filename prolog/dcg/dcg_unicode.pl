:- module(
  dcg_unicode,
  [
    alpha_numeric//0,
    alpha_numeric//1, % ?Code:code
    bracket//0,
    bracket//1, % ?Code:code
    bracket//2, % ?Type:oneof([angular,curly,langular,round,square])
                % ?Code:code
    character_tie//0,
    character_tie//1, % ?Code:code
    closing_bracket//0,
    closing_bracket//1, % ?Code:code
    closing_bracket//2, % ?Type:oneof([angular,curly,langular,round,square])
                        % ?Code:code
    graphic//0,
    graphic//1, % ?Code:code
    letter//0,
    letter//1, % ?Code:code
    letter_lowercase//0,
    letter_lowercase//1, % ?Code:code
    letter_uppercase//0,
    letter_uppercase//1, % ?Code:code
    line_separator//0,
    line_separator//1, % ?Code:code
    line_terminator//0,
    line_terminator//1, % ?Code:code
    middle_dot//0,
    middle_dot//1, % ?Code:code
    models//0,
    models//1, % ?Code:code
    next_line//0,
    next_line//1, % ?Code:code
    opening_bracket//0,
    opening_bracket//1, % ?Code:code
    opening_bracket//2, % ?Type:oneof([angular,curly,langular,round,square])
                        % ?Code:code
    orgham_space_mark//0,
    orgham_space_mark//1, % ?Code:code
    punctuation//0,
    punctuation//1, % ?Code:code
    undertie//0,
    undertie//1, % ?Code:code
    white//0,
    white//1, % ?Code:code
    zero_width_joiner//0,
    zero_width_joiner//1, % ?Code:code
    zero_width_non_joiner//0,
    zero_width_non_joiner//1 % ?Code:code
  ]
).

/** <module> DCG Unicode

DCG rules that encode characters from the UNICODE standard.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(dcg/dcg_ascii)).





%! alpha_numeric// .
%! alpha_numeric(?Code:code)// .
% Notice that there is a string asymmetry between the generative and
% the semidet case here.
%
% @see http://www.swi-prolog.org/pldoc/doc_for?object=char_type/2

alpha_numeric --> alpha_numeric(_).
alpha_numeric(Code) -->
  [Code],
  {code_type(Code, alnum)}.

%! bracket// .
%! bracket(?Code:code)// .
%! bracket(?Type:oneof([angular,curly,langular,round,square]), ?Code:code)// .

bracket --> bracket(_).
bracket(Code) --> bracket(_, Code).
bracket(Type, Code) --> closing_bracket(Type, Code).
bracket(Type, Code) --> opening_bracket(Type, Code).

%! character_tie// .
%! character_tie(?Code:code)// .

character_tie --> character_tie(_).
character_tie(8256) --> [8256].

%! closing_bracket// .
%! closing_bracket(?Code:code)// .
%! closing_bracket(
%!   ?Type:oneof([angular,curly,langular,round,square]),
%!   ?Code:code
%! )// .

closing_bracket --> closing_bracket(_).
closing_bracket(Code) --> closing_bracket(_, Code).
closing_bracket(Type, Code) --> ascii_closing_bracket(Type, Code).
closing_bracket(langular, 12297) --> [12297].

%! graphic// .
%! graphic(?Code:code)// .

graphic --> graphic(_).
graphic(Code) -->
  [Code],
  {code_type(Code, graph)}.

%! letter// .
%! letter(?Code:code)// .

letter --> letter(_).
letter(Code) -->
  [Code],
  {code_type(Code, alpha)}.

%! letter_lowercase// .
%! letter_lowercase(?Code:code)// .

letter_lowercase --> letter_lowercase(_).
letter_lowercase(Code) -->
  [Code],
  {code_type(Code, lower)}.

%! letter_uppercase// .
%! letter_uppercase(?Code:code)// .

letter_uppercase --> letter_uppercase(_).
letter_uppercase(Code) -->
  [Code],
  {code_type(Code, upper)}.

%! line_separator// .
%! line_separator(?Code:code)// .

line_separator --> line_separator(_).
line_separator(8232) --> [8232].

%! line_terminator// .
%! line_terminator(?Code:code)// .

line_terminator --> line_terminator(_).
line_terminator(Code) --> ascii_line_terminator(Code).
line_terminator(Code) --> next_line(Code).
line_terminator(Code) --> line_separator(Code).
line_terminator(Code) --> paragraph_separator(Code).

%! middle_dot// .
%! middle_dot(?Code:code)// .

middle_dot --> middle_dot(_).
middle_dot(183) --> [183].

%! models// .
%! models(?Code:code)// .

models --> models(_).
models(8871) --> [8871].

%! next_line// .
%! next_line(?Code:code)// .

next_line --> next_line(_).
next_line(133) --> [133].

%! nonbreaking_space// .
%! nonbreaking_space(?Code:code)// .

nonbreaking_space --> nonbreaking_space(_).
nonbreaking_space(160) --> [160].

%! opening_bracket// .
%! opening_bracket(?Code:code)// .
%! opening_bracket(
%!   ?Type:oneof([angular,curly,langular,round,square]),
%!   ?Code:code
%! )// .

opening_bracket --> opening_bracket(_).
opening_bracket(Code) --> opening_bracket(_, Code).
opening_bracket(Type, Code) --> ascii_opening_bracket(Type, Code).
opening_bracket(langular, 12296) --> [12296].

%! orgham_space_mark// .
%! orgham_space_mark(?Code:code)// .

orgham_space_mark --> orgham_space_mark(_).
orgham_space_mark(5760) --> [5760].

%! paragraph_separator// .
%! paragraph_separator(?Code:code)// .

paragraph_separator --> paragraph_separator(_).
paragraph_separator(8233) --> [8233].

%! punctuation// .
%! punctuation(?Code:code)// .

punctuation --> punctuation(_).
punctuation(Code) -->
  [Code],
  {code_type(Code, punct)}.

%! undertie// .
%! undertie(?Code:code)// .

undertie --> undertie(_).
undertie(8255) --> [8255].

%! white// .
%! white(?Code:code)// .
% @compat http://en.wikipedia.org/wiki/Whitespace_character
% @tbd Enter the rest of the table.

white --> white(_).
white(Code) --> ascii_white(Code).
white(Code) --> next_line(Code).
white(Code) --> nonbreaking_space(Code).
white(Code) --> orgham_space_mark(Code).
% ...

%! zero_width_joiner// .
%! zero_width_joiner(?Code:code)// .

zero_width_joiner --> zero_width_joiner(_).
zero_width_joiner(8203) --> [8203].

%! zero_width_non_joiner// .
%! zero_width_non_joiner(?Code:code)// .

zero_width_non_joiner --> zero_width_non_joiner(_).
zero_width_non_joiner(8204) --> [8204].
