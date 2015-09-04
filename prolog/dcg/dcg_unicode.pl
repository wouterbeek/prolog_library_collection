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
    entails//0,
    entails//1, % ?Code:code
    equivalence//0,
    equivalence//1, % ?Code:code
    falsum//0,
    falsum//1, % ?Code:code
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
    nonbreaking_space//0,
    nonbreaking_space//1, % ?Code:code
    opening_bracket//0,
    opening_bracket//1, % ?Code:code
    opening_bracket//2, % ?Type:oneof([angular,curly,langular,round,square])
                        % ?Code:code
    orgham_space_mark//0,
    orgham_space_mark//1, % ?Code:code
    provable//0,
    provable//1, % ?Code:code
    punctuation//0,
    punctuation//1, % ?Code:code
    set_membership//0,
    set_membership//1, % ?Code:code
    subclass//0,
    subclass//1, % ?Code:code
    undertie//0,
    undertie//1, % ?Code:code
    u_white//0,
    u_white//1, % ?Code:code
    zero_width_joiner//0,
    zero_width_joiner//1, % ?Code:code
    zero_width_non_joiner//0,
    zero_width_non_joiner//1 % ?Code:code
  ]
).
:- reexport(library(dcg/dcg_ascii)).

/** <module> DCG Unicode

DCG rules that encode characters from the UNICODE standard.

@author Wouter Beek
@version 2015/07-2015/09
*/





%! alpha_numeric// .
%! alpha_numeric(?Code:code)// .
% Notice that there is a string asymmetry between the generative and
% the semidet case here.
%
% @see http://www.swi-prolog.org/pldoc/doc_for?object=char_type/2

alpha_numeric --> alpha_numeric(_).
alpha_numeric(C) -->
  [C],
  {code_type(C, alnum)}.



%! bracket// .
%! bracket(?Code:code)// .
%! bracket(?Type:oneof([angular,curly,langular,round,square]), ?Code:code)// .

bracket --> bracket(_).
bracket(C) --> bracket(_, C).
bracket(Type, C) --> closing_bracket(Type, C).
bracket(Type, C) --> opening_bracket(Type, C).



%! character_tie// .
%! character_tie(?Code:code)// .
% ⁀

character_tie --> character_tie(_).
character_tie(8256) --> [8256].



%! closing_bracket// .
%! closing_bracket(?Code:code)// .
%! closing_bracket(
%!   ?Type:oneof([angular,curly,langular,round,square]),
%!   ?Code:code
%! )// .

closing_bracket --> closing_bracket(_).
closing_bracket(C) --> closing_bracket(_, C).
closing_bracket(Type, C) --> ascii_closing_bracket(Type, C).
closing_bracket(langular, 12297) --> [12297].



%! entails// .
%! entails(?Code:code)// .
% ⊨

entails --> entails(_).
entails(8658) --> [8658].


%! equivalence// .
%! equivalence(?Code:code)// .
% ≡

equivalence --> equivalence(_).
equivalence(8801) --> [8801].



%! falsum// .
%! falsum(?Code:code)// .
% ⊥

falsum --> falsum(_).
falsum(8869) --> [8869].



%! graphic// .
%! graphic(?Code:code)// .

graphic --> graphic(_).
graphic(C) -->
  [C],
  {code_type(C, graph)}.



%! letter// .
%! letter(?Code:code)// .

letter --> letter(_).
letter(C) -->
  [C],
  {code_type(C, alpha)}.



%! letter_lowercase// .
%! letter_lowercase(?Code:code)// .

letter_lowercase --> letter_lowercase(_).
letter_lowercase(C) -->
  [C],
  {code_type(C, lower)}.



%! letter_uppercase// .
%! letter_uppercase(?Code:code)// .

letter_uppercase --> letter_uppercase(_).
letter_uppercase(C) -->
  [C],
  {code_type(C, upper)}.



%! line_separator// .
%! line_separator(?Code:code)// .

line_separator --> line_separator(_).
line_separator(8232) --> [8232].



%! line_terminator// .
%! line_terminator(?Code:code)// .

line_terminator --> line_terminator(_).
line_terminator(C) --> ascii_line_terminator(C).
line_terminator(C) --> next_line(C).
line_terminator(C) --> line_separator(C).
line_terminator(C) --> paragraph_separator(C).



%! middle_dot// .
%! middle_dot(?Code:code)// .
% ·

middle_dot --> middle_dot(_).
middle_dot(183) --> [183].



%! models// .
%! models(?Code:code)// .
% ⊧

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
opening_bracket(C) --> opening_bracket(_, C).
opening_bracket(Type, C) --> ascii_opening_bracket(Type, C).
opening_bracket(langular, 12296) --> [12296].



%! orgham_space_mark// .
%! orgham_space_mark(?Code:code)// .
%  

orgham_space_mark --> orgham_space_mark(_).
orgham_space_mark(5760) --> [5760].



%! paragraph_separator// .
%! paragraph_separator(?Code:code)// .

paragraph_separator --> paragraph_separator(_).
paragraph_separator(8233) --> [8233].



%! provable// .
%! provable(?Code:code)// .
% ⊢

provable --> provable(_).
provable(8866) --> [8866].


%! punctuation// .
%! punctuation(?Code:code)// .

punctuation --> punctuation(_).
punctuation(C) -->
  [C],
  {code_type(C, punct)}.



%! set_membership// .
%! set_membership(?Code:code)// .
% ∊

set_membership --> set_membership(_).
set_membership(8714) --> [8714].



%! subclass// .
%! subclass(?Code:code)// .
% ⊆

subclass --> subclass(_).
subclass(8838) --> [8838].



%! undertie// .
%! undertie(?Code:code)// .
% ‿

undertie --> undertie(_).
undertie(8255) --> [8255].



%! u_white// .
%! u_white(?Code:code)// .
% @compat http://en.wikipedia.org/wiki/Whitespace_character
% @tbd Enter the rest of the table.

u_white --> u_white(_).
u_white(C) --> ascii_white(C).
u_white(C) --> next_line(C).
u_white(C) --> nonbreaking_space(C).
u_white(C) --> orgham_space_mark(C).
% ...



%! zero_width_joiner// .
%! zero_width_joiner(?Code:code)// .

zero_width_joiner --> zero_width_joiner(_).
zero_width_joiner(8203) --> [8203].



%! zero_width_non_joiner// .
%! zero_width_non_joiner(?Code:code)// .

zero_width_non_joiner --> zero_width_non_joiner(_).
zero_width_non_joiner(8204) --> [8204].
