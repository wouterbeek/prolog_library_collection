:- module(
  dcg_unicode,
  [
    alpha_numeric//0,
    alpha_numeric//1,         % ?C
    bracket//0,		      
    bracket//1,               % ?C
    bracket//2,               % ?Type:oneof([angular,curly,round,square,ungular]), ?C
    character_tie//0,	      
    character_tie//1,         % ?C
    closing_bracket//0,	      
    closing_bracket//1,       % ?C
    closing_bracket//2,       % ?Type:oneof([angular,curly,round,square,ungular])
                              % ?C
    entails//0,		      
    entails//1,               % ?C
    equivalence//0,	      
    equivalence//1,           % ?C
    falsum//0,		      
    falsum//1,                % ?C
    graphic//0,		      
    graphic//1,               % ?C
    letter//0,		      
    letter//1,                % ?C
    letter_lowercase//0,      
    letter_lowercase//1,      % ?C
    letter_uppercase//0,      
    letter_uppercase//1,      % ?C
    line_separator//0,	      
    line_separator//1,        % ?C
    line_terminator//0,	      
    line_terminator//1,       % ?C
    middle_dot//0,	      
    middle_dot//1,            % ?C
    models//0,		      
    models//1,                % ?C
    next_line//0,	      
    next_line//1,             % ?C
    nonbreaking_space//0,     
    nonbreaking_space//1,     % ?C
    opening_bracket//0,	      
    opening_bracket//1,       % ?C
    opening_bracket//2,       % ?Type:oneof([angular,curly,round,square,ungular]), ?C
    orgham_space_mark//0,     
    orgham_space_mark//1,     % ?C
    provable//0,	      
    provable//1,              % ?C
    punctuation//0,	      
    punctuation//1,           % ?C
    set_membership//0,	      
    set_membership//1,        % ?C
    subclass//0,	      
    subclass//1,              % ?C
    undertie//0,	      
    undertie//1,              % ?C
    u_white//0,
    u_white//1,               % ?C
    zero_width_joiner//0,
    zero_width_joiner//1,     % ?C
    zero_width_non_joiner//0,
    zero_width_non_joiner//1  % ?C
  ]
).

/** <module> DCG Unicode

DCG rules that encode characters from the UNICODE standard.

@author Wouter Beek
@version 2015/07-2015/09. 2016/02
*/





%! alpha_numeric// .
%! alpha_numeric(?C)// .
% Notice that there is a string asymmetry between the generative and
% the semidet case here.
%
% @see http://www.swi-prolog.org/pldoc/doc_for?object=char_type/2

alpha_numeric --> alpha_numeric(_).
alpha_numeric(C) -->
  [C],
  {code_type(C, alnum)}.



%! bracket// .
%! bracket(?C)// .
%! bracket(?Type:oneof([angular,curly,round,square,ungular]), ?C)// .

bracket --> bracket(_).
bracket(C) --> bracket(_, C).
bracket(Type, C) --> closing_bracket(Type, C).
bracket(Type, C) --> opening_bracket(Type, C).



%! character_tie// .
%! character_tie(?C)// .
% ⁀

character_tie --> character_tie(_).
character_tie(8256) --> [8256].



%! closing_bracket// .
%! closing_bracket(?C)// .
%! closing_bracket(?Type:oneof([angular,curly,round,square,ungular]), ?C)// .

closing_bracket --> closing_bracket(_).
closing_bracket(C) --> closing_bracket(_, C).
closing_bracket(Type, C) --> ascii_closing_bracket(Type, C).
closing_bracket(uangular, 12297) --> [12297].



%! entails// .
%! entails(?C)// .
% ⊨

entails --> entails(_).
entails(8658) --> [8658].


%! equivalence// .
%! equivalence(?C)// .
% ≡

equivalence --> equivalence(_).
equivalence(8801) --> [8801].



%! falsum// .
%! falsum(?C)// .
% ⊥

falsum --> falsum(_).
falsum(8869) --> [8869].



%! graphic// .
%! graphic(?C)// .

graphic --> graphic(_).
graphic(C) -->
  [C],
  {code_type(C, graph)}.



%! letter// .
%! letter(?C)// .

letter --> letter(_).
letter(C) -->
  [C],
  {code_type(C, alpha)}.



%! letter_lowercase// .
%! letter_lowercase(?C)// .

letter_lowercase --> letter_lowercase(_).
letter_lowercase(C) -->
  [C],
  {code_type(C, lower)}.



%! letter_uppercase// .
%! letter_uppercase(?C)// .

letter_uppercase --> letter_uppercase(_).
letter_uppercase(C) -->
  [C],
  {code_type(C, upper)}.



%! line_separator// .
%! line_separator(?C)// .

line_separator --> line_separator(_).
line_separator(8232) --> [8232].



%! line_terminator// .
%! line_terminator(?C)// .

line_terminator --> line_terminator(_).
line_terminator(C) --> ascii_line_terminator(C).
line_terminator(C) --> next_line(C).
line_terminator(C) --> line_separator(C).
line_terminator(C) --> paragraph_separator(C).



%! middle_dot// .
%! middle_dot(?C)// .
% ·

middle_dot --> middle_dot(_).
middle_dot(183) --> [183].



%! models// .
%! models(?C)// .
% ⊧

models --> models(_).
models(8871) --> [8871].



%! next_line// .
%! next_line(?C)// .

next_line --> next_line(_).
next_line(133) --> [133].



%! nonbreaking_space// .
%! nonbreaking_space(?C)// .

nonbreaking_space --> nonbreaking_space(_).
nonbreaking_space(160) --> [160].



%! opening_bracket// .
%! opening_bracket(?C)// .
%! opening_bracket(?Type:oneof([angular,curly,round,square,ungular]), ?C)// .

opening_bracket --> opening_bracket(_).
opening_bracket(C) --> opening_bracket(_, C).
opening_bracket(Type, C) --> ascii_opening_bracket(Type, C).
opening_bracket(uangular, 12296) --> [12296].



%! orgham_space_mark// .
%! orgham_space_mark(?C)// .
%  

orgham_space_mark --> orgham_space_mark(_).
orgham_space_mark(5760) --> [5760].



%! paragraph_separator// .
%! paragraph_separator(?C)// .

paragraph_separator --> paragraph_separator(_).
paragraph_separator(8233) --> [8233].



%! provable// .
%! provable(?C)// .
% ⊢

provable --> provable(_).
provable(8866) --> [8866].


%! punctuation// .
%! punctuation(?C)// .

punctuation --> punctuation(_).
punctuation(C) -->
  [C],
  {code_type(C, punct)}.



%! set_membership// .
%! set_membership(?C)// .
% ∊

set_membership --> set_membership(_).
set_membership(8714) --> [8714].



%! subclass// .
%! subclass(?C)// .
% ⊆

subclass --> subclass(_).
subclass(8838) --> [8838].



%! undertie// .
%! undertie(?C)// .
% ‿

undertie --> undertie(_).
undertie(8255) --> [8255].



%! u_white// .
%! u_white(?C)// .
% @compat http://en.wikipedia.org/wiki/Whitespace_character
% @tbd Enter the rest of the table.

u_white --> u_white(_).
u_white(C) --> ascii_white(C).
u_white(C) --> next_line(C).
u_white(C) --> nonbreaking_space(C).
u_white(C) --> orgham_space_mark(C).
% ...



%! zero_width_joiner// .
%! zero_width_joiner(?C)// .

zero_width_joiner --> zero_width_joiner(_).
zero_width_joiner(8203) --> [8203].



%! zero_width_non_joiner// .
%! zero_width_non_joiner(?C)// .

zero_width_non_joiner --> zero_width_non_joiner(_).
zero_width_non_joiner(8204) --> [8204].
