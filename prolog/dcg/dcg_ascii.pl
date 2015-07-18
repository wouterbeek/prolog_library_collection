:- module(
  dcg_ascii,
  [
    a//0,
    a//1, % ?Code:code
    a//2, % ?Code:code
          % ?Index:between(1,26)
    a_lowercase//0,
    a_lowercase//1, % ?Code:code
    a_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    a_uppercase//0,
    a_uppercase//1, % ?Code:code
    a_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    acknowledgement//0,
    acknowledgement//1, % ?Code:code
    ascii_alpha_numeric//0,
    ascii_alpha_numeric//1, % ?Code:code
    ampersand//0,
    ampersand//1, % ?Code:code
    ampersat//0,
    ampersat//1, % ?Code:code
    angular_bracket//0,
    angular_bracket//1, % ?Code:code
    angular_bracket//2, % ?Type:oneof([angular])
                        % ?Code:code
    apetail//0,
    apetail//1, % ?Code:code
    apostrophe//0,
    apostrophe//1, % ?Code:code
    asterisk//0,
    asterisk//1, % ?Code:code
    at_sign//0,
    at_sign//1, % ?Code:code
    at_symbol//0,
    at_symbol//1, % ?Code:code
    b//0,
    b//1, % ?Code:code
    b//2, % ?Code:code
          % ?Index:between(1,26)
    b_lowercase//0,
    b_lowercase//1, % ?Code:code
    b_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    b_uppercase//0,
    b_uppercase//1, % ?Code:code
    b_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    backslash//0,
    backslash//1, % ?Code:code
    backspace//0,
    backspace//1, % ?Code:code
    bell//0,
    bell//1, % ?Code:code
    binary_digit//0,
    binary_digit//1, % ?Code:code
    binary_digit//2, % ?Weight:between(0,1)
                     % ?Code:code
    ascii_bracket//0,
    ascii_bracket//1, % ?Code:code
    ascii_bracket//2, % ?Type:oneof([angular,curly,round,square])
                      % ?Code:code
    c//0,
    c//1, % ?Code:code
    c//2, % ?Code:code
          % ?Index:between(1,26)
    c_lowercase//0,
    c_lowercase//1, % ?Code:code
    c_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    c_uppercase//0,
    c_uppercase//1, % ?Code:code
    c_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    cancel//0,
    cancel//1, % ?Code:code
    caret//0,
    caret//1, % ?Code:code
    carriage_return//0,
    carriage_return//1, % ?Code:code
    character//0,
    character//1, % ?Code:code
    character_tabulation//0,
    character_tabulation//1, % ?Code:code
    circle_bracket//0,
    circle_bracket//1, % ?Code:code
    circle_bracket//2, % ?Type:oneof([round])
                       % ?Code:code
    closing_angular_bracket//0,
    closing_angular_bracket//1, % ?Code:code
    closing_angular_bracket//2, % ?Type:oneof([angular])
                                % ?Code:code
    ascii_closing_bracket//0,
    ascii_closing_bracket//1, % ?Code:code
    ascii_closing_bracket//2, % ?Type:oneof([angular,curly,round,square])
                              % ?Code:code
    closing_curly_bracket//0,
    closing_curly_bracket//1, % ?Code:code
    closing_curly_bracket//2, % ?Type:oneof([curly])
                              % ?Code:code
    closing_round_bracket//0,
    closing_round_bracket//1, % ?Code:code
    closing_round_bracket//2, % ?Type:oneof([round])
                              % ?Code:code
    closing_square_bracket//0,
    closing_square_bracket//1, % ?Code:code
    closing_square_bracket//2, % ?Type:oneof([square])
                               % ?Code:code
    colon//0,
    colon//1, % ?Code:code
    comma//0,
    comma//1, % ?Code:code
    commercial_at//0,
    commercial_at//1, % ?Code:code
    control//0,
    control//1, % ?Code:code
    copyright//0,
    copyright//1, % ?Code:code
    crosshatch//0,
    crosshatch//1, % ?Code:code
    curly_bracket//0,
    curly_bracket//1, % ?Code:code
    curly_bracket//2, % ?Type:oneof([curly])
                      % ?Code:code
    d//0,
    d//1, % ?Code:code
    d//2, % ?Code:code
          % ?Index:between(1,26)
    d_lowercase//0,
    d_lowercase//1, % ?Code:code
    d_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    d_uppercase//0,
    d_uppercase//1, % ?Code:code
    d_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    data_link_escape//0,
    data_link_escape//1, % ?Code:code
    decimal_digit//0,
    decimal_digit//1, % ?Weight:between(0,9)
    decimal_digit//2, % ?Weight:between(0,9)
                      % ?Code:code
    ascii_delete//0,
    ascii_delete//1, % ?Code:code
    device_control//0,
    device_control//1, % ?Code:code
    device_control_1//0,
    device_control_1//1, % ?Code:code
    device_control_2//0,
    device_control_2//1, % ?Code:code
    device_control_3//0,
    device_control_3//1, % ?Code:code
    device_control_4//0,
    device_control_4//1, % ?Code:code
    dollar_sign//0,
    dollar_sign//1, % ?Code:code
    dot//0,
    dot//1, % ?Code:code
    double_quote//0,
    double_quote//1, % ?Code:code
    e//0,
    e//1, % ?Code:code
    e//2, % ?Code:code
          % ?Index:between(1,26)
    e_lowercase//0,
    e_lowercase//1, % ?Code:code
    e_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    e_uppercase//0,
    e_uppercase//1, % ?Code:code
    e_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    eight//0,
    eight//1, % ?Code:code
    eight//2, % ?Weight:between(8,8)
              % ?Code:code
    end_of_line//0,
    end_of_line//1, % ?Code:code
    end_of_medium//0,
    end_of_medium//1, % ?Code:code
    end_of_text//0,
    end_of_text//1, % ?Code:code
    end_of_transmission//0,
    end_of_transmission//1, % ?Code:code
    end_of_transmission_block//0,
    end_of_transmission_block//1, % ?Code:code
    equals_sign//0,
    equals_sign//1, % ?Code:code
    enquiry//0,
    enquiry//1, % ?Code:code
    escape//0,
    escape//1, % ?Code:code
    exclamation_mark//0,
    exclamation_mark//1, % ?Code:code
    f//0,
    f//1, % ?Code:code
    f//2, % ?Code:code
          % ?Index:between(1,26)
    f_lowercase//0,
    f_lowercase//1, % ?Code:code
    f_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    f_uppercase//0,
    f_uppercase//1, % ?Code:code
    f_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    file_separator//0,
    file_separator//1, % ?Code:code
    five//0,
    five//1, % ?Code:code
    five//2, % ?Weight:between(5,5)
             % ?Code:code
    form_feed//0,
    form_feed//1, % ?Code:code
    forward_slash//0,
    forward_slash//1, % ?Code:code
    four//0,
    four//1, % ?Code:code
    four//2, % ?Weight:between(4,4)
             % ?Code:code
    g//0,
    g//1, % ?Code:code
    g//2, % ?Code:code
          % ?Index:between(1,26)
    g_lowercase//0,
    g_lowercase//1, % ?Code:code
    g_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    g_uppercase//0,
    g_uppercase//1, % ?Code:code
    g_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    ascii_graphic//0,
    ascii_graphic//1, % ?Code:code
    grave_accent//0,
    grave_accent//1, % ?Code:code
    greater_than_sign//0,
    greater_than_sign//1, % ?Code:code
    group_separator//0,
    group_separator//1, % ?Code:code
    h//0,
    h//1, % ?Code:code
    h//2, % ?Code:code
          % ?Index:between(1,26)
    h_lowercase//0,
    h_lowercase//1, % ?Code:code
    h_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    h_uppercase//0,
    h_uppercase//1, % ?Code:code
    h_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    hexadecimal_digit//0,
    hexadecimal_digit//1, % ?Weight:between(0,15)
    hexadecimal_digit//2, % ?Weight:between(0,15)
                          % ?Code:code
    horizontal_tab//0,
    horizontal_tab//1, % ?Code:code
    hyphen//0,
    hyphen//1, % ?Code:code
    hyphen_minus//0,
    hyphen_minus//1, % ?Code:code
    i//0,
    i//1, % ?Code:code
    i//2, % ?Code:code
          % ?Index:between(1,26)
    i_lowercase//0,
    i_lowercase//1, % ?Code:code
    i_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    i_uppercase//0,
    i_uppercase//1, % ?Code:code
    i_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    j//0,
    j//1, % ?Code:code
    j//2, % ?Code:code
          % ?Index:between(1,26)
    j_lowercase//0,
    j_lowercase//1, % ?Code:code
    j_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    j_uppercase//0,
    j_uppercase//1, % ?Code:code
    j_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    k//0,
    k//1, % ?Code:code
    k//2, % ?Code:code
          % ?Index:between(1,26)
    k_lowercase//0,
    k_lowercase//1, % ?Code:code
    k_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    k_uppercase//0,
    k_uppercase//1, % ?Code:code
    k_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    l//0,
    l//1, % ?Code:code
    l//2, % ?Code:code
          % ?Index:between(1,26)
    l_lowercase//0,
    l_lowercase//1, % ?Code:code
    l_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    l_uppercase//0,
    l_uppercase//1, % ?Code:code
    l_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    less_than_sign//0,
    less_than_sign//1, % ?Code:code
    ascii_letter//0,
    ascii_letter//1, % ?Code:code
    ascii_letter//2, % ?Code:code
                     % ?Index:between(1,26)
    ascii_letter_lowercase//0,
    ascii_letter_lowercase//1, % ?Code:code
    ascii_letter_lowercase//2, % ?Code:code
                               % ?Index:between(1,26)
    ascii_letter_uppercase//0,
    ascii_letter_uppercase//1, % ?Code:code
    ascii_letter_uppercase//2, % ?Code:code
                               % ?Index:between(1,26)
    line_feed//0,
    line_feed//1, % ?Code:code
    line_tabulation//0,
    line_tabulation//1, % ?Code:code
    ascii_line_terminator//0,
    ascii_line_terminator//1, % ?Code:code
    m//0,
    m//1, % ?Code:code
    m//2, % ?Code:code
          % ?Index:between(1,26)
    m_lowercase//0,
    m_lowercase//1, % ?Code:code
    m_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    m_uppercase//0,
    m_uppercase//1, % ?Code:code
    m_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    minus_sign//0,
    minus_sign//1, % ?Code:code
    n//0,
    n//1, % ?Code:code
    n//2, % ?Code:code
          % ?Index:between(1,26)
    n_lowercase//0,
    n_lowercase//1, % ?Code:code
    n_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    n_uppercase//0,
    n_uppercase//1, % ?Code:code
    n_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    negative_acknowledgement//0,
    negative_acknowledgement//1, % ?Code:code
    nine//0,
    nine//1, % ?Code:code
    nine//2, % ?Weight:between(9,9)
             % ?Code:code
    null//0,
    null//1, % ?Code:code
    number_sign//0,
    number_sign//1, % ?Code:code
    o//0,
    o//1, % ?Code:code
    o//2, % ?Code:code
          % ?Index:between(1,26)
    o_lowercase//0,
    o_lowercase//1, % ?Code:code
    o_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    o_uppercase//0,
    o_uppercase//1, % ?Code:code
    o_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    octal_digit//0,
    octal_digit//1, % ?Weight:between(0,7)
    octal_digit//2, % ?Weight:between(0,7)
                    % ?Code:code
    one//0,
    one//1, % ?Code:code
    one//2, % ?Weight:between(1,1)
            % ?Code:code
    opening_angular_bracket//0,
    opening_angular_bracket//1, % ?Code:code
    opening_angular_bracket//2, % ?Type:oneof([angular])
                                % ?Code:code
    ascii_opening_bracket//0,
    ascii_opening_bracket//1, % ?Code:code
    ascii_opening_bracket//2, % ?Type:oneof([angular,curly,round,square])
                              % ?Code:code
    opening_curly_bracket//0,
    opening_curly_bracket//1, % ?Code:code
    opening_curly_bracket//2, % ?Type:oneof([curly])
                              % ?Code:code
    opening_round_bracket//0,
    opening_round_bracket//1, % ?Code:code
    opening_round_bracket//2, % ?Type:oneof([round])
                              % ?Code:code
    opening_square_bracket//0,
    opening_square_bracket//1, % ?Code:code
    opening_square_bracket//2, % ?Type:oneof([square])
                               % ?Code:code
    p//0,
    p//1, % ?Code:code
    p//2, % ?Code:code
          % ?Index:between(1,26)
    p_lowercase//0,
    p_lowercase//1, % ?Code:code
    p_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    p_uppercase//0,
    p_uppercase//1, % ?Code:code
    p_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    parenthesis//0,
    parenthesis//1, % ?Code:code
    percent_sign//0,
    percent_sign//1, % ?Code:code
    plus_sign//0,
    plus_sign//1, % ?Code:code
    positive_acknowledgement//0,
    positive_acknowledgement//1, % ?Code:code
    ascii_print//0,
    ascii_print//1, % ?Code:code
    ascii_punctuation//0,
    ascii_punctuation//1, % ?Code:code
    q//0,
    q//1, % ?Code:code
    q//2, % ?Code:code
          % ?Index:between(1,26)
    q_lowercase//0,
    q_lowercase//1, % ?Code:code
    q_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    q_uppercase//0,
    q_uppercase//1, % ?Code:code
    q_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    question_mark//0,
    question_mark//1, % ?Code:code
    r//0,
    r//1, % ?Code:code
    r//2, % ?Code:code
          % ?Index:between(1,26)
    r_lowercase//0,
    r_lowercase//1, % ?Code:code
    r_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    r_uppercase//0,
    r_uppercase//1, % ?Code:code
    r_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    record_separator//0,
    record_separator//1, % ?Code:code
    round_bracket//0,
    round_bracket//1, % ?Code:code
    round_bracket//2, % ?Type:oneof([round])
                      % ?Code:code
    s//0,
    s//1, % ?Code:code
    s//2, % ?Code:code
          % ?Index:between(1,26)
    s_lowercase//0,
    s_lowercase//1, % ?Code:code
    s_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    s_uppercase//0,
    s_uppercase//1, % ?Code:code
    s_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    semi_colon//0,
    semi_colon//1, % ?Code:code
    seven//0,
    seven//1, % ?Code:code
    seven//2, % ?Weight:between(7,7)
              % ?Code:code
    shift//0,
    shift//1, % ?Code:code
    shift_in//0,
    shift_in//1, % ?Code:code
    shift_out//0,
    shift_out//1, % ?Code:code
    single_quote//0,
    single_quote//1, % ?Code:code
    six//0,
    six//1, % ?Code:code
    six//2, % ?Weight:between(6,6)
            % ?Code:code
    slash//0,
    slash//1, % ?Code:code
    soft_bracket//0,
    soft_bracket//1, % ?Code:code
    space//0,
    space//1, % ?Code:code
    square_bracket//0,
    square_bracket//1, % ?Code:code
    square_bracket//2, % ?Type:oneof([square])
                       % ?Code:code
    start_of_heading//0,
    start_of_heading//1, % ?Code:code
    start_of_text//0,
    start_of_text//1, % ?Code:code
    substitute//0,
    substitute//1, % ?Code:code
    synchronous_idle//0,
    synchronous_idle//1, % ?Code:code
    t//0,
    t//1, % ?Code:code
    t//2, % ?Code:code
          % ?Index:between(1,26)
    t_lowercase//0,
    t_lowercase//1, % ?Code:code
    t_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    t_uppercase//0,
    t_uppercase//1, % ?Code:code
    t_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    ascii_tab//0,
    ascii_tab//1, % ?Code:code
    three//0,
    three//1, % ?Code:code
    three//2, % ?Weight:between(3,3)
              % ?Code:code
    tilde//0,
    tilde//1, % ?Code:code
    two//0,
    two//1, % ?Code:code
    two//2, % ?Weight:between(2,2)
            % ?Code:code
    u//0,
    u//1, % ?Code:code
    u//2, % ?Code:code
          % ?Index:between(1,26)
    u_lowercase//0,
    u_lowercase//1, % ?Code:code
    u_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    u_uppercase//0,
    u_uppercase//1, % ?Code:code
    u_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    underscore//0,
    underscore//1, % ?Code:code
    unit_separator//0,
    unit_separator//1, % ?Code:code
    v//0,
    v//1, % ?Code:code
    v//2, % ?Code:code
          % ?Index:between(1,26)
    v_lowercase//0,
    v_lowercase//1, % ?Code:code
    v_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    v_uppercase//0,
    v_uppercase//1, % ?Code:code
    v_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    vertical_bar//0,
    vertical_bar//1, % ?Code:code
    vertical_tab//0,
    vertical_tab//1, % ?Code:code
    w//0,
    w//1, % ?Code:code
    w//2, % ?Code:code
          % ?Index:between(1,26)
    w_lowercase//0,
    w_lowercase//1, % ?Code:code
    w_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    w_uppercase//0,
    w_uppercase//1, % ?Code:code
    w_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    ascii_white//0,
    ascii_white//1, % ?Code:code
    x//0,
    x//1, % ?Code:code
    x//2, % ?Code:code
          % ?Index:between(1,26)
    x_lowercase//0,
    x_lowercase//1, % ?Code:code
    x_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    x_uppercase//0,
    x_uppercase//1, % ?Code:code
    x_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    y//0,
    y//1, % ?Code:code
    y//2, % ?Code:code
          % ?Index:between(1,26)
    y_lowercase//0,
    y_lowercase//1, % ?Code:code
    y_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    y_uppercase//0,
    y_uppercase//1, % ?Code:code
    y_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    z//0,
    z//1, % ?Code:code
    z//2, % ?Code:code
          % ?Index:between(1,26)
    z_lowercase//0,
    z_lowercase//1, % ?Code:code
    z_lowercase//2, % ?Code:code
                    % ?Index:between(1,26)
    z_uppercase//0,
    z_uppercase//1, % ?Code:code
    z_uppercase//2, % ?Code:code
                    % ?Index:between(1,26)
    zero//0,
    zero//1, % ?Code:code
    zero//2 % ?Weight:between(0,0)
            % ?Code:code
  ]
).

/** <module> DCG ASCII

This module allows all individual
[ASCII characters](http://en.wikipedia.org/wiki/ASCII)
to be refered to within DCG by name.

In addition this module defines meaningful groupings of ASCII characters,
such as brackets, lowercase letters, control characters,
hexadecimal digits, whites, etc.

Every character and characer group comes with a variant DCG
that has the decimal character code as additional argument.

As an example, we can define the fragment of a URI (per RFC 3986)
without having to look up the numeric ASCII codes,
resulting in better readable code as well:

```prolog
fragment(Frag) -->
  '*'(fragment_code, Cs, []),
  {atom_codes(Frag, Cs)}.

fragment_code(C) --> pchar(C).
fragment_code(C) --> forward_slash(C).
fragment_code(C) --> question_mark(C).
```

This can be especially useful for characers that are non-graphic
or that look similar to other characers on some displays
(e.g., carriage return and line-feed).

Alpha-numeric characters (i.e., `0-9A-Za-z`) also have a variant DCG
with a weight argument that represents the count-by-one index of
the character in the sequence of decimal digits and the alphabet
respectively.

Bracket characters and bracket character groups have a variant
with an additional `Type` argument which is either `angular`, `curly`,
`round`, or `square`.
A handy special case for this occurs with content that is
(consistently) surrounded by some bracket type,
possibly returning the bracket type to the calling context:

```prolog
bracketed_content(Type, Dcg_0) -->
  ascii_opening_bracket(Type, _),
  Dcg_0,
  ascii_closing_bracket(Type, _).
```

The above can e.g. be used to parse
[Markdown URL notation](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet#links):

```prolog
markdown_url(Label_0, Url_0) -->
  bracketed_content(square, Label_0),
  bracketed_content(round, Url_0).
```

---

@author Wouter Beek
@see http://www.ascii-code.com/
@version 2015/07
*/



%! a// .
%! a(?Code:code)// .
%! a(?Code:code, ?Index:between(1,26))// .

a --> a(_).
a(Code) --> a(Code, _).
a(Code, Index) --> a_uppercase(Code, Index).
a(Code, Index) --> a_lowercase(Code, Index).

%! a_lowercase// .
%! a_lowercase(?Code:code)// .
%! a_lowercase(?Code:code, ?Index:between(1,26))// .

a_lowercase --> a_lowercase(_).
a_lowercase(Code) --> a_lowercase(Code, _).
a_lowercase(97, 1) --> [97].

%! a_uppercase// .
%! a_uppercase(?Code:code)// .
%! a_uppercase(?Code:code, ?Index:between(1,26))// .

a_uppercase --> a_uppercase(_).
a_uppercase(Code) --> a_uppercase(Code, _).
a_uppercase(65, 1) --> [65].

%! acknowledgement// .
%! acknowledgement(?Code:code)// .

acknowledgement --> acknowledgement(_).
acknowledgement(Code) --> negative_acknowledgement(Code).
acknowledgement(Code) --> positive_acknowledgement(Code).

%! ascii_alpha_numeric// .
%! ascii_alpha_numeric(?Code:code)// .
% ASCII alpha-numeric characters are ASCII letters and digits.

ascii_alpha_numeric --> ascii_alpha_numeric(_).
ascii_alpha_numeric(Code) --> ascii_letter(Code).
ascii_alpha_numeric(Code) --> decimal_digit(_, Code).

%! ampersand// .
%! ampersand(?Code:code)// .

ampersand --> ampersand(_).
ampersand(38) --> [38].

%! ampersat// .
%! ampersat(?Code:code)// .
% An @-sign is sometimes called an **ampersat**.

ampersat --> ampersat(_).
ampersat(Code) --> at_sign(Code).

%! angular_bracket// .
%! angular_bracket(?Code:code)// .
%! angular_bracket(?Type:oneof([angular]), ?Code:code)// .

angular_bracket --> angular_bracket(_).
angular_bracket(Code) --> angular_bracket(_, Code).
angular_bracket(Type, Code) --> opening_angular_bracket(Type, Code).
angular_bracket(Type, Code) --> closing_angular_bracket(Type, Code).

%! apetail// .
%! apetail(?Code:code)// .
% An @-sign is sometimes called an **apetail**.

apetail --> apetail(_).
apetail(Code) --> at_sign(Code).

%! apostrophe// .
%! apostrophe(?Code:code)// .

apostrophe --> apostrophe(_).
apostrophe(39) --> [39].

%! asterisk// .
%! asterisk(?Code:code)// .

asterisk --> asterisk(_).
asterisk(42) --> [42].

%! at_sign// .
%! at_sign(?Code:code)// .

at_sign --> at_sign(_).
at_sign(64) --> [64].

%! at_symbol// .
%! at_symbol(?Code:code)// .
% The @-sign is sometimes called the **@-symbol**.

at_symbol --> at_sign.
at_symbol(Code) --> at_sign(Code).

%! b// .
%! b(?Code:code)// .
%! b(?Code:code, ?Index:between(1,26))// .

b --> b(_).
b(Code) --> b(Code, _).
b(Code, Index) --> b_uppercase(Code, Index).
b(Code, Index) --> b_lowercase(Code, Index).

%! b_lowercase// .
%! b_lowercase(?Code:code)// .
%! b_lowercase(?Code:code, ?Index:between(1,26))// .

b_lowercase --> b_lowercase(_).
b_lowercase(Code) --> b_lowercase(Code, _).
b_lowercase(98, 2) --> [98].

%! b_uppercase// .
%! b_uppercase(?Code:code)// .
%! b_uppercase(?Code:code, ?Index:between(1,26))// .

b_uppercase --> b_uppercase(_).
b_uppercase(Code) --> b_uppercase(Code, _).
b_uppercase(66, 2) --> [66].

%! backslash// .
%! backslash(?Code:code)// .

backslash --> backslash(_).
backslash(92) --> [92].

%! backspace// .
%! backspace(?Code:code)// .

backspace --> backspace(_).
backspace(8) --> [8].

%! bell// .
%! bell(?Code:code)// .

bell --> bell(_).
bell(7) --> [7].

%! binary_digit// .
%! binary_digit(?Weight:between(0,1))// .
%! binary_digit(?Weight:between(0,1), ?Code:code)// .

binary_digit --> binary_digit(_).
binary_digit(Weight) --> binary_digit(Weight, _).
binary_digit(Weight, Code) --> zero(Weight, Code).
binary_digit(Weight, Code) --> one(Weight, Code).

%! ascii_bracket// .
%! ascii_bracket(?Code:code)// .
%! bracket(?Type:oneof([angular,curly,round,square]), ?Code:code)// .

ascii_bracket --> ascii_bracket(_).
ascii_bracket(Code) --> ascii_bracket(_, Code).
ascii_bracket(Type, Code) --> ascii_closing_bracket(Type, Code).
ascii_bracket(Type, Code) --> ascii_opening_bracket(Type, Code).

%! c// .
%! c(?Code:code)// .
%! c(?Code:code, ?Index:between(1,26))// .

c --> c(_).
c(Code) --> c(Code, _).
c(Code, Index) --> c_uppercase(Code, Index).
c(Code, Index) --> c_lowercase(Code, Index).

%! c_lowercase// .
%! c_lowercase(?Code:code)// .
%! c_lowercase(?Code:code, ?Index:between(1,26))// .

c_lowercase --> c_lowercase(_).
c_lowercase(Code) --> c_lowercase(Code, _).
c_lowercase(99, 3) --> [99].

%! c_uppercase// .
%! c_uppercase(?Code:code)// .
%! c_uppercase(?Code:code, ?Index:between(1,26))// .

c_uppercase --> c_uppercase(_).
c_uppercase(Code) --> c_uppercase(Code, _).
c_uppercase(67, 3) --> [67].

%! cancel// .
%! cancel(?Code:code)// .

cancel --> cancel(_).
cancel(24) --> [24].

%! caret// .
%! caret(?Code:code)// .

caret --> caret(_).
caret(94) --> [94].

%! carriage_return// .
%! carriage_return(?Code:code)// .

carriage_return --> carriage_return(_).
carriage_return(13) --> [13].

%! character// .
%! character(?Code:code)// .
% ASCII characters are ASCII control, graphic, and white characters.

character --> character(_).
character(Code) --> control(Code).
character(Code) --> ascii_graphic(Code).
character(Code) --> ascii_white(Code).

%! character_tabulation// .
%! character_tabulation(?Code:code)// .

character_tabulation --> character_tabulation(_).
character_tabulation(Code) --> horizontal_tab(Code).

%! circle_bracket// .
%! circle_bracket(?Code:code)// .
%! circle_bracket(?Type:round, ?Code:code)// .
% Round brackets are sometimes called **circle brackets**.

circle_bracket --> circle_bracket(_).
circle_bracket(Code) --> circle_bracket(_, Code).
circle_bracket(Type, Code) --> round_bracket(Type, Code).

%! closing_angular_bracket// .
%! closing_angular_bracket(?Code:code)// .
%! closing_angular_bracket(?Type:oneof([angular]), ?Code:code)// .

closing_angular_bracket --> closing_angular_bracket(_).
closing_angular_bracket(Code) --> closing_angular_bracket(_, Code).
closing_angular_bracket(angular, Code) --> greater_than_sign(Code).

%! ascii_closing_bracket// .
%! ascii_closing_bracket(?Code:code)// .
%! ascii_closing_bracket(
%!   ?Type:oneof([angular,curly,round,square]),
%!   ?Code:code
%! )// .

ascii_closing_bracket --> ascii_closing_bracket(_).
ascii_closing_bracket(Code) --> ascii_closing_bracket(_, Code).
ascii_closing_bracket(Type, Code) --> closing_angular_bracket(Type, Code).
ascii_closing_bracket(Type, Code) --> closing_curly_bracket(Type, Code).
ascii_closing_bracket(Type, Code) --> closing_round_bracket(Type, Code).
ascii_closing_bracket(Type, Code) --> closing_square_bracket(Type, Code).

%! closing_curly_bracket// .
%! closing_curly_bracket(?Code:code)// .
%! closing_curly_bracket(?Type:oneof([curly]), ?Code:code)// .

closing_curly_bracket --> closing_curly_bracket(_).
closing_curly_bracket(Code) --> closing_curly_bracket(_, Code).
closing_curly_bracket(curly, 125) --> [125].

%! closing_round_bracket// .
%! closing_round_bracket(?Code:code)// .
%! closing_round_bracket(?Type:oneof([round]), ?Code:code)// .

closing_round_bracket --> closing_round_bracket(_).
closing_round_bracket(Code) --> closing_round_bracket(_, Code).
closing_round_bracket(round, 41) --> [41].

%! closing_square_bracket// .
%! closing_square_bracket(?Code:code)// .
%! closing_square_bracket(?Type:oneof([square]), ?Code:code)// .

closing_square_bracket --> closing_square_bracket(_).
closing_square_bracket(Code) --> closing_square_bracket(_, Code).
closing_square_bracket(square, 93) --> [93].

%! colon// .
%! colon(?Code:code)// .

colon --> colon(_).
colon(58) --> [58].

%! comma// .
%! comma(?Code:code)// .

comma --> comma(_).
comma(44) --> [44].

%! commercial_at// .
%! commercial_at(?Code:code)// .
% An @-sign is sometimes called a **commercial-@**.

commercial_at --> commercial_at(_).
commercial_at(Code) --> at_sign(Code).

%! control// .
%! control(?Code:code)// .
% ASCII control characters.

control --> control(_).
control(Code) --> acknowledgement(Code).
control(Code) --> backspace(Code).
control(Code) --> bell(Code).
control(Code) --> cancel(Code).
control(Code) --> carriage_return(Code).
control(Code) --> data_link_escape(Code).
control(Code) --> ascii_delete(Code).
control(Code) --> device_control(Code).
control(Code) --> enquiry(Code).
control(Code) --> end_of_medium(Code).
control(Code) --> end_of_text(Code).
control(Code) --> end_of_transmission(Code).
control(Code) --> end_of_transmission_block(Code).
control(Code) --> escape(Code).
control(Code) --> file_separator(Code).
control(Code) --> form_feed(Code).
control(Code) --> group_separator(Code).
control(Code) --> line_feed(Code).
control(Code) --> null(Code).
control(Code) --> record_separator(Code).
control(Code) --> shift(Code).
control(Code) --> start_of_heading(Code).
control(Code) --> start_of_text(Code).
control(Code) --> substitute(Code).
control(Code) --> synchronous_idle(Code).
control(Code) --> ascii_tab(Code).
control(Code) --> unit_separator(Code).

%! copyright// .
%! copyright(?Code:code)// .

copyright --> copyright(_).
copyright(169) --> [169].

%! crosshatch// .
%! crosshatch(?Code:code)// .
% A number sign is sometimes called a **crosshatch**.

crosshatch --> crosshatch(_).
crosshatch(Code) --> number_sign(Code).

%! curly_bracket// .
%! curly_bracket(?Code:code)// .
%! curly_bracket(?Type:oneof([curly]), ?Code:code)// .

curly_bracket --> curly_bracket(_).
curly_bracket(Code) --> curly_bracket(_, Code).
curly_bracket(Type, Code) --> closing_curly_bracket(Type, Code).
curly_bracket(Type, Code) --> opening_curly_bracket(Type, Code).

%! d// .
%! d(?Code:code)// .
%! d(?Code:code, ?Index:between(1,26))// .

d --> d(_).
d(Code) --> d(_, Code).
d(Code, Index) --> d_uppercase(Code, Index).
d(Code, Index) --> d_lowercase(Code, Index).

%! d_lowercase// .
%! d_lowercase(?Code:code)// .
%! d_lowercase(?Code:code, ?Index:between(1,26))// .

d_lowercase --> d_lowercase(_).
d_lowercase(Code) --> d_lowercase(Code, _).
d_lowercase(100, 4) --> [100].

%! d_uppercase// .
%! d_uppercase(?Code:code)// .
%! d_uppercase(?Code:code, ?Index:between(1,26))// .

d_uppercase --> d_uppercase(_).
d_uppercase(Code) --> d_uppercase(Code, _).
d_uppercase(68, 4) --> [68].

%! data_link_escape// .
%! data_link_escape(?Code:code)// .

data_link_escape --> data_link_escape(_).
data_link_escape(16) --> [16].

%! decimal_digit// .
%! decimal_digit(?Weight:between(0,9))// .
%! decimal_digit(?Weight:between(0,9), ?Code:code)// .

decimal_digit --> decimal_digit(_).
decimal_digit(Weight) --> decimal_digit(Weight, _).
decimal_digit(Weight, Code) --> octal_digit(Weight, Code).
decimal_digit(Weight, Code) --> eight(Weight, Code).
decimal_digit(Weight, Code) --> nine(Weight, Code).

%! ascii_delete// .
%! ascii_delete(?Code:code)// .

ascii_delete --> [127].
ascii_delete(127) --> [127].

%! device_control// .
%! device_control(?Code:code)// .
% The device control characters are are device control 1, 2, 3 and 4.

device_control --> device_control(_).
device_control(Code) --> device_control_1(Code).
device_control(Code) --> device_control_2(Code).
device_control(Code) --> device_control_3(Code).
device_control(Code) --> device_control_4(Code).

%! device_control_1// .
%! device_control_1(?Code:code)// .

device_control_1 --> device_control_1(_).
device_control_1(17) --> [17].

%! device_control_2// .
%! device_control_2(?Code:code)// .

device_control_2 --> device_control_2(_).
device_control_2(18) --> [18].

%! device_control_3// .
%! device_control_3(?Code:code)// .

device_control_3 --> device_control_3(_).
device_control_3(19) --> [19].

%! device_control_4// .
%! device_control_4(?Code:code)// .

device_control_4 --> device_control_4(_).
device_control_4(20) --> [20].

%! dollar_sign// .
%! dollar_sign(?Code:code)// .

dollar_sign --> dollar_sign(_).
dollar_sign(36) --> [36].

%! dot// .
%! dot(?Code:code)// .

dot --> dot(_).
dot(46) --> [46].

%! double_quote// .
%! double_quote(?Code:code)// .

double_quote --> double_quote(_).
double_quote(34) --> [34].

%! e// .
%! e(?Code:code)// .
%! e(?Code:code, ?Index:between(1,26))// .

e --> e(_).
e(Code) --> e(Code, _).
e(Code, Index) --> e_uppercase(Code, Index).
e(Code, Index) --> e_lowercase(Code, Index).

%! e_lowercase// .
%! e_lowercase(?Code:code)// .
%! e_lowercase(?Code:code, ?Index:between(1,26))// .

e_lowercase --> e_lowercase(_).
e_lowercase(Code) --> e_lowercase(Code, _).
e_lowercase(101, 5) --> [101].

%! e_uppercase// .
%! e_uppercase(?Code:code)// .
%! e_uppercase(?Code:code, ?Index:between(1,26))// .

e_uppercase --> e_uppercase(_).
e_uppercase(Code) --> e_uppercase(Code, _).
e_uppercase(69, 5) --> [69].

%! eight// .
%! eight(?Code:code)// .
%! eight(?Weight:between(8,8), ?Code:code)// .

eight --> eight(_).
eight(Code) --> eight(_, Code).
eight(8, 56) --> [56].

%! end_of_line// .
%! end_of_line(?Code:code)// .

end_of_line --> end_of_line(_).
end_of_line(Code) --> carriage_return(Code).
end_of_line(Code) --> end_of_medium(Code).
end_of_line(Code) --> end_of_text(Code).
end_of_line(Code) --> end_of_transmission(Code).
end_of_line(Code) --> end_of_transmission_block(Code).
end_of_line(Code) --> line_feed(Code).

%! end_of_medium// .
%! end_of_medium(?Code:code)// .

end_of_medium --> end_of_medium(_).
end_of_medium(25) --> [25].

%! end_of_text// .
%! end_of_text(?Code:code)// .

end_of_text --> end_of_text(_).
end_of_text(3) --> [3].

%! end_of_transmission// .
%! end_of_transmission(?Code:code)// .

end_of_transmission --> end_of_transmission(_).
end_of_transmission(4) --> [4].

%! end_of_transmission_block// .
%! end_of_transmission_block(?Code:code)// .

end_of_transmission_block --> end_of_transmission_block(_).
end_of_transmission_block(23) --> [23].

%! equals_sign// .
%! equals_sign(?Code:code)// .

equals_sign --> equals_sign(_).
equals_sign(61) --> [61].

%! enquiry// .
%! enquiry(?Code:code)// .

enquiry --> enquiry(_).
enquiry(5) --> [5].

%! escape// .
%! escape(?Code:code)// .

escape --> escape(_).
escape(27) --> [27].

%! exclamation_mark// .
%! exclamation_mark(?Code:code)// .

exclamation_mark --> exclamation_mark(_).
exclamation_mark(33) --> [33].

%! f// .
%! f(?Code:code)// .
%! f(?Code:code, ?Index:between(1,26))// .

f --> f(_).
f(Code) --> f(Code, _).
f(Code, Index) --> f_uppercase(Code, Index).
f(Code, Index) --> f_lowercase(Code, Index).

%! f_lowercase// .
%! f_lowercase(?Code:code)// .
%! f_lowercase(?Code:code, ?Index:between(1,26))// .

f_lowercase --> f_lowercase(_).
f_lowercase(Code) --> f_lowercase(Code, _).
f_lowercase(102, 6) --> [102].

%! f_uppercase// .
%! f_uppercase(?Code:code)// .
%! f_uppercase(?Code:code, ?Index:between(1,26))// .

f_uppercase --> f_uppercase(_).
f_uppercase(Code) --> f_uppercase(Code, _).
f_uppercase(70, 6) --> [70].

%! file_separator// .
%! file_separator(?Code:code)// .

file_separator --> file_separator(_).
file_separator(28) --> [28].

%! five// .
%! five(?Code:code)// .
%! five(?Weight:between(5,5), ?Code:code)// .

five --> five(_).
five(Code) --> five(_, Code).
five(5, 53) --> [53].

%! form_feed// .
%! form_feed(?Code:code)// .

form_feed --> form_feed(_).
form_feed(12) --> [12].

%! forward_slash// .
%! forward_slash(?Code:code)// .

forward_slash --> forward_slash(_).
forward_slash(47) --> [47].

%! four// .
%! four(?Code:code)// .
%! four(?Weight:between(4,4), ?Code:code)// .

four --> four(_).
four(Code) --> four(_, Code).
four(4, 52) --> [52].

%! g// .
%! g(?Code:code)// .
%! g(?Code:code, ?Index:between(1,26))// .

g --> g(_).
g(Code) --> g(Code, _).
g(Code, Index) --> g_uppercase(Code, Index).
g(Code, Index) --> g_lowercase(Code, Index).

%! g_lowercase// .
%! g_lowercase(?Code:code)// .
%! g_lowercase(?Code:code, ?Index:between(1,26))// .

g_lowercase --> g_lowercase(_).
g_lowercase(Code) --> g_lowercase(Code, _).
g_lowercase(103, 7) --> [103].

%! g_uppercase// .
%! g_uppercase(?Code:code)// .
%! g_uppercase(?Code:code, ?Index:between(1,26))// .

g_uppercase --> g_uppercase(_).
g_uppercase(Code) --> g_uppercase(Code, _).
g_uppercase(71, 7) --> [71].

%! ascii_graphic// .
%! ascii_graphic(?Code:code)// .
% ASCII graphic characters are ASCII alpha-numeric characters
% and ASCII punctuation.

ascii_graphic --> ascii_graphic(_).
ascii_graphic(Code) --> ascii_alpha_numeric(Code).
ascii_graphic(Code) --> ascii_punctuation(Code).

%! grave_accent// .
%! grave_accent(?Code:code)// .

grave_accent --> grave_accent(_).
grave_accent(96) --> [96].

%! greater_than_sign// .
%! greater_than_sign(?Code:code)// .

greater_than_sign --> greater_than_sign(_).
greater_than_sign(62) --> [62].

%! group_separator// .
%! group_separator(?Code:code)// .

group_separator --> group_separator(_).
group_separator(29) --> [29].

%! h// .
%! h(?Code:code)// .
%! h(?Code:code, ?Index:between(1,26))// .

h --> h(_).
h(Code) --> h(Code, _).
h(Code, Index) --> h_uppercase(Code, Index).
h(Code, Index) --> h_lowercase(Code, Index).

%! h_lowercase// .
%! h_lowercase(?Code:code)// .
%! h_lowercase(?Code:code, ?Index:between(1,26))// .

h_lowercase --> h_lowercase(_).
h_lowercase(Code) --> h_lowercase(Code, _).
h_lowercase(104, 8) --> [104].

%! h_uppercase// .
%! h_uppercase(?Code:code)// .
%! h_uppercase(?Code:code, ?Index:between(1,26))// .

h_uppercase --> h_uppercase(_).
h_uppercase(Code) --> h_uppercase(Code, _).
h_uppercase(72, 8) --> [72].

%! hexadecimal_digit// .
%! hexadecimal_digit(?Weight:between(0,15))// .
%! hexadecimal_digit(?Weight:between(0,15), ?Code:code)// .

hexadecimal_digit --> hexadecimal_digit(_).
hexadecimal_digit(Weight) --> hexadecimal_digit(Weight, _).
hexadecimal_digit(Weight, Code) --> decimal_digit(Weight, Code).
hexadecimal_digit(10, Code) --> a(Code).
hexadecimal_digit(11, Code) --> b(Code).
hexadecimal_digit(12, Code) --> c(Code).
hexadecimal_digit(13, Code) --> d(Code).
hexadecimal_digit(14, Code) --> e(Code).
hexadecimal_digit(15, Code) --> f(Code).

%! horizontal_tab// .
%! horizontal_tab(?Code:code)// .

horizontal_tab --> horizontal_tab(_).
horizontal_tab(9) --> [9].

%! hyphen// .
%! hyphen(?Code:code)// .
% A hyphen/minus is sometimes called a **hyphen**.

hyphen --> hyphen(_).
hyphen(Code) --> hyphen_minus(Code).

%! hyphen_minus// .
%! hyphen_minus(?Code:code)// .

hyphen_minus --> hyphen_minus(_).
hyphen_minus(45) --> [45].

%! i// .
%! i(?Code:code)// .
%! i(?Code:code, ?Index:between(1,26))// .

i --> i(_).
i(Code) --> i(Code, _).
i(Code, Index) --> i_uppercase(Code, Index).
i(Code, Index) --> i_lowercase(Code, Index).

%! i_lowercase// .
%! i_lowercase(?Code:code)// .
%! i_lowercase(?Code:code, ?Index:between(1,26))// .

i_lowercase --> i_lowercase(_).
i_lowercase(Code) --> i_lowercase(Code, _).
i_lowercase(105, 9) --> [105].

%! i_uppercase// .
%! i_uppercase(?Code:code)// .
%! i_uppercase(?Code:code, ?Index:between(1,26))// .

i_uppercase --> i_uppercase(_).
i_uppercase(Code) --> i_uppercase(Code, _).
i_uppercase(73, 9) --> [73].

%! j// .
%! j(?Code:code)// .
%! j(?Code:code, ?Index:between(1,26))// .

j --> j(_).
j(Code) --> j(Code, _).
j(Code, Index) --> j_uppercase(Code, Index).
j(Code, Index) --> j_lowercase(Code, Index).

%! j_lowercase// .
%! j_lowercase(?Code:code)// .
%! j_lowercase(?Code:code, ?Index:between(1,26))// .

j_lowercase --> j_lowercase(_).
j_lowercase(Code) --> j_lowercase(Code, _).
j_lowercase(106, 10) --> [106].

%! j_uppercase// .
%! j_uppercase(?Code:code)// .
%! j_uppercase(?Code:code, ?Index:between(1,26))// .

j_uppercase --> j_uppercase(_).
j_uppercase(Code) --> j_uppercase(Code, _).
j_uppercase(74, 10) --> [74].

%! k// .
%! k(?Code:code)// .

k --> k(_).
k(Code) --> k(Code, _).
k(Code, Index) --> k_uppercase(Code, Index).
k(Code, Index) --> k_lowercase(Code, Index).

%! k_lowercase// .
%! k_lowercase(?Code:code)// .
%! k_lowercase(?Code:code, ?Index:between(1,26))// .

k_lowercase --> k_lowercase(_).
k_lowercase(Code) --> k_lowercase(Code, _).
k_lowercase(107, 11) --> [107].

%! k_uppercase// .
%! k_uppercase(?Code:code)// .
%! k_uppercase(?Code:code, ?Index:between(1,26))// .

k_uppercase --> k_uppercase(_).
k_uppercase(Code) --> k_uppercase(Code, _).
k_uppercase(75, 11) --> [75].

%! l// .
%! l(?Code:code)// .
%! l(?Code:code, ?Index:between(1,26))// .

l --> l(_).
l(Code) --> l(Code, _).
l(Code, Index) --> l_uppercase(Code, Index).
l(Code, Index) --> l_lowercase(Code, Index).

%! l_lowercase// .
%! l_lowercase(?Code:code)// .
%! l_lowercase(?Code:code, ?Index:between(1,26))// .

l_lowercase --> l_lowercase(_).
l_lowercase(Code) --> l_lowercase(Code, _).
l_lowercase(108, 12) --> [108].

%! l_uppercase// .
%! l_uppercase(?Code:code)// .
%! l_uppercase(?Code:code, ?Index:between(1,26))// .

l_uppercase --> l_uppercase(_).
l_uppercase(Code) --> l_uppercase(Code, _).
l_uppercase(76, 12) --> [76].

%! less_than_sign// .
%! less_than_sign(?Code:code)// .

less_than_sign --> less_than_sign(_).
less_than_sign(60) --> [60].

%! ascii_letter// .
%! ascii_letter(?Code:code)// .
%! ascii_letter(?Code:code, ?Index:between(1,26))// .

ascii_letter --> ascii_letter(_).
ascii_letter(Code) --> ascii_letter(Code, _).
ascii_letter(Code, Index) --> ascii_letter_lowercase(Code, Index).
ascii_letter(Code, Index) --> ascii_letter_uppercase(Code, Index).

%! ascii_letter_lowercase// .
%! ascii_letter_lowercase(?Code:code)// .
%! ascii_letter_lowercase(?Code:code, ?Index:between(1,26))// .

ascii_letter_lowercase --> ascii_letter_lowercase(_).
ascii_letter_lowercase(Code) --> ascii_letter_lowercase(Code, _).
ascii_letter_lowercase(Code, Index) --> a_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> b_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> c_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> d_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> e_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> f_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> g_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> h_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> i_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> j_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> k_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> l_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> m_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> n_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> o_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> p_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> q_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> r_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> s_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> t_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> u_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> v_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> w_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> x_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> y_lowercase(Code, Index).
ascii_letter_lowercase(Code, Index) --> z_lowercase(Code, Index).

%! ascii_letter_uppercase// .
%! ascii_letter_uppercase(?Code:code)// .
%! ascii_letter_uppercase(?Code:code, ?Index:between(1,26))// .

ascii_letter_uppercase --> ascii_letter_uppercase(_).
ascii_letter_uppercase(Code) --> ascii_letter_uppercase(Code, _).
ascii_letter_uppercase(Code, Index) --> a_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> b_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> c_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> d_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> e_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> f_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> g_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> h_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> i_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> j_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> k_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> l_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> m_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> n_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> o_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> p_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> q_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> r_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> s_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> t_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> u_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> v_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> w_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> x_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> y_uppercase(Code, Index).
ascii_letter_uppercase(Code, Index) --> z_uppercase(Code, Index).

%! line_feed// .
%! line_feed(?Code:code)// .

line_feed --> line_feed(_).
line_feed(10) --> [10].

%! line_tabulation// .
%! line_tabulation(?Code:code)// .

line_tabulation --> line_tabulation(_).
line_tabulation(Code) --> vertical_tab(Code).

%! ascii_line_terminator// .
%! ascii_line_terminator(?Code:code)// .

ascii_line_terminator --> ascii_line_terminator(_).
ascii_line_terminator(Code) --> carriage_return(Code).
ascii_line_terminator(Code) --> form_feed(Code).
ascii_line_terminator(Code) --> line_feed(Code).
ascii_line_terminator(Code) --> vertical_tab(Code).

%! m// .
%! m(?Code:code)// .
%! m(?Code:code, ?Index:between(1,26))// .

m --> m(_).
m(Code) --> m_uppercase(Code, _).
m(Code, Index) --> m_uppercase(Code, Index).
m(Code, Index) --> m_lowercase(Code, Index).

%! m_lowercase// .
%! m_lowercase(?Code:code)// .
%! m_lowercase(?Code:code, ?Index:between(1,26))// .

m_lowercase --> m_lowercase(_).
m_lowercase(Code) --> m_lowercase(Code, _).
m_lowercase(109, 13) --> [109].

%! m_uppercase// .
%! m_uppercase(?Code:code)// .
%! m_uppercase(?Code:code, ?Index:between(1,26))// .

m_uppercase --> m_uppercase(_).
m_uppercase(Code) --> m_uppercase(Code, _).
m_uppercase(77, 13) --> [77].

%! minus_sign// .
%! minus_sign(?Code:code)// .

minus_sign --> minus_sign(_).
minus_sign(Code) --> hyphen_minus(Code).

%! n// .
%! n(?Code:code)// .
%! n(?Code:code, ?Index:between(1,26))// .

n --> n(_).
n(Code) --> n(Code, _).
n(Code, Index) --> n_uppercase(Code, Index).
n(Code, Index) --> n_lowercase(Code, Index).

%! n_lowercase// .
%! n_lowercase(?Code:code)// .
%! n_lowercase(?Code:code, ?Index:between(1,26))// .

n_lowercase --> n_lowercase(_).
n_lowercase(Code) --> n_lowercase(Code, _).
n_lowercase(110, 14) --> [110].

%! n_uppercase// .
%! n_uppercase(?Code:code)// .
%! n_uppercase(?Code:code, ?Index:between(1,26))// .

n_uppercase --> n_uppercase(_).
n_uppercase(Code) --> n_uppercase(Code, _).
n_uppercase(78, 14) --> [78].

%! negative_acknowledgement// .
%! negative_acknowledgement(?Code:code)// .

negative_acknowledgement --> negative_acknowledgement(_).
negative_acknowledgement(21) --> [21].

%! nine// .
%! nine(?Code:code)// .
%! nine(?Weight:between(9,9), ?Code:code)// .

nine --> nine(_).
nine(Code) --> nine(_, Code).
nine(9, 57) --> [57].

%! null// .
%! null(?Code:code)// .

null --> null(_).
null(0) --> [0].

%! number_sign// .
%! number_sign(?Code:code)// .

number_sign --> number_sign(_).
number_sign(35) --> [35].

%! o// .
%! o(?Code:code)// .
%! o(?Code:code, ?Index:between(1,26))// .

o --> o(_).
o(Code) --> o(Code, _).
o(Code, Index) --> o_uppercase(Code, Index).
o(Code, Index) --> o_lowercase(Code, Index).

%! o_lowercase// .
%! o_lowercase(?Code:code)// .
%! o_lowercase(?Code:code, ?Index:between(1,26))// .

o_lowercase --> o_lowercase(_).
o_lowercase(Code) --> o_lowercase(Code, _).
o_lowercase(111, 15) --> [111].

%! o_uppercase// .
%! o_uppercase(?Code:code)// .
%! o_uppercase(?Code:code, ?Index:between(1,26))// .

o_uppercase --> o_uppercase(_).
o_uppercase(Code) --> o_uppercase(Code, _).
o_uppercase(79, 15) --> [79].

%! octal_digit// .
%! octal_digit(?Weight:between(0,7))// .
%! octal_digit(?Weight:between(0,7), ?Code:code)// .

octal_digit --> octal_digit(_).
octal_digit(Weight) --> octal_digit(Weight, _).
octal_digit(Weight, Code) --> binary_digit(Weight, Code).
octal_digit(Weight, Code) --> two(Weight, Code).
octal_digit(Weight, Code) --> three(Weight, Code).
octal_digit(Weight, Code) --> four(Weight, Code).
octal_digit(Weight, Code) --> five(Weight, Code).
octal_digit(Weight, Code) --> six(Weight, Code).
octal_digit(Weight, Code) --> seven(Weight, Code).

%! one// .
%! one(?Code:code)// .
%! one(?Weight:between(1,1), ?Code:code)// .

one --> one(_).
one(Code) --> one(_, Code).
one(1, 49) --> [49].

%! opening_angular_bracket// .
%! opening_angular_bracket(?Code:code)// .
%! opening_angular_bracket(?Type:oneof([angular]), ?Code:code)// .

opening_angular_bracket --> opening_angular_bracket(_).
opening_angular_bracket(Code) --> less_than_sign(Code).
opening_angular_bracket(angular, Code) --> opening_angular_bracket(Code).

%! ascii_opening_bracket// .
%! ascii_opening_bracket(?Code:code)// .
%! ascii_opening_bracket(
%!   ?Type:oneof([angular,curly,round,square]),
%!   ?Code:code
%! )// .

ascii_opening_bracket --> ascii_opening_bracket(_).
ascii_opening_bracket(Code) --> ascii_opening_bracket(_, Code).
ascii_opening_bracket(Type, Code) --> opening_angular_bracket(Type, Code).
ascii_opening_bracket(Type, Code) --> opening_curly_bracket(Type, Code).
ascii_opening_bracket(Type, Code) --> opening_round_bracket(Type, Code).
ascii_opening_bracket(Type, Code) --> opening_square_bracket(Type, Code).

%! opening_curly_bracket// .
%! opening_curly_bracket(?Code:code)// .
%! opening_curly_bracket(?Type:oneof([curly]), ?Code:code)// .

opening_curly_bracket --> opening_curly_bracket(_).
opening_curly_bracket(Code) --> opening_curly_bracket(_, Code).
opening_curly_bracket(curly, 123) --> [123].

%! opening_round_bracket// .
%! opening_round_bracket(?Code:code)// .
%! opening_round_bracket(?Type:oneof([round]), ?Code:code)// .

opening_round_bracket --> opening_round_bracket(_).
opening_round_bracket(Code) --> opening_round_bracket(_, Code).
opening_round_bracket(round, 40) --> [40].

%! opening_square_bracket// .
%! opening_square_bracket(?Code:code)// .
%! opening_square_bracket(?Type:oneof([square]), ?Code:code)// .

opening_square_bracket --> opening_square_bracket(_).
opening_square_bracket(Code) --> opening_square_bracket(_, Code).
opening_square_bracket(square, 91) --> [91].

%! p// .
%! p(?Code:code)// .
%! p(?Code:code, ?Index:between(1,26))// .

p --> p(_).
p(Code) --> p(Code, _).
p(Code, Index) --> p_uppercase(Code, Index).
p(Code, Index) --> p_lowercase(Code, Index).

%! p_lowercase// .
%! p_lowercase(?Code:code)// .
%! p_lowercase(?Code:code, ?Index:between(1,26))// .

p_lowercase --> p_lowercase(_).
p_lowercase(Code) --> p_lowercase(Code, _).
p_lowercase(112, 16) --> [112].

%! p_uppercase// .
%! p_uppercase(?Code:code)// .
%! p_uppercase(?Code:code, ?Index:between(1,26))// .

p_uppercase --> p_uppercase(_).
p_uppercase(Code) --> p_uppercase(Code, _).
p_uppercase(80, 16) --> [80].

%! parenthesis// .
%! parenthesis(?Code:code)// .

parenthesis --> parenthesis(_).
parenthesis(Code) --> round_bracket(Code).

%! percent_sign// .
%! percent_sign(?Code:code)// .

percent_sign --> percent_sign(_).
percent_sign(37) --> [37].

%! plus_sign// .
%! plus_sign(?Code:code)// .

plus_sign --> plus_sign(_).
plus_sign(43) --> [43].

%! positive_acknowledgement// .
%! positive_acknowledgement(?Code:code)// .

positive_acknowledgement --> positive_acknowledgement(_).
positive_acknowledgement(6) --> [6].

%! ascii_print// .
%! ascii_print(?Code:code)// .
% ASCII print characters are ASCII graphic characters and space.

ascii_print --> ascii_print(_).
ascii_print(Code) --> ascii_graphic(Code).
ascii_print(Code) --> space(Code).

%! ascii_punctuation// .
%! ascii_punctuation(?Code:code)// .
% ASCII punctuation characters.

ascii_punctuation --> ascii_punctuation(_).
ascii_punctuation(Code) --> ampersand(Code).
ascii_punctuation(Code) --> apostrophe(Code).
ascii_punctuation(Code) --> asterisk(Code).
ascii_punctuation(Code) --> at_sign(Code).
ascii_punctuation(Code) --> ascii_bracket(Code).
ascii_punctuation(Code) --> caret(Code).
ascii_punctuation(Code) --> colon(Code).
ascii_punctuation(Code) --> comma(Code).
ascii_punctuation(Code) --> dollar_sign(Code).
ascii_punctuation(Code) --> dot(Code).
ascii_punctuation(Code) --> double_quote(Code).
ascii_punctuation(Code) --> equals_sign(Code).
ascii_punctuation(Code) --> exclamation_mark(Code).
ascii_punctuation(Code) --> grave_accent(Code).
ascii_punctuation(Code) --> greater_than_sign(Code).
ascii_punctuation(Code) --> hyphen_minus(Code).
ascii_punctuation(Code) --> less_than_sign(Code).
ascii_punctuation(Code) --> number_sign(Code).
ascii_punctuation(Code) --> percent_sign(Code).
ascii_punctuation(Code) --> plus_sign(Code).
ascii_punctuation(Code) --> question_mark(Code).
ascii_punctuation(Code) --> semi_colon(Code).
ascii_punctuation(Code) --> slash(Code).
ascii_punctuation(Code) --> tilde(Code).
ascii_punctuation(Code) --> underscore(Code).
ascii_punctuation(Code) --> vertical_bar(Code).

%! q// .
%! q(?Code:code)// .
%! q(?Code:code, ?Index:between(1,26))// .

q --> q(_).
q(Code) --> q(Code, _).
q(Code, Index) --> q_uppercase(Code, Index).
q(Code, Index) --> q_lowercase(Code, Index).

%! q_lowercase// .
%! q_lowercase(?Code:code)// .
%! q_lowercase(?Code:code, ?Index:between(1,26))// .

q_lowercase --> q_lowercase(_).
q_lowercase(Code) --> q_lowercase(Code, _).
q_lowercase(113, 17) --> [113].

%! q_uppercase// .
%! q_uppercase(?Code:code)// .
%! q_uppercase(?Code:code, ?Index:between(1,26))// .

q_uppercase --> q_uppercase(_).
q_uppercase(Code) --> q_uppercase(Code, _).
q_uppercase(81, 17) --> [81].

%! question_mark// .
%! question_mark(?Code:code)// .

question_mark --> question_mark(_).
question_mark(63) --> [63].

%! r// .
%! r(?Code:code)// .
%! r(?Code:code, ?Index:between(1,26))// .

r --> r(_).
r(Code) --> r(Code, _).
r(Code, Index) --> r_uppercase(Code, Index).
r(Code, Index) --> r_lowercase(Code, Index).

%! r_lowercase// .
%! r_lowercase(?Code:code)// .
%! r_lowercase(?Code:code, ?Index:between(1,26))// .

r_lowercase --> r_lowercase(_).
r_lowercase(Code) --> r_lowercase(Code, _).
r_lowercase(114, 19) --> [114].

%! r_uppercase// .
%! r_uppercase(?Code:code)// .
%! r_uppercase(?Code:code, ?Index:between(1,26))// .

r_uppercase --> r_uppercase(_).
r_uppercase(Code) --> r_uppercase(Code, _).
r_uppercase(82, 19) --> [82].

%! record_separator// .
%! record_separator(?Code:code)// .

record_separator --> record_separator(_).
record_separator(30) --> [30].

%! round_bracket// .
%! round_bracket(?Code:code)// .
%! round_bracket(?Type:oneof([round]), ?Code:code)// .

round_bracket --> round_bracket(_).
round_bracket(Code) --> closing_round_bracket(Code).
round_bracket(Code) --> opening_round_bracket(Code).
round_bracket(round, Code) --> round_bracket(Code).

%! s// .
%! s(?Code:code)// .
%! s(?Code:code, ?Index:between(1,26))// .

s --> s(_).
s(Code) --> s(Code, _).
s(Code, Index) --> s_uppercase(Code, Index).
s(Code, Index) --> s_lowercase(Code, Index).

%! s_lowercase// .
%! s_lowercase(?Code:code)// .
%! s_lowercase(?Code:code, ?Index:between(1,26))// .

s_lowercase --> s_lowercase(_).
s_lowercase(Code) --> s_lowercase(Code, _).
s_lowercase(115, 19) --> [115].

%! s_uppercase// .
%! s_uppercase(?Code:code)// .
%! s_uppercase(?Code:code, ?Index:between(1,26))// .

s_uppercase --> s_uppercase(_).
s_uppercase(Code) --> s_uppercase(Code, _).
s_uppercase(83, 19) --> [83].

%! semi_colon// .
%! semi_colon(?Code:code)// .

semi_colon --> semi_colon(_).
semi_colon(59) --> [59].

%! seven// .
%! seven(?Code:code)// .
%! seven(?Weight:between(7,7), ?Code:code)// .

seven --> seven(_).
seven(Code) --> seven(_, Code).
seven(7, 55) --> [55].

%! shift// .
%! shift(?Code:code)// .
% Shift in and shift out are both **shift** keys.

shift --> shift(_).
shift(Code) --> shift_in(Code).
shift(Code) --> shift_out(Code).

%! shift_in// .
%! shift_in(?Code:code)// .

shift_in --> shift_in(_).
shift_in(15) --> [15].

%! shift_out// .
%! shift_out(?Code:code)// .

shift_out --> shift_out(_).
shift_out(14) --> [14].

%! single_quote// .
%! single_quote(?Code:code)// .
% Apostrophies are sometimes called **single quotes**.

single_quote --> single_quote(_).
single_quote(Code) --> apostrophe(Code).

%! six// .
%! six(?Code:code)// .
%! six(?Weight:between(6,6), ?Code:code)// .

six --> six(_).
six(Code) --> six(_, Code).
six(6, 54) --> [54].

%! slash// .
%! slash(?Code:code)// .
% Backslashes and forward slashes are both **slashes**.

slash --> slash(_).
slash(Code) --> backslash(Code).
slash(Code) --> forward_slash(Code).

%! soft_bracket// .
%! soft_bracket(?Code:code)// .
% Round brackets are sometimes called **round brackets**.

soft_bracket --> soft_bracket(_).
soft_bracket(Code) --> round_bracket(Code).

%! space// .
%! space(?Code:code)// .

space --> space(_).
space(32) --> [32].

%! square_bracket// .
%! square_bracket(?Code:code)// .
%! square_bracket(?Type:oneof([square]), ?Code:code)// .
% Opening and closing square brackets are both **square brackets**.

square_bracket --> square_bracket(_).
square_bracket(Code) --> square_bracket(_, Code).
square_bracket(Type, Code) --> closing_square_bracket(Type, Code).
square_bracket(Type, Code) --> opening_square_bracket(Type, Code).

%! start_of_heading// .
%! start_of_heading(?Code:code)// .

start_of_heading --> start_of_heading(_).
start_of_heading(1) --> [1].

%!  start_of_text// .
%!  start_of_text(?Code:code)// .

start_of_text --> start_of_text(_).
start_of_text(2) --> [2].

%! substitute// .
%! substitute(?Code:code)// .

substitute --> substitute(_).
substitute(26) --> [26].

%! synchronous_idle// .
%! synchronous_idle(?Code:code)// .

synchronous_idle --> synchronous_idle(_).
synchronous_idle(22) --> [22].

%! t// .
%! t(?Code:code)// .
%! t(?Code:code, ?Index:between(1,26))// .

t --> t(_).
t(Code) --> t(Code, _).
t(Code, Index) --> t_uppercase(Code, Index).
t(Code, Index) --> t_lowercase(Code, Index).

%! t_lowercase// .
%! t_lowercase(?Code:code)// .
%! t_lowercase(?Code:code, ?Index:between(1,26))// .

t_lowercase --> t_lowercase(_).
t_lowercase(Code) --> t_lowercase(Code, _).
t_lowercase(116, 20) --> [116].

%! t_uppercase// .
%! t_uppercase(?Code:code)// .
%! t_uppercase(?Code:code, ?Index:between(1,26))// .

t_uppercase --> t_uppercase(_).
t_uppercase(Code) --> t_uppercase(Code, _).
t_uppercase(84, 20) --> [84].

%! ascii_tab// .
%! ascii_tab(?Code:code)// .
% Horizontal and vertical tabs are both **tabs**.

ascii_tab --> ascii_tab(_).
ascii_tab(Code) --> horizontal_tab(Code).
ascii_tab(Code) --> vertical_tab(Code).

%! three// .
%! three(?Code:code)// .
%! three(?Weight:between(3,3), ?Code:code)// .

three --> three(_).
three(Code) --> three(_, Code).
three(3, 51) --> [51].

%! tilde// .
%! tilde(?Code:code)// .

tilde --> tilde(_).
tilde(126) --> [126].

%! two// .
%! two(?Code:code)// .
%! two(?Weight:between(2,2), ?Code:code)// .

two --> two(_).
two(Code) --> two(_, Code).
two(2, 50) --> [50].

%! u// .
%! u(?Code:code)// .
%! u(?Code:code, ?Index:between(1,26))// .

u --> u(_).
u(Code) --> u(Code, _).
u(Code, Index) --> u_uppercase(Code, Index).
u(Code, Index) --> u_lowercase(Code, Index).

%! u_lowercase// .
%! u_lowercase(?Code:code)// .
%! u_lowercase(?Code:code, ?Index:between(1,26))// .

u_lowercase --> u_lowercase(_).
u_lowercase(Code) --> u_lowercase(Code, _).
u_lowercase(117, 21) --> [117].

%! u_uppercase// .
%! u_uppercase(?Code:code)// .
%! u_uppercase(?Code:code, ?Index:between(1,26))// .

u_uppercase --> u_uppercase(_).
u_uppercase(Code) --> u_uppercase(Code, _).
u_uppercase(85, 21) --> [85].

%! underscore// .
%! underscore(?Code:code)// .

underscore --> underscore(_).
underscore(95) --> [95].

%! unit_separator// .
%! unit_separator(?Code:code)// .

unit_separator --> unit_separator(_).
unit_separator(31) --> [31].

%! v// .
%! v(?Code:code)// .
%! v(?Code:code, ?Index:between(1,26))// .

v --> v(_).
v(Code) --> v(Code, _).
v(Code, Index) --> v_uppercase(Code, Index).
v(Code, Index) --> v_lowercase(Code, Index).

%! v_lowercase// .
%! v_lowercase(?Code:code)// .
%! v_lowercase(?Code:code, ?Index:between(1,26))// .

v_lowercase --> v_lowercase(_).
v_lowercase(Code) --> v_lowercase(Code, _).
v_lowercase(118, 22) --> [118].

%! v_uppercase// .
%! v_uppercase(?Code:code)// .
%! v_uppercase(?Code:code, ?Index:between(1,26))// .

v_uppercase --> v_uppercase(_).
v_uppercase(Code) --> v_uppercase(Code, _).
v_uppercase(86, 22) --> [86].

%! vertical_bar// .
%! vertical_bar(?Code:code)// .

vertical_bar --> vertical_bar(_).
vertical_bar(124) --> [124].

%! vertical_tab// .
%! vertical_tab(?Code:code)// .

vertical_tab --> vertical_tab(_).
vertical_tab(11) --> [11].

%! w// .
%! w(?Code:code)// .
%! w(?Code:code, ?Index:between(1,26))// .

w --> w(_).
w(Code) --> w(Code, _).
w(Code, Index) --> w_uppercase(Code, Index).
w(Code, Index) --> w_lowercase(Code, Index).

%! w_lowercase// .
%! w_lowercase(?Code:code)// .
%! w_lowercase(?Code:code, ?Index:between(1,26))// .

w_lowercase --> w_lowercase(_).
w_lowercase(Code) --> w_lowercase(Code, _).
w_lowercase(119, 23) --> [119].

%! w_uppercase// .
%! w_uppercase(?Code:code)// .
%! w_uppercase(?Code:code, ?Index:between(1,26))// .

w_uppercase --> w_uppercase(_).
w_uppercase(Code) --> w_uppercase(Code, _).
w_uppercase(87, 23) --> [87].

%! ascii_white// .
%! ascii_white(?Code:code)// .
% ASCII **whites** are end-of-line, form feed, space and the two tabs.
%
% @compat http://en.wikipedia.org/wiki/Whitespace_character

ascii_white --> ascii_white(_).
ascii_white(Code) --> carriage_return(Code).
ascii_white(Code) --> form_feed(Code).
ascii_white(Code) --> line_feed(Code).
ascii_white(Code) --> space(Code).
ascii_white(Code) --> ascii_tab(Code).

%! x// .
%! x(?Code:code)// .
%! x(?Code:code, ?Index:between(1,26))// .

x --> x(_).
x(Code) --> x(Code, _).
x(Code, Index) --> x_uppercase(Code, Index).
x(Code, Index) --> x_lowercase(Code, Index).

%! x_lowercase// .
%! x_lowercase(?Code:code)// .
%! x_lowercase(?Code:code, ?Index:between(1,26))// .

x_lowercase --> x_lowercase(_).
x_lowercase(Code) --> x_lowercase(Code, _).
x_lowercase(120, 24) --> [120].

%! x_uppercase// .
%! x_uppercase(?Code:code)// .
%! x_uppercase(?Code:code, ?Index:between(1,26))// .

x_uppercase --> x_uppercase(_).
x_uppercase(Code) --> x_uppercase(Code, _).
x_uppercase(88, 24) --> [88].

%! y// .
%! y(?Code:code)// .
%! y(?Code:code, ?Index:between(1,26))// .

y --> y(_).
y(Code) --> y(Code, _).
y(Code, Index) --> y_uppercase(Code, Index).
y(Code, Index) --> y_lowercase(Code, Index).

%! y_lowercase// .
%! y_lowercase(?Code:code)// .
%! y_lowercase(?Code:code, ?Index:between(1,26))// .

y_lowercase --> y_lowercase(_).
y_lowercase(Code) --> y_lowercase(Code, _).
y_lowercase(121, 25) --> [121].

%! y_uppercase// .
%! y_uppercase(?Code:code)// .
%! y_uppercase(?Code:code, ?Index:between(1,26))// .

y_uppercase --> y_uppercase(_).
y_uppercase(Code) --> y_uppercase(Code, _).
y_uppercase(89, 25) --> [89].

%! z// .
%! z(?Code:code)// .
%! z(?Code:code, ?Index:between(1,26))// .

z --> z(_).
z(Code) --> z(Code, _).
z(Code, Index) --> z_uppercase(Code, Index).
z(Code, Index) --> z_lowercase(Code, Index).

%! z_lowercase// .
%! z_lowercase(?Code:code)// .
%! z_lowercase(?Code:code, ?Index:between(1,26))// .

z_lowercase --> z_lowercase(_).
z_lowercase(Code) --> z_lowercase(Code, _).
z_lowercase(122, 26) --> [122].

%! z_uppercase// .
%! z_uppercase(?Code:code)// .
%! z_uppercase(?Code:code, ?Index:between(1,26))// .

z_uppercase --> z_uppercase(_).
z_uppercase(Code) --> z_uppercase(Code, _).
z_uppercase(90, 26) --> [90].

%! zero// .
%! zero(?Code:code)// .
%! zero(?Weight:between(0,0), ?Code:code)// .

zero --> zero(_).
zero(Code) --> zero(_, Code).
zero(0, 48) --> [48].
