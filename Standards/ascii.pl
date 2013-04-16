:-
  module(
  ascii,
  [
    a/3,
    a_lowercase/3,
    a_uppercase/3,
    acknowledgement/3,
    alphanumeric/3,
    ampersand/3,
    any/3,
    apostrophe/3,
    asterisk/3,
    at_sign/3,
    b/3,
    b_lowercase/3,
    b_uppercase/3,
    backslash/3,
    backspace/3,
    bell/3,
    binary_digit/3,
    blank/3,
    bracket/3,
    c/3,
    c_lowercase/3,
    c_uppercase/3,
    cancel/3,
    caret/3,
    carriage_return/3,
    closing_bracket/3,
    closing_curly_bracket/3,
    closing_round_bracket/3,
    closing_square_bracket/3,
    colon/3,
    comma/3,
    control/3,
    curly_bracket/3,
    d/3,
    d_lowercase/3,
    d_uppercase/3,
    data_link_escape/3,
    delete/3,
    device_control/3,
    device_control_1/3,
    device_control_2/3,
    device_control_3/3,
    device_control_4/3,
    decimal_digit/3,
    digit/3,
    dollar_sign/3,
    dot/3,
    double_quote/3,
    e/3,
    e_lowercase/3,
    e_uppercase/3,
    eight/3,
    enquiry/3,
    end_of_line/3,
    end_of_medium/3,
    end_of_text/3,
    end_of_transmission/3,
    end_of_transmission_block/3,
    end_of_word/3,
    equals_sign/3,
    escape/3,
    exclamation_mark/3,
    exponent_sign/3,
    f/3,
    f_lowercase/3,
    f_uppercase/3,
    file_separator/3,
    five/3,
    form_feed/3,
    forward_slash/3,
    four/3,
    g/3,
    g_lowercase/3,
    g_uppercase/3,
    graph/3,
    grave_accent/3,
    greater_than_sign/3,
    group_separator/3,
    h/3,
    h_lowercase/3,
    h_uppercase/3,
    hexadecimal_digit/3,
    horizontal_tab/3,
    hyphen/3,
    hyphen_minus/3,
    i/3,
    i_lowercase/3,
    i_uppercase/3,
    j/3,
    j_lowercase/3,
    j_uppercase/3,
    k/3,
    k_lowercase/3,
    k_uppercase/3,
    l/3,
    l_lowercase/3,
    l_uppercase/3,
    line_feed/3,
    less_than_sign/3,
    letter/3,
    letter_lowercase/3,
    letter_uppercase/3,
    m/3,
    m_lowercase/3,
    m_uppercase/3,
    minus_sign/3,
    n/3,
    n_lowercase/3,
    n_uppercase/3,
    negative_acknowledgement/3,
    nine/3,
    null/3,
    number_sign/3,
    o/3,
    o_lowercase/3,
    o_uppercase/3,
    one/3,
    octal_digit/3,
    opening_bracket/3,
    opening_curly_bracket/3,
    opening_round_bracket/3,
    opening_square_bracket/3,
    p/3,
    p_lowercase/3,
    p_uppercase/3,
    percent_sign/3,
    plus_sign/3,
    positive_acknowledgement/3,
    print/3,
    punctuation/3,
    q/3,
    q_lowercase/3,
    q_uppercase/3,
    question_mark/3,
    r/3,
    r_lowercase/3,
    r_uppercase/3,
    record_separator/3,
    round_bracket/3,
    s/3,
    s_lowercase/3,
    s_uppercase/3,
    semi_colon/3,
    seven/3,
    shift/3,
    shift_in/3,
    shift_out/3,
    sign/3,
    six/3,
    slash/3,
    space/3,
    square_bracket/3,
    start_of_heading/3,
    start_of_text/3,
    substitute/3,
    synchronous_idle/3,
    t/3,
    t_lowercase/3,
    t_uppercase/3,
    tab/3,
    three/3,
    tilde/3,
    two/3,
    u/3,
    u_lowercase/3,
    u_uppercase/3,
    underscore/3,
    unit_separator/3,
    v/3,
    v_lowercase/3,
    v_uppercase/3,
    vertical_bar/3,
    vertical_tab/3,
    w/3,
    w_lowercase/3,
    w_uppercase/3,
    white/3,
    word/3,
    x/3,
    x_lowercase/3,
    x_uppercase/3,
    y/3,
    y_lowercase/3,
    y_uppercase/3,
    z/3,
    z_lowercase/3,
    z_uppercase/3,
    zero/3
  ]
).

/** <module> ASCII

DCG rules that encode the ASCII standard.

@author Wouter Beek
@version 2013/01-2013/02
*/



a(O1, R1-R0, C1-C0):-
  letter(a, O1, R1-R0, C1-C0).

a_lowercase(_O1, [97 | R0]-R0, [97 | C0]-C0).

a_uppercase(_O1, [65 | R0]-R0, [65 | C0]-C0).

acknowledgement(O1, R1-R0, C1-C0):-
  negative_acknowledgement(O1, R1-R0, C1-C0).
acknowledgement(O1, R1-R0, C1-C0):-
  positive_acknowledgement(O1, R1-R0, C1-C0).

alphanumeric(O1, R1-R0, C1-C0):-
  letter(O1, R1-R0, C1-C0).
alphanumeric(O1, R1-R0, C1-C0):-
  digit(O1, R1-R0, C1-C0).

ampersand(_O1, [38 | R0]-R0, [38 | C0]-C0).

% We only consider per-code replacements.
any(O1, [Y | R0]-R0, [X | C0]-C0):-
  option(replace(From,To), O1),
  call(From, O1, [X | R0]-R0, [X | C0], C0),
  call(To,   O1, [Y | R0]-R0, [Y | C0], C0),
  !.
% Codes > 127 are non-ASCII. We have not created DCGs for these codes yet.
any(_O1, [X | R0]-R0, [X | C0]-C0):-
  X > 127,
  !.
any(O1, R1-R0, C1-C0):-
  control(O1, R1-R0, C1-C0).
any(O1, R1-R0, C1-C0):-
  graph(O1, R1-R0, C1-C0).
any(O1, R1-R0, C1-C0):-
  white(O1, R1-R0, C1-C0).

% XML: &apos;
apostrophe(O1, [38, 97, 112, 111, 115, 59 | R0]-R0, [39 | C0]-C0):-
  option(lang(xml), O1),
  !.
apostrophe(_O1, [39 | R0]-R0, [39 | C0]-C0).

asterisk(_O1, [42 | R0]-R0, [42 | C0]-C0).

at_sign(_O1, [65 | R0]-R0, [64 | C0]-C0).

b(O1, R1-R0, C1-C0):-
  \+ option(case(upper), O1),
  b_lowercase(O1, R1-R0, C1-C0).
b(O1, [X | R0]-R0, C1-C0):-
  (option(case(insensitive), O1) ; option(case(upper), O1)),
  b_lowercase(O1, [Y | R0]-R0, C1-C0),
  X is Y - 32.
b(O1, R1-R0, C1-C0):-
  \+ option(case(lower), O1),
  b_uppercase(O1, R1-R0, C1-C0).
b(O1, [X | R0]-R0, C1-C0):-
  (option(case(insensitive), O1) ; option(case(lower), O1)),
  b_uppercase(O1, [Y | R0]-R0, C1-C0),
  X is Y + 32.

b(O1, R1-R0, C1-C0):-
  letter(b, O1, R1-R0, C1-C0).

b_lowercase(_O1, [98 | R0]-R0, [98 | C0]-C0).

b_uppercase(_O1, [66 | R0]-R0, [66 | C0]-C0).

backslash(_O1, [92 | R0]-R0, [92 | C0]-C0).

backspace(_O1, [8 | R0]-R0, [8 | C0]-C0).

bell(O1, [92, 98 | R0]-R0, [7 | C0]-C0):-
  option(lang(c), O1),
  !.
bell(_O1, [7 | R0]-R0, [7 | C0]-C0).

binary_digit(O1, R1-R0, C1-C0):-
  zero(O1, R1-R0, C1-C0).
binary_digit(O1, R1-R0, C1-C0):-
  one(O1, R1-R0, C1-C0).

blank(O1, R1-R0, C1-C0):-
  space(O1, R1-R0, C1-C0).
blank(O1, R1-R0, C1-C0):-
  horizontal_tab(O1, R1-R0, C1-C0).

bracket(O1, R1-R0, C1-C0):-
  closing_bracket(O1, R1-R0, C1-C0).
bracket(O1, R1-R0, C1-C0):-
  opening_bracket(O1, R1-R0, C1-C0).

c(O1, R1-R0, C1-C0):-
  letter(c, O1, R1-R0, C1-C0).

c_lowercase(_O1, [99 | R0]-R0, [99 | C0]-C0).

c_uppercase(_O1, [67 | R0]-R0, [67 | C0]-C0).

cancel(_O1, [24 | R0]-R0, [24 | C0]-C0).

caret(_O1, [94 | R0]-R0, [94 | C0]-C0).

carriage_return(_O1, [13 | R0]-R0, [13 | C0]-C0).

closing_bracket(O1, R1-R0, C1-C0):-
  closing_curly_bracket(O1, R1-R0, C1-C0).
closing_bracket(O1, R1-R0, C1-C0):-
  closing_round_bracket(O1, R1-R0, C1-C0).
closing_bracket(O1, R1-R0, C1-C0):-
  closing_square_bracket(O1, R1-R0, C1-C0).

closing_curly_bracket(_O1, [125 | R0]-R0, [125 | C0]-C0).

closing_round_bracket(_O1, [41 | R0]-R0, [41 | C0]-C0).

closing_square_bracket(_O1, [93 | R0]-R0, [93 | C0]-C0).

colon(_O1, [58 | R0]-R0, [58 | C0]-C0).

comma(_O1, [44 | R0]-R0, [44 | C0]-C0).

control(O1, R1-R0, C1-C0):-
  acknowledgement(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  backspace(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  bell(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  cancel(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  carriage_return(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  data_link_escape(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  delete(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  device_control(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  enquiry(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  end_of_medium(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  end_of_text(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  end_of_transmission(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  end_of_transmission_block(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  escape(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  file_separator(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  form_feed(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  group_separator(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  line_feed(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  null(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  record_separator(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  shift(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  start_of_heading(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  start_of_text(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  substitute(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  synchronous_idle(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  tab(O1, R1-R0, C1-C0).
control(O1, R1-R0, C1-C0):-
  unit_separator(O1, R1-R0, C1-C0).

curly_bracket(O1, R1-R0, C1-C0):-
  closing_curly_bracket(O1, R1-R0, C1-C0).
curly_bracket(O1, R1-R0, C1-C0):-
  opening_curly_bracket(O1, R1-R0, C1-C0).

d(O1, R1-R0, C1-C0):-
  letter(d, O1, R1-R0, C1-C0).

d_lowercase(_O1, [100 | R0]-R0, [100 | C0]-C0).

d_uppercase(_O1, [68 | R0]-R0, [68 | C0]-C0).

data_link_escape(_O1, [16 | R0]-R0, [16 | C0]-C0).

delete(_O1, [127 | R0]-R0, [127 | C0]-C0).

device_control(O1, R1-R0, C1-C0):-
  device_control_1(O1, R1-R0, C1-C0).
device_control(O1, R1-R0, C1-C0):-
  device_control_2(O1, R1-R0, C1-C0).
device_control(O1, R1-R0, C1-C0):-
  device_control_3(O1, R1-R0, C1-C0).
device_control(O1, R1-R0, C1-C0):-
  device_control_4(O1, R1-R0, C1-C0).

device_control_1(_O1, [17 | R0]-R0, [17 | C0]-C0).

device_control_2(_O1, [18 | R0]-R0, [18 | C0]-C0).

device_control_3(_O1, [19 | R0]-R0, [19 | C0]-C0).

device_control_4(_O1, [20 | R0]-R0, [20 | C0]-C0).

decimal_digit(O1, R1-R0, C1-C0):-
  octal_digit(O1, R1-R0, C1-C0).
decimal_digit(O1, R1-R0, C1-C0):-
  eight(O1, R1-R0, C1-C0).
decimal_digit(O1, R1-R0, C1-C0):-
  nine(O1, R1-R0, C1-C0).

digit(O1, R1-R0, C1-C0):-
  decimal_digit(O1, R1-R0, C1-C0).
digit(O1, R1-R0, C1-C0):-
  hexadecimal_digit(O1, R1-R0, C1-C0).
digit(O1, R1-R0, C1-C0):-
  octal_digit(O1, R1-R0, C1-C0).

dollar_sign(_O1, [36 | R0]-R0, [36 | C0]-C0).

dot(_O1, [46 | R0]-R0, [46 | C0]-C0).

% XML: &quot;
double_quote(O1, [38, 113, 117, 111, 116, 59 | R0]-R0, [34 | C0]-C0):-
  option(lang(xml), O1),
  !.
double_quote(_O1, [34 | R0]-R0, [34 | C0]-C0).

e(O1, R1-R0, C1-C0):-
  letter(e, O1, R1-R0, C1-C0).

e_lowercase(_O1, [101 | R0]-R0, [101 | C0]-C0).

e_uppercase(_O1, [69 | R0]-R0, [69 | C0]-C0).

eight(_O1, [56 | R0]-R0, [56 | C0]-C0).

enquiry(_O1, [5 | R0]-R0, [5 | C0]-C0).

end_of_line(O1, R1-R0, C1-C0):-
  carriage_return(O1, R1-R0, C1-C0).
end_of_line(O1, R1-R0, C1-C0):-
  end_of_medium(O1, R1-R0, C1-C0).
end_of_line(O1, R1-R0, C1-C0):-
  end_of_text(O1, R1-R0, C1-C0).
end_of_line(O1, R1-R0, C1-C0):-
  end_of_transmission(O1, R1-R0, C1-C0).
end_of_line(O1, R1-R0, C1-C0):-
  end_of_transmission_block(O1, R1-R0, C1-C0).
end_of_line(O1, R1-R0, C1-C0):-
  line_feed(O1, R1-R0, C1-C0).

end_of_medium(_O1, [25 | R0]-R0, [25 | C0]-C0).

end_of_text(_O1, [3 | R0]-R0, [3 | C0]-C0).

end_of_transmission(_O1, [4 | R0]-R0, [4 | C0]-C0).

end_of_transmission_block(_O1, [23 | R0]-R0, [23 | C0]-C0).

end_of_word(O1, R1-R0, C1-C0):-
  end_of_line(O1, R1-R0, C1-C0).
end_of_word(O1, R1-R0, C1-C0):-
  punctuation(O1, R1-R0, C1-C0).
end_of_word(O1, R1-R0, C1-C0):-
  white(O1, R1-R0, C1-C0).

equals_sign(_O1, [61 | R0]-R0, [61 | C0]-C0).

escape(_O1, [27 | R0]-R0, [27 | C0]-C0).

exclamation_mark(_O1, [33 | R0]-R0, [33 | C0]-C0).

exponent_sign(O1, R1-R0, C1-C0):-
  e(O1, R1-R0, C1-C0).

f(O1, R1-R0, C1-C0):-
  letter(f, O1, R1-R0, C1-C0).

f_lowercase(_O1, [102 | R0]-R0, [102 | C0]-C0).

f_uppercase(_O1, [70 | R0]-R0, [70 | C0]-C0).

file_separator(_O1, [28 | R0]-R0, [28 | C0]-C0).

five(_O1, [53 | R0]-R0, [53 | C0]-C0).

form_feed(_O1, [12 | R0]-R0, [12 | C0]-C0).

forward_slash(_O1, [47 | R0]-R0, [47 | C0]-C0).

four(_O1, [52 | R0]-R0, [52 | C0]-C0).

g(O1, R1-R0, C1-C0):-
  letter(g, O1, R1-R0, C1-C0).

g_lowercase(_O1, [103 | R0]-R0, [103 | C0]-C0).

g_uppercase(_O1, [71 | R0]-R0, [71 | C0]-C0).

graph(O1, R1-R0, C1-C0):-
  alphanumeric(O1, R1-R0, C1-C0).
graph(O1, R1-R0, C1-C0):-
  punctuation(O1, R1-R0, C1-C0).

grave_accent(_O1, [96 | R0]-R0, [96 | C0]-C0).

% XML: &gt;
greater_than_sign(O1, [38, 103, 116, 59 | R0]-R0, [62 | C0]-C0):-
  option(lang(xml), O1),
  !.
greater_than_sign(_O1, [62 | R0]-R0, [62 | C0]-C0).

group_separator(_O1, [29 | R0]-R0, [29 | C0]-C0).

h(O1, R1-R0, C1-C0):-
  letter(h, O1, R1-R0, C1-C0).

h_lowercase(_O1, [104 | R0]-R0, [104 | C0]-C0).

h_uppercase(_O1, [72 | R0]-R0, [72 | C0]-C0).

hexadecimal_digit(O1, R1-R0, C1-C0):-
  decimal_digit(O1, R1-R0, C1-C0).
hexadecimal_digit(O1, R1-R0, C1-C0):-
  a(O1, R1-R0, C1-C0).
hexadecimal_digit(O1, R1-R0, C1-C0):-
  b(O1, R1-R0, C1-C0).
hexadecimal_digit(O1, R1-R0, C1-C0):-
  c(O1, R1-R0, C1-C0).
hexadecimal_digit(O1, R1-R0, C1-C0):-
  d(O1, R1-R0, C1-C0).
hexadecimal_digit(O1, R1-R0, C1-C0):-
  e(O1, R1-R0, C1-C0).
hexadecimal_digit(O1, R1-R0, C1-C0):-
  f(O1, R1-R0, C1-C0).

horizontal_tab(O1, [92, 116 | R0]-R0, [9 | C0]-C0):-
  option(lang(c), O1),
  !.
horizontal_tab(_O1, [9 | R0]-R0, [9 | C0]-C0).

hyphen(O1, R1-R0, C1-C0):-
  hyphen_minus(O1, R1-R0, C1-C0).

hyphen_minus(_O1, [45 | R0]-R0, [45 | C0]-C0).

i(O1, R1-R0, C1-C0):-
  letter(i, O1, R1-R0, C1-C0).

i_lowercase(_O1, [105 | R0]-R0, [105 | C0]-C0).

i_uppercase(_O1, [73 | R0]-R0, [73 | C0]-C0).

j(O1, R1-R0, C1-C0):-
  letter(j, O1, R1-R0, C1-C0).

j_lowercase(_O1, [106 | R0]-R0, [106 | C0]-C0).

j_uppercase(_O1, [74 | R0]-R0, [74 | C0]-C0).

k(O1, R1-R0, C1-C0):-
  letter(k, O1, R1-R0, C1-C0).

k_lowercase(_O1, [107 | R0]-R0, [107 | C0]-C0).

k_uppercase(_O1, [75 | R0]-R0, [75 | C0]-C0).

l(O1, R1-R0, C1-C0):-
  letter(l, O1, R1-R0, C1-C0).

l_lowercase(_O1, [108 | R0]-R0, [108 | C0]-C0).

l_uppercase(_O1, [76 | R0]-R0, [76 | C0]-C0).

line_feed(O1, [92, 110 | R0]-R0, [10 | C0]-C0):-
  option(lang(c), O1),
  !.
line_feed(_O1, [10 | R0]-R0, [10 | C0]-C0).

% XML: &lt;
less_than_sign(O1, [38, 108, 116, 59 | R0]-R0, [60 | C0]-C0):-
  option(lang(xml), O1),
  !.
less_than_sign(_O1, [60 | R0]-R0, [60 | C0]-C0).

% Case sensitive, case insensitive and lowercase parse this.
% Uppercase cannot return an lowercase result.
letter(O1, R1-R0, C1-C0):-
  \+ option(case(upper), O1),
  letter_lowercase(O1, R1-R0, C1-C0).
% Only case insesitive and uppercase parse this, since they accept/require
% the uppercase variant of the lowercase input.
letter(O1, [X | R0]-R0, C1-C0):-
  (option(case(insensitive), O1) ; option(case(upper), O1)),
  letter_lowercase(O1, [Y | R0]-R0, C1-C0),
  X is Y - 32.
% Case sensitive, case insensitive and lowercase parse this.
% Lowercase cannot return an uppercase result.
letter(O1, R1-R0, C1-C0):-
  \+ option(case(lower), O1),
  letter_uppercase(O1, R1-R0, C1-C0).
% Only case insesitive and lowercase parse this, since they accept/require
% the lowerrcase variant of the uppercase input.
letter(O1, [X | R0]-R0, C1-C0):-
  (option(case(insensitive), O1) ; option(case(lower), O1)),
  letter_uppercase(O1, [Y | R0]-R0, C1-C0),
  X is Y + 32.

%% letter(
%%   +Letter:oneof([a-z]),
%%   +Options:list(nvpair),
%%   ?R:diffpair,
%%   ?C:diffpair
%% ) is nondet.
% This is the generic method for letters. It uses the option =|case/1|= to
% determine which uppercase and/or lowercase variants of a letter are to be
% read and/or written.

% A lowercase letter is read and written.
% A result for options =|case(insensitive)|=, =|case(lower)|=, and
% =|case(sensitive)|=.
letter(Letter, O1, R1-R0, C1-C0):-
  \+ option(case(upper), O1),
  format(atom(P), '~w_lowercase', [Letter]),
  call(P, O1, R1-R0, C1-C0).
% A lowercase letter is read and its uppercase variant is written.
% A result for options =|case(insensitive)|= and =|case(upper)|=.
letter(Letter, O1, [X | R0]-R0, C1-C0):-
  (option(case(insensitive), O1) ; option(case(upper), O1)),
  format(atom(P), '~w_lowercase', [Letter]),
  call(P, O1, [Y | R0]-R0, C1-C0),
  X is Y - 32.
% An uppercase letter is read and written.
% A result for options =|case(insensitive)|=, =|case(sensitive)|= and
% =|case(upper)|=.
letter(Letter, O1, R1-R0, C1-C0):-
  \+ option(case(lower), O1),
  format(atom(P), '~w_uppercase', [Letter]),
  call(P, O1, R1-R0, C1-C0).
% An uppercase letter is read and its lowercase variant is written.
% A result for options =|case(insensitive)|= and =|case(lower)|=.
letter(Letter, O1, [X | R0]-R0, C1-C0):-
  (option(case(insensitive), O1) ; option(case(lower), O1)),
  format(atom(P), '~w_uppercase', [Letter]),
  call(P, O1, [Y | R0]-R0, C1-C0),
  X is Y + 32.

letter_lowercase(O1, R1-R0, C1-C0):-
  a_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  b_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  c_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  d_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  e_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  f_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  g_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  h_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  i_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  j_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  k_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  l_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  m_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  n_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  o_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  p_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  q_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  r_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  s_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  t_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  u_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  v_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  w_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  x_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  y_lowercase(O1, R1-R0, C1-C0).
letter_lowercase(O1, R1-R0, C1-C0):-
  z_lowercase(O1, R1-R0, C1-C0).

letter_uppercase(O1, R1-R0, C1-C0):-
  a_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  b_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  c_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  d_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  e_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  g_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  h_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  i_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  j_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  k_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  l_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  m_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  n_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  o_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  p_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  q_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  r_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  s_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  t_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  u_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  v_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  w_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  x_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  y_uppercase(O1, R1-R0, C1-C0).
letter_uppercase(O1, R1-R0, C1-C0):-
  z_uppercase(O1, R1-R0, C1-C0).

m(O1, R1-R0, C1-C0):-
  letter(m, O1, R1-R0, C1-C0).

m_lowercase(_O1, [109 | R0]-R0, [109 | C0]-C0).

m_uppercase(_O1, [77 | R0]-R0, [77 | C0]-C0).

minus_sign(O1, R1-R0, C1-C0):-
  hyphen_minus(O1, R1-R0, C1-C0).

n(O1, R1-R0, C1-C0):-
  letter(n, O1, R1-R0, C1-C0).

n_lowercase(_O1, [110 | R0]-R0, [110 | C0]-C0).

n_uppercase(_O1, [78 | R0]-R0, [78 | C0]-C0).

negative_acknowledgement(_O1, [21 | R0]-R0, [21 | C0]-C0).

nine(_O1, [57 | R0]-R0, [57 | C0]-C0).

null(_O1, [0 | R0]-R0, [0 | C0]-C0).

number_sign(_O1, [35 | R0]-R0, [35 | C0]-C0).

o(O1, R1-R0, C1-C0):-
  letter(o, O1, R1-R0, C1-C0).

o_lowercase(_O1, [111 | R0]-R0, [111 | C0]-C0).

o_uppercase(_O1, [79 | R0]-R0, [79 | C0]-C0).

one(_O1, [49 | R0]-R0, [49 | C0]-C0).

octal_digit(O1, R1-R0, C1-C0):-
  binary_digit(O1, R1-R0, C1-C0).
octal_digit(O1, R1-R0, C1-C0):-
  two(O1, R1-R0, C1-C0).
octal_digit(O1, R1-R0, C1-C0):-
  three(O1, R1-R0, C1-C0).
octal_digit(O1, R1-R0, C1-C0):-
  four(O1, R1-R0, C1-C0).
octal_digit(O1, R1-R0, C1-C0):-
  five(O1, R1-R0, C1-C0).
octal_digit(O1, R1-R0, C1-C0):-
  six(O1, R1-R0, C1-C0).
octal_digit(O1, R1-R0, C1-C0):-
  seven(O1, R1-R0, C1-C0).

opening_bracket(O1, R1-R0, C1-C0):-
  opening_curly_bracket(O1, R1-R0, C1-C0).
opening_bracket(O1, R1-R0, C1-C0):-
  opening_round_bracket(O1, R1-R0, C1-C0).
opening_bracket(O1, R1-R0, C1-C0):-
  opening_square_bracket(O1, R1-R0, C1-C0).

opening_curly_bracket(_O1, [123 | R0]-R0, [123 | C0]-C0).

opening_round_bracket(_O1, [40 | R0]-R0, [40 | C0]-C0).

opening_square_bracket(_O1, [91 | R0]-R0, [91 | C0]-C0).

p(O1, R1-R0, C1-C0):-
  letter(p, O1, R1-R0, C1-C0).

p_lowercase(_O1, [112 | R0]-R0, [112 | C0]-C0).

p_uppercase(_O1, [80 | R0]-R0, [80 | C0]-C0).

percent_sign(_O1, [37 | R0]-R0, [37 | C0]-C0).

plus_sign(_O1, [43 | R0]-R0, [43 | C0]-C0).

positive_acknowledgement(_O1, [6 | R0]-R0, [6 | C0]-C0).

print(O1, R1-R0, C1-C0):-
  graph(O1, R1-R0, C1-C0).
print(O1, R1-R0, C1-C0):-
  space(O1, R1-R0, C1-C0).

punctuation(O1, R1-R0, C1-C0):-
  ampersand(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  apostrophe(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  asterisk(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  at_sign(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  bracket(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  caret(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  colon(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  comma(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  dollar_sign(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  dot(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  double_quote(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  equals_sign(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  exclamation_mark(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  grave_accent(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  greater_than_sign(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  hyphen_minus(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  less_than_sign(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  number_sign(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  percent_sign(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  plus_sign(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  question_mark(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  semi_colon(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  slash(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  tilde(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  underscore(O1, R1-R0, C1-C0).
punctuation(O1, R1-R0, C1-C0):-
  vertical_bar(O1, R1-R0, C1-C0).

q(O1, R1-R0, C1-C0):-
  letter(q, O1, R1-R0, C1-C0).

q_lowercase(_O1, [113 | R0]-R0, [113 | C0]-C0).

q_uppercase(_O1, [81 | R0]-R0, [81 | C0]-C0).

question_mark(_O1, [63 | R0]-R0, [63 | C0]-C0).

r(O1, R1-R0, C1-C0):-
  letter(r, O1, R1-R0, C1-C0).

r_lowercase(_O1, [114 | R0]-R0, [114 | C0]-C0).

r_uppercase(_O1, [82 | R0]-R0, [82 | C0]-C0).

record_separator(_O1, [30 | R0]-R0, [30 | C0]-C0).

round_bracket(O1, R1-R0, C1-C0):-
  closing_round_bracket(O1, R1-R0, C1-C0).
round_bracket(O1, R1-R0, C1-C0):-
  opening_round_bracket(O1, R1-R0, C1-C0).

s(O1, R1-R0, C1-C0):-
  letter(s, O1, R1-R0, C1-C0).

s_lowercase(_O1, [115 | R0]-R0, [115 | C0]-C0).

s_uppercase(_O1, [83 | R0]-R0, [83 | C0]-C0).

semi_colon(_O1, [59 | R0]-R0, [59 | C0]-C0).

seven(_O1, [55 | R0]-R0, [55 | C0]-C0).

shift(O1, R1-R0, C1-C0):-
  shift_in(O1, R1-R0, C1-C0).
shift(O1, R1-R0, C1-C0):-
  shift_out(O1, R1-R0, C1-C0).

shift_in(_O1, [15 | R0]-R0, [15 | C0]-C0).

shift_out(_O1, [14 | R0]-R0, [14 | C0]-C0).

sign(O1, R1-R0, C1-C0):-
  minus_sign(O1, R1-R0, C1-C0).
sign(O1, R1-R0, C1-C0):-
  plus_sign(O1, R1-R0, C1-C0).

single_quote(O1, R1-R0, C1-C0):-
  apostrophe(O1, R1-R0, C1-C0).

six(_O1, [54 | R0]-R0, [54 | C0]-C0).

slash(O1, R1-R0, C1-C0):-
  backslash(O1, R1-R0, C1-C0).
slash(O1, R1-R0, C1-C0):-
  forward_slash(O1, R1-R0, C1-C0).

space(_O1, [32 | R0]-R0, [32 | C0]-C0).

square_bracket(O1, R1-R0, C1-C0):-
  closing_square_bracket(O1, R1-R0, C1-C0).
square_bracket(O1, R1-R0, C1-C0):-
  opening_square_bracket(O1, R1-R0, C1-C0).

start_of_heading(_O1, [1 | R0]-R0, [1 | C0]-C0).

start_of_text(_O1, [2 | R0]-R0, [2 | C0]-C0).

substitute(_O1, [26 | R0]-R0, [26 | C0]-C0).

synchronous_idle(_O1, [22 | R0]-R0, [22 | C0]-C0).

t(O1, R1-R0, C1-C0):-
  letter(t, O1, R1-R0, C1-C0).

t_lowercase(_O1, [116 | R0]-R0, [116 | C0]-C0).

t_uppercase(_O1, [84 | R0]-R0, [84 | C0]-C0).

tab(O1, R1-R0, C1-C0):-
  horizontal_tab(O1, R1-R0, C1-C0).
tab(O1, R1-R0, C1-C0):-
  vertical_tab(O1, R1-R0, C1-C0).

three(_O1, [51 | R0]-R0, [51 | C0]-C0).

tilde(_O1, [126 | R0]-R0, [126 | C0]-C0).

two(_O1, [50 | R0]-R0, [50 | C0]-C0).

u(O1, R1-R0, C1-C0):-
  letter(u, O1, R1-R0, C1-C0).

u_lowercase(_O1, [117 | R0]-R0, [117 | C0]-C0).

u_uppercase(_O1, [85 | R0]-R0, [85 | C0]-C0).

underscore(_O1, [95 | R0]-R0, [95 | C0]-C0).

unit_separator(_O1, [31 | R0]-R0, [31 | C0]-C0).

v(O1, R1-R0, C1-C0):-
  letter(v, O1, R1-R0, C1-C0).

v_lowercase(_O1, [118 | R0]-R0, [118 | C0]-C0).

v_uppercase(_O1, [86 | R0]-R0, [86 | C0]-C0).

vertical_bar(_O1, [124 | R0]-R0, [124 | C0]-C0).

vertical_tab(_O1, [11 | R0]-R0, [11 | C0]-C0).

w(O1, R1-R0, C1-C0):-
  letter(w, O1, R1-R0, C1-C0).

w_lowercase(_O1, [119 | R0]-R0, [119 | C0]-C0).

w_uppercase(_O1, [87 | R0]-R0, [87 | C0]-C0).

white(O1, R1-R0, C1-C0):-
  end_of_line(O1, R1-R0, C1-C0).
white(O1, R1-R0, C1-C0):-
  form_feed(O1, R1-R0, C1-C0).
white(O1, R1-R0, C1-C0):-
  space(O1, R1-R0, C1-C0).
white(O1, R1-R0, C1-C0):-
  tab(O1, R1-R0, C1-C0).

% Word consist of alphanumeric character and underscores.
word(O1, R1-R0, C1-C0):-
  alphanumeric(O1, R1-R0, C1-C0).
word(O1, R1-R0, C1-C0):-
  underscore(O1, R1-R0, C1-C0).

x(O1, R1-R0, C1-C0):-
  letter(x, O1, R1-R0, C1-C0).

x_lowercase(_O1, [120 | R0]-R0, [120 | C0]-C0).

x_uppercase(_O1, [88 | R0]-R0, [88 | C0]-C0).

y(O1, R1-R0, C1-C0):-
  letter(y, O1, R1-R0, C1-C0).

y_lowercase(_O1, [121 | R0]-R0, [121 | C0]-C0).

y_uppercase(_O1, [89 | R0]-R0, [89 | C0]-C0).

z(O1, R1-R0, C1-C0):-
  letter(z, O1, R1-R0, C1-C0).

z_lowercase(_O1, [122 | R0]-R0, [122 | C0]-C0).

z_uppercase(_O1, [90 | R0]-R0, [90 | C0]-C0).

zero(_O1, [48 | R0]-R0, [48 | C0]-C0).

