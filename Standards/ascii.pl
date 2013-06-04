:- module(
  ascii,
  [
    ascii_a/3,
    ascii_a_lowercase/3,
    ascii_a_uppercase/3,
    ascii_acknowledgement/3,
    ascii_alphanumeric/3,
    ascii_ampersand/3,
    ascii_any/3,
    ascii_apostrophe/3,
    ascii_decimal_digit/3,
    ascii_digit/3,
    ascii_asterisk/3,
    ascii_at_sign/3,
    ascii_b/3,
    ascii_b_lowercase/3,
    ascii_b_uppercase/3,
    ascii_backslash/3,
    ascii_backspace/3,
    ascii_bell/3,
    ascii_binary_digit/3,
    ascii_blank/3,
    ascii_bracket/3,
    ascii_c/3,
    ascii_c_lowercase/3,
    ascii_c_uppercase/3,
    ascii_cancel/3,
    ascii_caret/3,
    ascii_carriage_return/3,
    ascii_closing_bracket/3,
    ascii_closing_curly_bracket/3,
    ascii_closing_round_bracket/3,
    ascii_closing_square_bracket/3,
    ascii_colon/3,
    ascii_comma/3,
    ascii_control/3,
    ascii_curly_bracket/3,
    ascii_d/3,
    ascii_d_lowercase/3,
    ascii_d_uppercase/3,
    ascii_data_link_escape/3,
    ascii_delete/3,
    ascii_device_control/3,
    ascii_device_control_1/3,
    ascii_device_control_2/3,
    ascii_device_control_3/3,
    ascii_device_control_4/3,
    ascii_dollar_sign/3,
    ascii_dot/3,
    ascii_double_quote/3,
    ascii_e/3,
    ascii_e_lowercase/3,
    ascii_e_uppercase/3,
    ascii_eight/3,
    ascii_enquiry/3,
    ascii_end_of_line/3,
    ascii_end_of_medium/3,
    ascii_end_of_text/3,
    ascii_end_of_transmission/3,
    ascii_end_of_transmission_block/3,
    ascii_end_of_word/3,
    ascii_equals_sign/3,
    ascii_escape/3,
    ascii_exclamation_mark/3,
    ascii_exponent_sign/3,
    ascii_f/3,
    ascii_f_lowercase/3,
    ascii_f_uppercase/3,
    ascii_file_separator/3,
    ascii_five/3,
    ascii_form_feed/3,
    ascii_forward_slash/3,
    ascii_four/3,
    ascii_g/3,
    ascii_g_lowercase/3,
    ascii_g_uppercase/3,
    ascii_graph/3,
    ascii_grave_accent/3,
    ascii_greater_than_sign/3,
    ascii_group_separator/3,
    ascii_h/3,
    ascii_h_lowercase/3,
    ascii_h_uppercase/3,
    ascii_hexadecimal_digit/3,
    ascii_horizontal_tab/3,
    ascii_hyphen/3,
    ascii_hyphen_minus/3,
    ascii_i/3,
    ascii_i_lowercase/3,
    ascii_i_uppercase/3,
    ascii_j/3,
    ascii_j_lowercase/3,
    ascii_j_uppercase/3,
    ascii_k/3,
    ascii_k_lowercase/3,
    ascii_k_uppercase/3,
    ascii_l/3,
    ascii_l_lowercase/3,
    ascii_l_uppercase/3,
    ascii_line_feed/3,
    ascii_less_than_sign/3,
    ascii_letter/3,
    ascii_letter_lowercase/3,
    ascii_letter_uppercase/3,
    ascii_m/3,
    ascii_m_lowercase/3,
    ascii_m_uppercase/3,
    ascii_minus_sign/3,
    ascii_n/3,
    ascii_n_lowercase/3,
    ascii_n_uppercase/3,
    ascii_negative_acknowledgement/3,
    ascii_nine/3,
    ascii_null/3,
    ascii_number_sign/3,
    ascii_o/3,
    ascii_o_lowercase/3,
    ascii_o_uppercase/3,
    ascii_one/3,
    ascii_octal_digit/3,
    ascii_opening_bracket/3,
    ascii_opening_curly_bracket/3,
    ascii_opening_round_bracket/3,
    ascii_opening_square_bracket/3,
    ascii_p/3,
    ascii_p_lowercase/3,
    ascii_p_uppercase/3,
    ascii_percent_sign/3,
    ascii_plus_sign/3,
    ascii_positive_acknowledgement/3,
    ascii_print/3,
    ascii_punctuation/3,
    ascii_q/3,
    ascii_q_lowercase/3,
    ascii_q_uppercase/3,
    ascii_question_mark/3,
    ascii_r/3,
    ascii_r_lowercase/3,
    ascii_r_uppercase/3,
    ascii_record_separator/3,
    ascii_round_bracket/3,
    ascii_s/3,
    ascii_s_lowercase/3,
    ascii_s_uppercase/3,
    ascii_semi_colon/3,
    ascii_seven/3,
    ascii_shift/3,
    ascii_shift_in/3,
    ascii_shift_out/3,
    ascii_sign/3,
    ascii_six/3,
    ascii_slash/3,
    ascii_space/3,
    ascii_square_bracket/3,
    ascii_start_of_heading/3,
    ascii_start_of_text/3,
    ascii_substitute/3,
    ascii_synchronous_idle/3,
    ascii_t/3,
    ascii_t_lowercase/3,
    ascii_t_uppercase/3,
    ascii_tab/3,
    ascii_three/3,
    ascii_tilde/3,
    ascii_two/3,
    ascii_u/3,
    ascii_u_lowercase/3,
    ascii_u_uppercase/3,
    ascii_underscore/3,
    ascii_unit_separator/3,
    ascii_v/3,
    ascii_v_lowercase/3,
    ascii_v_uppercase/3,
    ascii_vertical_bar/3,
    ascii_vertical_tab/3,
    ascii_w/3,
    ascii_w_lowercase/3,
    ascii_w_uppercase/3,
    ascii_white/3,
    ascii_word/3,
    ascii_x/3,
    ascii_x_lowercase/3,
    ascii_x_uppercase/3,
    ascii_y/3,
    ascii_y_lowercase/3,
    ascii_y_uppercase/3,
    ascii_z/3,
    ascii_z_lowercase/3,
    ascii_z_uppercase/3,
    ascii_zero/3
  ]
).

/** <module> ASCI.

DCG rules that encode the ASCII standard.

@author Wouter Beek
@version 2013/01-2013/02, 2013/06.
*/



ascii_a(O1, R1-R0, C1-C0):-
  ascii_letter(a, O1, R1-R0, C1-C0).

ascii_a_lowercase(_O1, [97 | R0]-R0, [97 | C0]-C0).

ascii_a_uppercase(_O1, [65 | R0]-R0, [65 | C0]-C0).

ascii_acknowledgement(O1, R1-R0, C1-C0):-
  ascii_negative_acknowledgement(O1, R1-R0, C1-C0).
ascii_acknowledgement(O1, R1-R0, C1-C0):-
  ascii_positive_acknowledgement(O1, R1-R0, C1-C0).

ascii_alphanumeric(O1, R1-R0, C1-C0):-
  ascii_letter(O1, R1-R0, C1-C0).
ascii_alphanumeric(O1, R1-R0, C1-C0):-
  ascii_digit(O1, R1-R0, C1-C0).

ascii_ampersand(_O1, [38 | R0]-R0, [38 | C0]-C0).

% We only consider per-code replacements.
ascii_any(O1, [Y | R0]-R0, [X | C0]-C0):-
  option(replace(From,To), O1),
  call(From, O1, [X | R0]-R0, [X | C0], C0),
  call(To,   O1, [Y | R0]-R0, [Y | C0], C0),
  !.
% Codes > 127 are non-ASCII. We have not created DCGs for these codes yet.
ascii_any(_O1, [X | R0]-R0, [X | C0]-C0):-
  X > 127,
  !.
ascii_any(O1, R1-R0, C1-C0):-
  ascii_control(O1, R1-R0, C1-C0).
ascii_any(O1, R1-R0, C1-C0):-
  ascii_graph(O1, R1-R0, C1-C0).
ascii_any(O1, R1-R0, C1-C0):-
  ascii_white(O1, R1-R0, C1-C0).

% XML: &apos.
ascii_apostrophe(O1, [38, 97, 112, 111, 115, 59 | R0]-R0, [39 | C0]-C0):-
  option(lang(xml), O1),
  !.
ascii_apostrophe(_O1, [39 | R0]-R0, [39 | C0]-C0).

ascii_decimal_digit(O1, R1-R0, C1-C0):-
  ascii_octal_digit(O1, R1-R0, C1-C0).
ascii_decimal_digit(O1, R1-R0, C1-C0):-
  ascii_eight(O1, R1-R0, C1-C0).
ascii_decimal_digit(O1, R1-R0, C1-C0):-
  ascii_nine(O1, R1-R0, C1-C0).

ascii_digit(O1, R1-R0, C1-C0):-
  ascii_decimal_digit(O1, R1-R0, C1-C0).
ascii_digit(O1, R1-R0, C1-C0):-
  ascii_hexadecimal_digit(O1, R1-R0, C1-C0).
ascii_digit(O1, R1-R0, C1-C0):-
  ascii_octal_digit(O1, R1-R0, C1-C0).

ascii_asterisk(_O1, [42 | R0]-R0, [42 | C0]-C0).

ascii_at_sign(_O1, [65 | R0]-R0, [64 | C0]-C0).

ascii_b(O1, R1-R0, C1-C0):-
  \+ option(case(upper), O1),
  ascii_b_lowercase(O1, R1-R0, C1-C0).
ascii_b(O1, [X | R0]-R0, C1-C0):-
  (option(case(insensitive), O1) ; option(case(upper), O1)),
  ascii_b_lowercase(O1, [Y | R0]-R0, C1-C0),
  X is Y - 32.
ascii_b(O1, R1-R0, C1-C0):-
  \+ option(case(lower), O1),
  ascii_b_uppercase(O1, R1-R0, C1-C0).
ascii_b(O1, [X | R0]-R0, C1-C0):-
  (option(case(insensitive), O1) ; option(case(lower), O1)),
  ascii_b_uppercase(O1, [Y | R0]-R0, C1-C0),
  X is Y + 32.

ascii_b(O1, R1-R0, C1-C0):-
  ascii_letter(b, O1, R1-R0, C1-C0).

ascii_b_lowercase(_O1, [98 | R0]-R0, [98 | C0]-C0).

ascii_b_uppercase(_O1, [66 | R0]-R0, [66 | C0]-C0).

ascii_backslash(_O1, [92 | R0]-R0, [92 | C0]-C0).

ascii_backspace(_O1, [8 | R0]-R0, [8 | C0]-C0).

ascii_bell(O1, [92, 98 | R0]-R0, [7 | C0]-C0):-
  option(lang(c), O1),
  !.
ascii_bell(_O1, [7 | R0]-R0, [7 | C0]-C0).

ascii_binary_digit(O1, R1-R0, C1-C0):-
  ascii_zero(O1, R1-R0, C1-C0).
ascii_binary_digit(O1, R1-R0, C1-C0):-
  ascii_one(O1, R1-R0, C1-C0).

ascii_blank(O1, R1-R0, C1-C0):-
  ascii_space(O1, R1-R0, C1-C0).
ascii_blank(O1, R1-R0, C1-C0):-
  ascii_horizontal_tab(O1, R1-R0, C1-C0).

ascii_bracket(O1, R1-R0, C1-C0):-
  ascii_closing_bracket(O1, R1-R0, C1-C0).
ascii_bracket(O1, R1-R0, C1-C0):-
  ascii_opening_bracket(O1, R1-R0, C1-C0).

ascii_c(O1, R1-R0, C1-C0):-
  ascii_letter(c, O1, R1-R0, C1-C0).

ascii_c_lowercase(_O1, [99 | R0]-R0, [99 | C0]-C0).

ascii_c_uppercase(_O1, [67 | R0]-R0, [67 | C0]-C0).

ascii_cancel(_O1, [24 | R0]-R0, [24 | C0]-C0).

ascii_caret(_O1, [94 | R0]-R0, [94 | C0]-C0).

ascii_carriage_return(_O1, [13 | R0]-R0, [13 | C0]-C0).

ascii_closing_bracket(O1, R1-R0, C1-C0):-
  ascii_closing_curly_bracket(O1, R1-R0, C1-C0).
ascii_closing_bracket(O1, R1-R0, C1-C0):-
  ascii_closing_round_bracket(O1, R1-R0, C1-C0).
ascii_closing_bracket(O1, R1-R0, C1-C0):-
  ascii_closing_square_bracket(O1, R1-R0, C1-C0).

ascii_closing_curly_bracket(_O1, [125 | R0]-R0, [125 | C0]-C0).

ascii_closing_round_bracket(_O1, [41 | R0]-R0, [41 | C0]-C0).

ascii_closing_square_bracket(_O1, [93 | R0]-R0, [93 | C0]-C0).

ascii_colon(_O1, [58 | R0]-R0, [58 | C0]-C0).

ascii_comma(_O1, [44 | R0]-R0, [44 | C0]-C0).

ascii_control(O1, R1-R0, C1-C0):-
  ascii_acknowledgement(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_backspace(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_bell(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_cancel(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_carriage_return(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_data_link_escape(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_delete(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_device_control(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_enquiry(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_end_of_medium(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_end_of_text(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_end_of_transmission(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_end_of_transmission_block(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_escape(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_file_separator(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_form_feed(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_group_separator(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_line_feed(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_null(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_record_separator(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_shift(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_start_of_heading(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_start_of_text(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_substitute(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_synchronous_idle(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_tab(O1, R1-R0, C1-C0).
ascii_control(O1, R1-R0, C1-C0):-
  ascii_unit_separator(O1, R1-R0, C1-C0).

ascii_curly_bracket(O1, R1-R0, C1-C0):-
  ascii_closing_curly_bracket(O1, R1-R0, C1-C0).
ascii_curly_bracket(O1, R1-R0, C1-C0):-
  ascii_opening_curly_bracket(O1, R1-R0, C1-C0).

ascii_d(O1, R1-R0, C1-C0):-
  ascii_letter(d, O1, R1-R0, C1-C0).

ascii_d_lowercase(_O1, [100 | R0]-R0, [100 | C0]-C0).

ascii_d_uppercase(_O1, [68 | R0]-R0, [68 | C0]-C0).

ascii_data_link_escape(_O1, [16 | R0]-R0, [16 | C0]-C0).

ascii_delete(_O1, [127 | R0]-R0, [127 | C0]-C0).

ascii_device_control(O1, R1-R0, C1-C0):-
  ascii_device_control_1(O1, R1-R0, C1-C0).
ascii_device_control(O1, R1-R0, C1-C0):-
  ascii_device_control_2(O1, R1-R0, C1-C0).
ascii_device_control(O1, R1-R0, C1-C0):-
  ascii_device_control_3(O1, R1-R0, C1-C0).
ascii_device_control(O1, R1-R0, C1-C0):-
  ascii_device_control_4(O1, R1-R0, C1-C0).

ascii_device_control_1(_O1, [17 | R0]-R0, [17 | C0]-C0).

ascii_device_control_2(_O1, [18 | R0]-R0, [18 | C0]-C0).

ascii_device_control_3(_O1, [19 | R0]-R0, [19 | C0]-C0).

ascii_device_control_4(_O1, [20 | R0]-R0, [20 | C0]-C0).

ascii_dollar_sign(_O1, [36 | R0]-R0, [36 | C0]-C0).

ascii_dot(_O1, [46 | R0]-R0, [46 | C0]-C0).

% XML: &quot.
ascii_double_quote(O1, [38, 113, 117, 111, 116, 59 | R0]-R0, [34 | C0]-C0):-
  option(lang(xml), O1),
  !.
ascii_double_quote(_O1, [34 | R0]-R0, [34 | C0]-C0).

ascii_e(O1, R1-R0, C1-C0):-
  ascii_letter(e, O1, R1-R0, C1-C0).

ascii_e_lowercase(_O1, [101 | R0]-R0, [101 | C0]-C0).

ascii_e_uppercase(_O1, [69 | R0]-R0, [69 | C0]-C0).

ascii_eight(_O1, [56 | R0]-R0, [56 | C0]-C0).

ascii_enquiry(_O1, [5 | R0]-R0, [5 | C0]-C0).

ascii_end_of_line(O1, R1-R0, C1-C0):-
  ascii_carriage_return(O1, R1-R0, C1-C0).
ascii_end_of_line(O1, R1-R0, C1-C0):-
  ascii_end_of_medium(O1, R1-R0, C1-C0).
ascii_end_of_line(O1, R1-R0, C1-C0):-
  ascii_end_of_text(O1, R1-R0, C1-C0).
ascii_end_of_line(O1, R1-R0, C1-C0):-
  ascii_end_of_transmission(O1, R1-R0, C1-C0).
ascii_end_of_line(O1, R1-R0, C1-C0):-
  ascii_end_of_transmission_block(O1, R1-R0, C1-C0).
ascii_end_of_line(O1, R1-R0, C1-C0):-
  ascii_line_feed(O1, R1-R0, C1-C0).

ascii_end_of_medium(_O1, [25 | R0]-R0, [25 | C0]-C0).

ascii_end_of_text(_O1, [3 | R0]-R0, [3 | C0]-C0).

ascii_end_of_transmission(_O1, [4 | R0]-R0, [4 | C0]-C0).

ascii_end_of_transmission_block(_O1, [23 | R0]-R0, [23 | C0]-C0).

ascii_end_of_word(O1, R1-R0, C1-C0):-
  ascii_end_of_line(O1, R1-R0, C1-C0).
ascii_end_of_word(O1, R1-R0, C1-C0):-
  ascii_punctuation(O1, R1-R0, C1-C0).
ascii_end_of_word(O1, R1-R0, C1-C0):-
  ascii_white(O1, R1-R0, C1-C0).

ascii_equals_sign(_O1, [61 | R0]-R0, [61 | C0]-C0).

ascii_escape(_O1, [27 | R0]-R0, [27 | C0]-C0).

ascii_exclamation_mark(_O1, [33 | R0]-R0, [33 | C0]-C0).

ascii_exponent_sign(O1, R1-R0, C1-C0):-
  ascii_e(O1, R1-R0, C1-C0).

ascii_f(O1, R1-R0, C1-C0):-
  ascii_letter(f, O1, R1-R0, C1-C0).

ascii_f_lowercase(_O1, [102 | R0]-R0, [102 | C0]-C0).

ascii_f_uppercase(_O1, [70 | R0]-R0, [70 | C0]-C0).

ascii_file_separator(_O1, [28 | R0]-R0, [28 | C0]-C0).

ascii_five(_O1, [53 | R0]-R0, [53 | C0]-C0).

ascii_form_feed(_O1, [12 | R0]-R0, [12 | C0]-C0).

ascii_forward_slash(_O1, [47 | R0]-R0, [47 | C0]-C0).

ascii_four(_O1, [52 | R0]-R0, [52 | C0]-C0).

ascii_g(O1, R1-R0, C1-C0):-
  ascii_letter(g, O1, R1-R0, C1-C0).

ascii_g_lowercase(_O1, [103 | R0]-R0, [103 | C0]-C0).

ascii_g_uppercase(_O1, [71 | R0]-R0, [71 | C0]-C0).

ascii_graph(O1, R1-R0, C1-C0):-
  ascii_alphanumeric(O1, R1-R0, C1-C0).
ascii_graph(O1, R1-R0, C1-C0):-
  ascii_punctuation(O1, R1-R0, C1-C0).

ascii_grave_accent(_O1, [96 | R0]-R0, [96 | C0]-C0).

% XML: &gt.
ascii_greater_than_sign(O1, [38, 103, 116, 59 | R0]-R0, [62 | C0]-C0):-
  option(lang(xml), O1),
  !.
ascii_greater_than_sign(_O1, [62 | R0]-R0, [62 | C0]-C0).

ascii_group_separator(_O1, [29 | R0]-R0, [29 | C0]-C0).

ascii_h(O1, R1-R0, C1-C0):-
  ascii_letter(h, O1, R1-R0, C1-C0).

ascii_h_lowercase(_O1, [104 | R0]-R0, [104 | C0]-C0).

ascii_h_uppercase(_O1, [72 | R0]-R0, [72 | C0]-C0).

ascii_hexadecimal_digit(O1, R1-R0, C1-C0):-
  ascii_decimal_digit(O1, R1-R0, C1-C0).
ascii_hexadecimal_digit(O1, R1-R0, C1-C0):-
  ascii_a(O1, R1-R0, C1-C0).
ascii_hexadecimal_digit(O1, R1-R0, C1-C0):-
  ascii_b(O1, R1-R0, C1-C0).
ascii_hexadecimal_digit(O1, R1-R0, C1-C0):-
  ascii_c(O1, R1-R0, C1-C0).
ascii_hexadecimal_digit(O1, R1-R0, C1-C0):-
  ascii_d(O1, R1-R0, C1-C0).
ascii_hexadecimal_digit(O1, R1-R0, C1-C0):-
  ascii_e(O1, R1-R0, C1-C0).
ascii_hexadecimal_digit(O1, R1-R0, C1-C0):-
  ascii_f(O1, R1-R0, C1-C0).

ascii_horizontal_tab(O1, [92, 116 | R0]-R0, [9 | C0]-C0):-
  option(lang(c), O1),
  !.
ascii_horizontal_tab(_O1, [9 | R0]-R0, [9 | C0]-C0).

ascii_hyphen(O1, R1-R0, C1-C0):-
  ascii_hyphen_minus(O1, R1-R0, C1-C0).

ascii_hyphen_minus(_O1, [45 | R0]-R0, [45 | C0]-C0).

ascii_i(O1, R1-R0, C1-C0):-
  ascii_letter(i, O1, R1-R0, C1-C0).

ascii_i_lowercase(_O1, [105 | R0]-R0, [105 | C0]-C0).

ascii_i_uppercase(_O1, [73 | R0]-R0, [73 | C0]-C0).

ascii_j(O1, R1-R0, C1-C0):-
  ascii_letter(j, O1, R1-R0, C1-C0).

ascii_j_lowercase(_O1, [106 | R0]-R0, [106 | C0]-C0).

ascii_j_uppercase(_O1, [74 | R0]-R0, [74 | C0]-C0).

ascii_k(O1, R1-R0, C1-C0):-
  ascii_letter(k, O1, R1-R0, C1-C0).

ascii_k_lowercase(_O1, [107 | R0]-R0, [107 | C0]-C0).

ascii_k_uppercase(_O1, [75 | R0]-R0, [75 | C0]-C0).

ascii_l(O1, R1-R0, C1-C0):-
  ascii_letter(l, O1, R1-R0, C1-C0).

ascii_l_lowercase(_O1, [108 | R0]-R0, [108 | C0]-C0).

ascii_l_uppercase(_O1, [76 | R0]-R0, [76 | C0]-C0).

ascii_line_feed(O1, [92, 110 | R0]-R0, [10 | C0]-C0):-
  option(lang(c), O1),
  !.
ascii_line_feed(_O1, [10 | R0]-R0, [10 | C0]-C0).

% XML: &lt.
ascii_less_than_sign(O1, [38, 108, 116, 59 | R0]-R0, [60 | C0]-C0):-
  option(lang(xml), O1),
  !.
ascii_less_than_sign(_O1, [60 | R0]-R0, [60 | C0]-C0).

% Case sensitive, case insensitive and lowercase parse this.
% Uppercase cannot return an lowercase result.
ascii_letter(O1, R1-R0, C1-C0):-
  \+ option(case(upper), O1),
  ascii_letter_lowercase(O1, R1-R0, C1-C0).
% Only case insesitive and uppercase parse this, since they accept/requir.
% the uppercase variant of the lowercase input.
ascii_letter(O1, [X | R0]-R0, C1-C0):-
  (option(case(insensitive), O1) ; option(case(upper), O1)),
  ascii_letter_lowercase(O1, [Y | R0]-R0, C1-C0),
  X is Y - 32.
% Case sensitive, case insensitive and lowercase parse this.
% Lowercase cannot return an uppercase result.
ascii_letter(O1, R1-R0, C1-C0):-
  \+ option(case(lower), O1),
  ascii_letter_uppercase(O1, R1-R0, C1-C0).
% Only case insesitive and lowercase parse this, since they accept/requir.
% the lowerrcase variant of the uppercase input.
ascii_letter(O1, [X | R0]-R0, C1-C0):-
  (option(case(insensitive), O1) ; option(case(lower), O1)),
  ascii_letter_uppercase(O1, [Y | R0]-R0, C1-C0),
  X is Y + 32.

%! letter(
%!   +Letter:oneof([a-z]),
%!   +Options:list(nvpair),
%!   ?R:diffpair,
%!   ?C:diffpair
%! ) is nondet.
% This is the generic method for letters. It uses the option =|case/1|= t.
% determine which uppercase and/or lowercase variants of a letter are to b.
% read and/or written.

% A lowercase letter is read and written.
% A result for options =|case(insensitive)|=, =|case(lower)|=, an.
% =|case(sensitive)|=.
ascii_letter(Letter, O1, R1-R0, C1-C0):-
  \+ option(case(upper), O1),
  format(atom(P), '~w_lowercase', [Letter]),
  call(P, O1, R1-R0, C1-C0).
% A lowercase letter is read and its uppercase variant is written.
% A result for options =|case(insensitive)|= and =|case(upper)|=.
ascii_letter(Letter, O1, [X | R0]-R0, C1-C0):-
  (option(case(insensitive), O1) ; option(case(upper), O1)),
  format(atom(P), '~w_lowercase', [Letter]),
  call(P, O1, [Y | R0]-R0, C1-C0),
  X is Y - 32.
% An uppercase letter is read and written.
% A result for options =|case(insensitive)|=, =|case(sensitive)|= an.
% =|case(upper)|=.
ascii_letter(Letter, O1, R1-R0, C1-C0):-
  \+ option(case(lower), O1),
  format(atom(P), '~w_uppercase', [Letter]),
  call(P, O1, R1-R0, C1-C0).
% An uppercase letter is read and its lowercase variant is written.
% A result for options =|case(insensitive)|= and =|case(lower)|=.
ascii_letter(Letter, O1, [X | R0]-R0, C1-C0):-
  (option(case(insensitive), O1) ; option(case(lower), O1)),
  format(atom(P), '~w_uppercase', [Letter]),
  call(P, O1, [Y | R0]-R0, C1-C0),
  X is Y + 32.

ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_a_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_b_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_c_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_d_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_e_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_f_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_g_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_h_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_i_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_j_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_k_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_l_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_m_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_n_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_o_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_p_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_q_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_r_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_s_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_t_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_u_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_v_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_w_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_x_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_y_lowercase(O1, R1-R0, C1-C0).
ascii_letter_lowercase(O1, R1-R0, C1-C0):-
  ascii_z_lowercase(O1, R1-R0, C1-C0).

ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_a_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_b_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_c_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_d_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_e_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_g_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_h_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_i_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_j_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_k_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_l_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_m_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_n_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_o_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_p_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_q_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_r_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_s_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_t_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_u_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_v_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_w_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_x_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_y_uppercase(O1, R1-R0, C1-C0).
ascii_letter_uppercase(O1, R1-R0, C1-C0):-
  ascii_z_uppercase(O1, R1-R0, C1-C0).

ascii_m(O1, R1-R0, C1-C0):-
  ascii_letter(m, O1, R1-R0, C1-C0).

ascii_m_lowercase(_O1, [109 | R0]-R0, [109 | C0]-C0).

ascii_m_uppercase(_O1, [77 | R0]-R0, [77 | C0]-C0).

ascii_minus_sign(O1, R1-R0, C1-C0):-
  ascii_hyphen_minus(O1, R1-R0, C1-C0).

ascii_n(O1, R1-R0, C1-C0):-
  ascii_letter(n, O1, R1-R0, C1-C0).

ascii_n_lowercase(_O1, [110 | R0]-R0, [110 | C0]-C0).

ascii_n_uppercase(_O1, [78 | R0]-R0, [78 | C0]-C0).

ascii_negative_acknowledgement(_O1, [21 | R0]-R0, [21 | C0]-C0).

ascii_nine(_O1, [57 | R0]-R0, [57 | C0]-C0).

ascii_null(_O1, [0 | R0]-R0, [0 | C0]-C0).

ascii_number_sign(_O1, [35 | R0]-R0, [35 | C0]-C0).

ascii_o(O1, R1-R0, C1-C0):-
  ascii_letter(o, O1, R1-R0, C1-C0).

ascii_o_lowercase(_O1, [111 | R0]-R0, [111 | C0]-C0).

ascii_o_uppercase(_O1, [79 | R0]-R0, [79 | C0]-C0).

ascii_one(_O1, [49 | R0]-R0, [49 | C0]-C0).

ascii_octal_digit(O1, R1-R0, C1-C0):-
  ascii_binary_digit(O1, R1-R0, C1-C0).
ascii_octal_digit(O1, R1-R0, C1-C0):-
  ascii_two(O1, R1-R0, C1-C0).
ascii_octal_digit(O1, R1-R0, C1-C0):-
  ascii_three(O1, R1-R0, C1-C0).
ascii_octal_digit(O1, R1-R0, C1-C0):-
  ascii_four(O1, R1-R0, C1-C0).
ascii_octal_digit(O1, R1-R0, C1-C0):-
  ascii_five(O1, R1-R0, C1-C0).
ascii_octal_digit(O1, R1-R0, C1-C0):-
  ascii_six(O1, R1-R0, C1-C0).
ascii_octal_digit(O1, R1-R0, C1-C0):-
  ascii_seven(O1, R1-R0, C1-C0).

ascii_opening_bracket(O1, R1-R0, C1-C0):-
  ascii_opening_curly_bracket(O1, R1-R0, C1-C0).
ascii_opening_bracket(O1, R1-R0, C1-C0):-
  ascii_opening_round_bracket(O1, R1-R0, C1-C0).
ascii_opening_bracket(O1, R1-R0, C1-C0):-
  ascii_opening_square_bracket(O1, R1-R0, C1-C0).

ascii_opening_curly_bracket(_O1, [123 | R0]-R0, [123 | C0]-C0).

ascii_opening_round_bracket(_O1, [40 | R0]-R0, [40 | C0]-C0).

ascii_opening_square_bracket(_O1, [91 | R0]-R0, [91 | C0]-C0).

ascii_p(O1, R1-R0, C1-C0):-
  ascii_letter(p, O1, R1-R0, C1-C0).

ascii_p_lowercase(_O1, [112 | R0]-R0, [112 | C0]-C0).

ascii_p_uppercase(_O1, [80 | R0]-R0, [80 | C0]-C0).

ascii_percent_sign(_O1, [37 | R0]-R0, [37 | C0]-C0).

ascii_plus_sign(_O1, [43 | R0]-R0, [43 | C0]-C0).

ascii_positive_acknowledgement(_O1, [6 | R0]-R0, [6 | C0]-C0).

ascii_print(O1, R1-R0, C1-C0):-
  ascii_graph(O1, R1-R0, C1-C0).
ascii_print(O1, R1-R0, C1-C0):-
  ascii_space(O1, R1-R0, C1-C0).

ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_ampersand(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_apostrophe(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_asterisk(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_at_sign(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_bracket(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_caret(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_colon(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_comma(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_dollar_sign(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_dot(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_double_quote(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_equals_sign(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_exclamation_mark(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_grave_accent(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_greater_than_sign(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_hyphen_minus(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_less_than_sign(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_number_sign(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_percent_sign(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_plus_sign(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_question_mark(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_semi_colon(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_slash(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_tilde(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_underscore(O1, R1-R0, C1-C0).
ascii_punctuation(O1, R1-R0, C1-C0):-
  ascii_vertical_bar(O1, R1-R0, C1-C0).

ascii_q(O1, R1-R0, C1-C0):-
  ascii_letter(q, O1, R1-R0, C1-C0).

ascii_q_lowercase(_O1, [113 | R0]-R0, [113 | C0]-C0).

ascii_q_uppercase(_O1, [81 | R0]-R0, [81 | C0]-C0).

ascii_question_mark(_O1, [63 | R0]-R0, [63 | C0]-C0).

ascii_r(O1, R1-R0, C1-C0):-
  ascii_letter(r, O1, R1-R0, C1-C0).

ascii_r_lowercase(_O1, [114 | R0]-R0, [114 | C0]-C0).

ascii_r_uppercase(_O1, [82 | R0]-R0, [82 | C0]-C0).

ascii_record_separator(_O1, [30 | R0]-R0, [30 | C0]-C0).

ascii_round_bracket(O1, R1-R0, C1-C0):-
  ascii_closing_round_bracket(O1, R1-R0, C1-C0).
ascii_round_bracket(O1, R1-R0, C1-C0):-
  ascii_opening_round_bracket(O1, R1-R0, C1-C0).

ascii_s(O1, R1-R0, C1-C0):-
  ascii_letter(s, O1, R1-R0, C1-C0).

ascii_s_lowercase(_O1, [115 | R0]-R0, [115 | C0]-C0).

ascii_s_uppercase(_O1, [83 | R0]-R0, [83 | C0]-C0).

ascii_semi_colon(_O1, [59 | R0]-R0, [59 | C0]-C0).

ascii_seven(_O1, [55 | R0]-R0, [55 | C0]-C0).

ascii_shift(O1, R1-R0, C1-C0):-
  ascii_shift_in(O1, R1-R0, C1-C0).
ascii_shift(O1, R1-R0, C1-C0):-
  ascii_shift_out(O1, R1-R0, C1-C0).

ascii_shift_in(_O1, [15 | R0]-R0, [15 | C0]-C0).

ascii_shift_out(_O1, [14 | R0]-R0, [14 | C0]-C0).

ascii_sign(O1, R1-R0, C1-C0):-
  ascii_minus_sign(O1, R1-R0, C1-C0).
ascii_sign(O1, R1-R0, C1-C0):-
  ascii_plus_sign(O1, R1-R0, C1-C0).

ascii_single_quote(O1, R1-R0, C1-C0):-
  ascii_apostrophe(O1, R1-R0, C1-C0).

ascii_six(_O1, [54 | R0]-R0, [54 | C0]-C0).

ascii_slash(O1, R1-R0, C1-C0):-
  ascii_backslash(O1, R1-R0, C1-C0).
ascii_slash(O1, R1-R0, C1-C0):-
  ascii_forward_slash(O1, R1-R0, C1-C0).

ascii_space(_O1, [32 | R0]-R0, [32 | C0]-C0).

ascii_square_bracket(O1, R1-R0, C1-C0):-
  ascii_closing_square_bracket(O1, R1-R0, C1-C0).
ascii_square_bracket(O1, R1-R0, C1-C0):-
  ascii_opening_square_bracket(O1, R1-R0, C1-C0).

ascii_start_of_heading(_O1, [1 | R0]-R0, [1 | C0]-C0).

ascii_start_of_text(_O1, [2 | R0]-R0, [2 | C0]-C0).

ascii_substitute(_O1, [26 | R0]-R0, [26 | C0]-C0).

ascii_synchronous_idle(_O1, [22 | R0]-R0, [22 | C0]-C0).

ascii_t(O1, R1-R0, C1-C0):-
  ascii_letter(t, O1, R1-R0, C1-C0).

ascii_t_lowercase(_O1, [116 | R0]-R0, [116 | C0]-C0).

ascii_t_uppercase(_O1, [84 | R0]-R0, [84 | C0]-C0).

ascii_tab(O1, R1-R0, C1-C0):-
  ascii_horizontal_tab(O1, R1-R0, C1-C0).
ascii_tab(O1, R1-R0, C1-C0):-
  ascii_vertical_tab(O1, R1-R0, C1-C0).

ascii_three(_O1, [51 | R0]-R0, [51 | C0]-C0).

ascii_tilde(_O1, [126 | R0]-R0, [126 | C0]-C0).

ascii_two(_O1, [50 | R0]-R0, [50 | C0]-C0).

ascii_u(O1, R1-R0, C1-C0):-
  ascii_letter(u, O1, R1-R0, C1-C0).

ascii_u_lowercase(_O1, [117 | R0]-R0, [117 | C0]-C0).

ascii_u_uppercase(_O1, [85 | R0]-R0, [85 | C0]-C0).

ascii_underscore(_O1, [95 | R0]-R0, [95 | C0]-C0).

ascii_unit_separator(_O1, [31 | R0]-R0, [31 | C0]-C0).

ascii_v(O1, R1-R0, C1-C0):-
  ascii_letter(v, O1, R1-R0, C1-C0).

ascii_v_lowercase(_O1, [118 | R0]-R0, [118 | C0]-C0).

ascii_v_uppercase(_O1, [86 | R0]-R0, [86 | C0]-C0).

ascii_vertical_bar(_O1, [124 | R0]-R0, [124 | C0]-C0).

ascii_vertical_tab(_O1, [11 | R0]-R0, [11 | C0]-C0).

ascii_w(O1, R1-R0, C1-C0):-
  ascii_letter(w, O1, R1-R0, C1-C0).

ascii_w_lowercase(_O1, [119 | R0]-R0, [119 | C0]-C0).

ascii_w_uppercase(_O1, [87 | R0]-R0, [87 | C0]-C0).

ascii_white(O1, R1-R0, C1-C0):-
  ascii_end_of_line(O1, R1-R0, C1-C0).
ascii_white(O1, R1-R0, C1-C0):-
  ascii_form_feed(O1, R1-R0, C1-C0).
ascii_white(O1, R1-R0, C1-C0):-
  ascii_space(O1, R1-R0, C1-C0).
ascii_white(O1, R1-R0, C1-C0):-
  ascii_tab(O1, R1-R0, C1-C0).

% A word consists of alphanumeric characters and underscores.
ascii_word(O1, R1-R0, C1-C0):-
  ascii_alphanumeric(O1, R1-R0, C1-C0).
ascii_word(O1, R1-R0, C1-C0):-
  ascii_underscore(O1, R1-R0, C1-C0).

ascii_x(O1, R1-R0, C1-C0):-
  ascii_letter(x, O1, R1-R0, C1-C0).

ascii_x_lowercase(_O1, [120 | R0]-R0, [120 | C0]-C0).

ascii_x_uppercase(_O1, [88 | R0]-R0, [88 | C0]-C0).

ascii_y(O1, R1-R0, C1-C0):-
  ascii_letter(y, O1, R1-R0, C1-C0).

ascii_y_lowercase(_O1, [121 | R0]-R0, [121 | C0]-C0).

ascii_y_uppercase(_O1, [89 | R0]-R0, [89 | C0]-C0).

ascii_z(O1, R1-R0, C1-C0):-
  ascii_letter(z, O1, R1-R0, C1-C0).

ascii_z_lowercase(_O1, [122 | R0]-R0, [122 | C0]-C0).

ascii_z_uppercase(_O1, [90 | R0]-R0, [90 | C0]-C0).

ascii_zero(_O1, [48 | R0]-R0, [48 | C0]-C0).

