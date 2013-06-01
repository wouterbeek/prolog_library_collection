:- module(
  dcg_ascii,
  [
    a//0,
    a_lowercase//0,
    a_uppercase//0,
    acknowledgement//0,
    ampersand//0,
    apostrophe//0,
    asterisk//0,
    at_sign//0,
    b//0,
    b_lowercase//0,
    b_uppercase//0,
    backslash//0,
    backspace//0,
    bell//0,
    bracket//0,
    c//0,
    c_lowercase//0,
    c_uppercase//0,
    cancel//0,
    caret//0,
    carriage_return//0,
    closing_bracket//0,
    closing_curly_bracket//0,
    closing_round_bracket//0,
    closing_square_bracket//0,
    colon//0,
    comma//0,
    control//0,
    copyright//0,
    curly_bracket//0,
    d//0,
    d_lowercase//0,
    d_uppercase//0,
    data_link_escape//0,
    delete//0,
    device_control//0,
    device_control_1//0,
    device_control_2//0,
    device_control_3//0,
    device_control_4//0,
    dollar_sign//0,
    dot//0,
    double_quote//0,
    e//0,
    e_lowercase//0,
    e_uppercase//0,
    eight//0,
    enquiry//0,
    end_of_line//0,
    end_of_medium//0,
    end_of_text//0,
    end_of_transmission//0,
    end_of_transmission_block//0,
    equals_sign//0,
    escape//0,
    exclamation_mark//0,
    exponent_sign//0,
    f//0,
    f_lowercase//0,
    f_uppercase//0,
    file_separator//0,
    five//0,
    form_feed//0,
    forward_slash//0,
    four//0,
    g//0,
    g_lowercase//0,
    g_uppercase//0,
    grave_accent//0,
    greater_than_sign//0,
    group_separator//0,
    h//0,
    h_lowercase//0,
    h_uppercase//0,
    horizontal_tab//0,
    hyphen//0,
    hyphen_minus//0,
    i//0,
    i_lowercase//0,
    i_uppercase//0,
    j//0,
    j_lowercase//0,
    j_uppercase//0,
    k//0,
    k_lowercase//0,
    k_uppercase//0,
    l//0,
    l_lowercase//0,
    l_uppercase//0,
    line_feed//0,
    less_than_sign//0,
    letter//0,
    letter_lowercase//0,
    letter_uppercase//0,
    m//0,
    m_lowercase//0,
    m_uppercase//0,
    minus_sign//0,
    n//0,
    n_lowercase//0,
    n_uppercase//0,
    negative_acknowledgement//0,
    nine//0,
    null//0,
    number_sign//0,
    o//0,
    o_lowercase//0,
    o_uppercase//0,
    one//0,
    opening_bracket//0,
    opening_curly_bracket//0,
    opening_round_bracket//0,
    opening_square_bracket//0,
    p//0,
    p_lowercase//0,
    p_uppercase//0,
    percent_sign//0,
    plus_sign//0,
    positive_acknowledgement//0,
    print//0,
    punctuation//0,
    q//0,
    q_lowercase//0,
    q_uppercase//0,
    question_mark//0,
    r//0,
    r_lowercase//0,
    r_uppercase//0,
    record_separator//0,
    round_bracket//0,
    s//0,
    s_lowercase//0,
    s_uppercase//0,
    semi_colon//0,
    seven//0,
    shift//0,
    shift_in//0,
    shift_out//0,
    sign//0,
    six//0,
    slash//0,
    space//0,
    square_bracket//0,
    start_of_heading//0,
    start_of_text//0,
    substitute//0,
    synchronous_idle//0,
    t//0,
    t_lowercase//0,
    t_uppercase//0,
    tab//0,
    three//0,
    tilde//0,
    two//0,
    u//0,
    u_lowercase//0,
    u_uppercase//0,
    underscore//0,
    unit_separator//0,
    v//0,
    v_lowercase//0,
    v_uppercase//0,
    vertical_bar//0,
    vertical_tab//0,
    w//0,
    w_lowercase//0,
    w_uppercase//0,
    x//0,
    x_lowercase//0,
    x_uppercase//0,
    y//0,
    y_lowercase//0,
    y_uppercase//0,
    z//0,
    z_lowercase//0,
    z_uppercase//0,
    zero//0
  ]
).

/** <module> DCG_ASCII

DCG rules that encode the ASCII standard.

There are several different variations of the 8-bit ASCII table.
The table below is according to ISO 8859-1, also called ISO Latin-1.
Codes 129-159 contain the Microsoft® Windows Latin-1 extended characters.

@author Wouter Beek
@compat http://www.ascii-code.com/
@version 2013/01-2013/02, 2013/05
*/



a --> a_lowercase.
a --> a_uppercase.

a_lowercase --> [97].

a_uppercase --> [65].

acknowledgement --> negative_acknowledgement.
acknowledgement --> positive_acknowledgement.

ampersand --> [38].

apostrophe --> [39].

asterisk --> [42].

at_sign --> [65].

b --> b_lowercase.
b --> b_uppercase.

b_lowercase --> [98].

b_uppercase --> [66].

backslash --> [92].

backspace --> [8].

bell --> [7].

bracket -->
  closing_bracket.
bracket -->
  opening_bracket.

c --> c_lowercase.
c --> c_uppercase.

c_lowercase --> [99].

c_uppercase --> [67].

cancel --> [24].

caret --> [94].

carriage_return --> [13].

closing_bracket --> closing_curly_bracket.
closing_bracket --> closing_round_bracket.
closing_bracket --> closing_square_bracket.

closing_curly_bracket --> [125].

closing_round_bracket --> [41].

closing_square_bracket --> [93].

colon --> [58].

comma --> [44].

control --> acknowledgement.
control --> backspace.
control --> bell.
control --> cancel.
control --> carriage_return.
control --> data_link_escape.
control --> delete.
control --> device_control.
control --> enquiry.
control --> end_of_medium.
control --> end_of_text.
control --> end_of_transmission.
control --> end_of_transmission_block.
control --> escape.
control --> file_separator.
control --> form_feed.
control --> group_separator.
control --> line_feed.
control --> null.
control --> record_separator.
control --> shift.
control --> start_of_heading.
control --> start_of_text.
control --> substitute.
control --> synchronous_idle.
control --> tab.
control --> unit_separator.

copyright --> [169].

curly_bracket --> closing_curly_bracket.
curly_bracket --> opening_curly_bracket.

d --> d_lowercase.
d --> d_uppercase.

d_lowercase --> [100].

d_uppercase --> [68].

data_link_escape --> [16].

delete --> [127].

device_control --> device_control_1.
device_control --> device_control_2.
device_control --> device_control_3.
device_control --> device_control_4.

device_control_1 --> [17].

device_control_2 --> [18].

device_control_3 --> [19].

device_control_4 --> [20].

dollar_sign --> [36].

dot --> [46].

double_quote --> [34].

e --> e_lowercase.
e --> e_uppercase.

e_lowercase --> [101].

e_uppercase --> [69].

eight --> [56].

enquiry --> [5].

end_of_line --> carriage_return.
end_of_line --> end_of_medium.
end_of_line --> end_of_text.
end_of_line --> end_of_transmission.
end_of_line --> end_of_transmission_block.
end_of_line --> line_feed.

end_of_medium --> [25].

end_of_text --> [3].

end_of_transmission --> [4].

end_of_transmission_block --> [23].

equals_sign --> [61].

escape --> [27].

exclamation_mark --> [33].

exponent_sign --> e.

f --> f_lowercase.
f --> f_uppercase.

f_lowercase --> [102].

f_uppercase --> [70].

file_separator --> [28].

five --> [53].

form_feed --> [12].

forward_slash --> [47].

four --> [52].

g --> g_lowercase.
g --> g_uppercase.

g_lowercase --> [103].

g_uppercase --> [71].

grave_accent --> [96].

greater_than_sign --> [62].

group_separator --> [29].

h --> h_lowercase.
h --> h_uppercase.

h_lowercase --> [104].

h_uppercase --> [72].

horizontal_tab --> [9].

hyphen --> hyphen_minus.

hyphen_minus --> [45].

i --> i_lowercase.
i --> i_uppercase.

i_lowercase --> [105].

i_uppercase --> [73].

j --> j_lowercase.
j --> j_uppercase.

j_lowercase --> [106].

j_uppercase --> [74].

k --> k_lowercase.
k --> k_uppercase.

k_lowercase --> [107].

k_uppercase --> [75].

l --> l_lowercase.
l --> l_uppercase.

l_lowercase --> [108].

l_uppercase --> [76].

line_feed --> [10].

less_than_sign --> [60].

letter --> letter_lowercase.
letter --> letter_uppercase.

letter_lowercase --> a_lowercase.
letter_lowercase --> b_lowercase.
letter_lowercase --> c_lowercase.
letter_lowercase --> d_lowercase.
letter_lowercase --> e_lowercase.
letter_lowercase --> f_lowercase.
letter_lowercase --> g_lowercase.
letter_lowercase --> h_lowercase.
letter_lowercase --> i_lowercase.
letter_lowercase --> j_lowercase.
letter_lowercase --> k_lowercase.
letter_lowercase --> l_lowercase.
letter_lowercase --> m_lowercase.
letter_lowercase --> n_lowercase.
letter_lowercase --> o_lowercase.
letter_lowercase --> p_lowercase.
letter_lowercase --> q_lowercase.
letter_lowercase --> r_lowercase.
letter_lowercase --> s_lowercase.
letter_lowercase --> t_lowercase.
letter_lowercase --> u_lowercase.
letter_lowercase --> v_lowercase.
letter_lowercase --> w_lowercase.
letter_lowercase --> x_lowercase.
letter_lowercase --> y_lowercase.
letter_lowercase --> z_lowercase.

letter_uppercase --> a_uppercase.
letter_uppercase --> b_uppercase.
letter_uppercase --> c_uppercase.
letter_uppercase --> d_uppercase.
letter_uppercase --> e_uppercase.
letter_uppercase --> g_uppercase.
letter_uppercase --> h_uppercase.
letter_uppercase --> i_uppercase.
letter_uppercase --> j_uppercase.
letter_uppercase --> k_uppercase.
letter_uppercase --> l_uppercase.
letter_uppercase --> m_uppercase.
letter_uppercase --> n_uppercase.
letter_uppercase --> o_uppercase.
letter_uppercase --> p_uppercase.
letter_uppercase --> q_uppercase.
letter_uppercase --> r_uppercase.
letter_uppercase --> s_uppercase.
letter_uppercase --> t_uppercase.
letter_uppercase --> u_uppercase.
letter_uppercase --> v_uppercase.
letter_uppercase --> w_uppercase.
letter_uppercase --> x_uppercase.
letter_uppercase --> y_uppercase.
letter_uppercase --> z_uppercase.

m --> m_lowercase.
m --> m_uppercase.

m_lowercase --> [109].

m_uppercase --> [77].

minus_sign --> hyphen_minus.

n --> n_lowercase.
n --> n_uppercase.

n_lowercase --> [110].

n_uppercase --> [78].

negative_acknowledgement --> [21].

nine --> [57].

null --> [0].

number_sign --> [35].

o --> o_lowercase.
o --> o_uppercase.

o_lowercase --> [111].

o_uppercase --> [79].

one --> [49].

opening_bracket --> opening_curly_bracket.
opening_bracket --> opening_round_bracket.
opening_bracket --> opening_square_bracket.

opening_curly_bracket --> [123].

opening_round_bracket --> [40].

opening_square_bracket --> [91].

p --> p_lowercase.
p --> p_uppercase.

p_lowercase --> [112].

p_uppercase --> [80].

percent_sign --> [37].

plus_sign --> [43].

positive_acknowledgement --> [6].

print --> [Code], {code_type(Code, graph)}.
print --> space.

punctuation --> ampersand.
punctuation --> apostrophe.
punctuation --> asterisk.
punctuation --> at_sign.
punctuation --> bracket.
punctuation --> caret.
punctuation --> colon.
punctuation --> comma.
punctuation --> dollar_sign.
punctuation --> dot.
punctuation --> double_quote.
punctuation --> equals_sign.
punctuation --> exclamation_mark.
punctuation --> grave_accent.
punctuation --> greater_than_sign.
punctuation --> hyphen_minus.
punctuation --> less_than_sign.
punctuation --> number_sign.
punctuation --> percent_sign.
punctuation --> plus_sign.
punctuation --> question_mark.
punctuation --> semi_colon.
punctuation --> slash.
punctuation --> tilde.
punctuation --> underscore.
punctuation --> vertical_bar.

q --> q_lowercase.
q --> q_uppercase.

q_lowercase --> [113].

q_uppercase --> [81].

question_mark --> [63].

r --> r_lowercase.
r --> r_uppercase.

r_lowercase --> [114].

r_uppercase --> [82].

record_separator --> [30].

round_bracket --> closing_round_bracket.
round_bracket --> opening_round_bracket.

s --> s_lowercase.
s --> s_uppercase.

s_lowercase --> [115].

s_uppercase --> [83].

semi_colon --> [59].

seven --> [55].

shift --> shift_in.
shift --> shift_out.

shift_in --> [15].

shift_out --> [14].

sign --> minus_sign.
sign --> plus_sign.

single_quote --> apostrophe.

six --> [54].

slash --> backslash.
slash --> forward_slash.

space --> [32].

square_bracket --> closing_square_bracket.
square_bracket --> opening_square_bracket.

start_of_heading --> [1].

start_of_text --> [2].

substitute --> [26].

synchronous_idle --> [22].

t --> t_lowercase.
t --> t_uppercase.

t_lowercase --> [116].

t_uppercase --> [84].

tab --> horizontal_tab.
tab --> vertical_tab.

three --> [51].

tilde --> [126].

two --> [50].

u --> u_lowercase.
u --> u_uppercase.

u_lowercase --> [117].

u_uppercase --> [85].

underscore --> [95].

unit_separator --> [31].

v --> v_lowercase.
v --> v_uppercase.

v_lowercase --> [118].

v_uppercase --> [86].

vertical_bar --> [124].

vertical_tab --> [11].

w --> w_lowercase.
w --> w_uppercase.

w_lowercase --> [119].

w_uppercase --> [87].

x --> x_lowercase.
x --> x_uppercase.

x_lowercase --> [120].

x_uppercase --> [88].

y --> y_lowercase.
y --> y_uppercase.

y_lowercase --> [121].

y_uppercase --> [89].

z --> z_lowercase.
z --> z_uppercase.

z_lowercase --> [122].

z_uppercase --> [90].

zero --> [48].

