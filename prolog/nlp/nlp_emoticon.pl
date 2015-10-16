:- module(
  nlp_emoticon,
  [
    emoticon//1 % -Meaning:string
  ]
).

/** <module> Some frequently observed emoticons

These are mostly useful for emoticons seen in prolog code.
Japanese (^_^) style emoticons not covered.

@author Anne Ogborn
@author Wouter Beek
@tbd various IM client icons.
@tbd Consider moving all this to a special annie emoticon dcg
@tbd Japanese (^_^) style emoticons
@version 2013, 2015/10
*/

:- use_module(library(dcg/dcg_content)).
:- use_module(library(lists)).
:- use_module(library(string_ext)).





%! emoticon(-Meaning:string)// .

emoticon("Annie is her usual happy self") --> "8cD".
emoticon(M) -->
  "-C:^{",
  mouth(M1),
  {string_concat("The General, ", M1, M)}.
emoticon(M) -->
  hat(M1),
  eyebrows(M2),
  eyes(M3),
  nose(M4),
  mouth(M5),
  {string_list_concat([M1,M2,M3,M4,M5], " ", M)}.
emoticon("darned if I know, half the time they confuse me too") --> ... .



%! eyebrows(-Meaning:string)// .

eyebrows("inner eyebrows down, expressing worry or consternation") --> ">".
eyebrows("eyebrows corrugated, expressing sternness") --> "|".



%! eyes(-Meaning:string)// .

eyes("") --> "8".
eyes("sunglasses") --> "B".
eyes("beady eyes") --> ":".
eyes("eyes closed or covered, in grief, disbelief, or pain") --> "X".
eyes("I'm a cyclops") --> "o".
eyes("wearing glasses") --> "88".
eyes("winking") --> ";".



%! hat(-Meaning:string)// .

hat("baseball cap worn sideways, or hat in general") --> "d".
hat("brain (brainy, or fried), or hair") --> "&".
hat("Carmen Miranda Hat") --> ")&$#|".
hat("Dunce cap, pointed head, or thinking cap") --> "<".
hat("Antenna") --> ">".
hat("hair standing on end") --> "=".



%! mouth(-Meaning:string)// .

mouth("kissing") --> "*".
mouth("puckered mouth") --> "+".
mouth("sad") --> "(".
mouth("happy, smile") --> ")".
mouth("toothy (maybe gap tooth dumb)") --> "E".
mouth("o mouth, suprise or shock") --> "o".
mouth("mouth agape, shock") --> "O".
mouth("stern or painful grimace") --> "[".
mouth("trying not to laugh, or smile") --> "]".
mouth("closed mouth look") --> "|".
mouth("discomfort, pain") --> "S".
mouth("tongue hanging out (delicious, distaste, or concentration)") --> "d".
mouth("Very happy - or default") --> "D".
mouth("Vampire fangs") --> "F".
mouth("variant mouth") --> "7".
mouth("puckered mouth - pain, good grief, or ouch") --> "X".
mouth("sticking out lower lip") --> "C".
mouth("variant smile") --> ">".
mouth("puzzlement, empathy, or concern") --> "?".
mouth("tongue hanging out (delicious, distaste, or concentration)") --> "p".
mouth("tongue hanging out (delicious, distaste, or concentration)") --> "P".
mouth("irony") --> "/".
mouth("irony") --> "\\".



%! nose(-Meaning:string)// .

nose("") --> "c".
nose("big nose") --> "C".
nose("button nose") --> "c:".
nose("non annie nose, reference to others") -->
  ("2" ; "^" ; "o" ; "-" ; "u" ; "U" ; "O" ; "v" ; "V" ; ">").
