:- module(
  nlp_lang,
  [
    current_lrange/1, % -LRange
    current_ltag/1,   % -LTag
    current_ltag/2,   % +LTags, -LTag
    lstring/2,        % +Name, -Str
    lstring/3         % +Name, +Args, -Str
  ]
).

/** <module> NLP: Language setting

@author Wouter Beek
@version 2016/02, 2016/04, 2016/06, 2016/09
*/

:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(ltag/ltag_match)).
:- use_module(library(settings)).

:- setting(nlp:lrange, list(atom), [en], '').

:- multifile
    nlp:lstring/3.

nlp:lstring(en, add_article, "Add article").
nlp:lstring(nl, add_article, "Voeg artikel toe").
nlp:lstring(en, article, "Article").
nlp:lstring(nl, article, "Artikel").
nlp:lstring(en, articles, "Articles").
nlp:lstring(nl, articles, "Artikelen").
nlp:lstring(en, articles_tagged_with, "Articles tagged with").
nlp:lstring(nl, articles_tagged_with, "Artikelen met tag").
nlp:lstring(en, author, "Author").
nlp:lstring(nl, author, "Auteur").
nlp:lstring(en, commented_on, "Commented on").
nlp:lstring(nl, commented_on, "Gereageerd op").
nlp:lstring(en, comments, "Comments").
nlp:lstring(nl, comments, "Reacties").
nlp:lstring(en, developed_with, "Developed with").
nlp:lstring(nl, developed_with, "Ontwikkeld met").
nlp:lstring(en, edit, "Edit").
nlp:lstring(nl, edit, "Bewerk").
nlp:lstring(en, edit_mode, "Edit mode").
nlp:lstring(nl, edit_mode, "Bewerken").
nlp:lstring(en, editing, "Editing").
nlp:lstring(nl, editing, "Bewerken").
nlp:lstring(en, en, "English").
nlp:lstring(nl, en, "Engels").
nlp:lstring(en, follow, "Follow").
nlp:lstring(nl, follow, "Volg").
nlp:lstring(en, follow_us_on_x, "Follow us on ~s").
nlp:lstring(nl, follow_us_on_x, "Volg ons op ~s").
nlp:lstring(en, language, "Language").
nlp:lstring(nl, language, "Taal").
nlp:lstring(en, like_us_on_x, "Like us on ~s").
nlp:lstring(nl, like_us_on_x, "Like ons op ~s").
nlp:lstring(en, new, "New").
nlp:lstring(nl, new, "Nieuw").
nlp:lstring(en, nl, "Dutch").
nlp:lstring(nl, nl, "Nederlands").
nlp:lstring(en, on, "on").
nlp:lstring(nl, on, "op").
nlp:lstring(en, overview, "Overview").
nlp:lstring(nl, overview, "Overzicht").
nlp:lstring(en, read_more, "Read more").
nlp:lstring(nl, read_more, "Lees meer").
nlp:lstring(en, share_x_on_y, "Share “~s” on ~s.").
nlp:lstring(nl, share_x_on_y, "Deel “~s” op ~s.").
nlp:lstring(en, tag, "Tag").
nlp:lstring(nl, tag, "Tag").
nlp:lstring(en, tags, "Tags").
nlp:lstring(nl, tags, "Tags").
nlp:lstring(en, tweet_to, "Tweet to").
nlp:lstring(nl, tweet_to, "Tweet naar").
nlp:lstring(en, tweets_by, "Tweets by").
nlp:lstring(nl, tweets_by, "Tweets van").
nlp:lstring(en, view_all_posts_tagged_with, "View all posts tagged with").
nlp:lstring(nl, view_all_posts_tagged_with, "Zie alle posts met tag").
nlp:lstring(en, votes, "votes").
nlp:lstring(nl, votes, "stemmen").
nlp:lstring(en, written_by, "Written by").
nlp:lstring(nl, written_by, "Geschreven door").
nlp:lstring(en, you, "You").
nlp:lstring(nl, you, "Jij").



%! current_lrange(-LRange) is det.

current_lrange(LRange) :-
  nlp:setting(lrange, LRange).



%! current_ltag(-LTag) is det.

current_ltag(LTag) :-
  current_lrange(LRange),
  lrange_to_ltag(LRange, LTag).


%! current_ltag(+LTags, -LTag) is det.

current_ltag(LTags, LTag) :-
  current_ltag(LTag),
  memberchk(LTag, LTags).



%! lrange_to_ltag(+LRange, -LTag) is nondet.

lrange_to_ltag(LRange, LTag2) :-
  member(LTag1, LRange),
  atomic_list_concat(Subtags, -, LTag1),
  longest_to_shortest_prefix0(PrefixSubtags, Subtags),
  PrefixSubtags \== [],
  atomic_list_concat(PrefixSubtags, -, LTag2).

longest_to_shortest_prefix0(L, L).
longest_to_shortest_prefix0(Prefix, L1) :-
  append(L2, [_], L1),
  longest_to_shortest_prefix0(Prefix, L2).



%! lstring(+Name, -Str) is det.
%! lstring(+Name, +Args, -Str) is det.

lstring(Name, Str) :-
  lstring(Name, [], Str).


lstring(Name, Args, Str) :-
  current_ltag(LTag),
  nlp:lstring(LTag, Name, Format),
  format(string(Str), Format, Args).
