:- multifile
    nlp:nlp_string/3.

nlp:nlp_string(en, add_article, "Add article").
nlp:nlp_string(nl, add_article, "Voeg artikel toe").
nlp:nlp_string(en, article, "Article").
nlp:nlp_string(nl, article, "Artikel").
nlp:nlp_string(en, articles, "Articles").
nlp:nlp_string(nl, articles, "Artikelen").
nlp:nlp_string(en, articles_tagged_with, "Articles tagged with").
nlp:nlp_string(nl, articles_tagged_with, "Artikelen met tag").
nlp:nlp_string(en, author, "Author").
nlp:nlp_string(nl, author, "Auteur").
nlp:nlp_string(en, commented_on, "Commented on").
nlp:nlp_string(nl, commented_on, "Gereageerd op").
nlp:nlp_string(en, comments, "Comments").
nlp:nlp_string(nl, comments, "Reacties").
nlp:nlp_string(en, developed_with, "Developed with").
nlp:nlp_string(nl, developed_with, "Ontwikkeld met").
nlp:nlp_string(en, edit, "Edit").
nlp:nlp_string(nl, edit, "Bewerk").
nlp:nlp_string(en, edit_mode, "Edit mode").
nlp:nlp_string(nl, edit_mode, "Bewerken").
nlp:nlp_string(en, editing, "Editing").
nlp:nlp_string(nl, editing, "Bewerken").
nlp:nlp_string(en, en, "English").
nlp:nlp_string(nl, en, "Engels").
nlp:nlp_string(en, follow, "Follow").
nlp:nlp_string(nl, follow, "Volg").
nlp:nlp_string(en, language, "Language").
nlp:nlp_string(nl, language, "Taal").
nlp:nlp_string(en, like_us_on_x, "Like us on ~s").
nlp:nlp_string(nl, like_us_on_x, "Like ons op ~s").
nlp:nlp_string(en, new, "New").
nlp:nlp_string(nl, new, "Nieuw").
nlp:nlp_string(en, nl, "Dutch").
nlp:nlp_string(nl, nl, "Nederlands").
nlp:nlp_string(en, on, "on").
nlp:nlp_string(nl, on, "op").
nlp:nlp_string(en, overview, "Overview").
nlp:nlp_string(nl, overview, "Overzicht").
nlp:nlp_string(en, read_more, "Read more").
nlp:nlp_string(nl, read_more, "Lees meer").
nlp:nlp_string(en, share_x_on_y, "Share “~s” on ~s.").
nlp:nlp_string(nl, share_x_on_y, "Deel “~s” op ~s.").
nlp:nlp_string(en, tag, "Tag").
nlp:nlp_string(nl, tag, "Tag").
nlp:nlp_string(en, tags, "Tags").
nlp:nlp_string(nl, tags, "Tags").
nlp:nlp_string(en, tweet_to, "Tweet to").
nlp:nlp_string(nl, tweet_to, "Tweet naar").
nlp:nlp_string(en, tweets_by, "Tweets by").
nlp:nlp_string(nl, tweets_by, "Tweets van").
nlp:nlp_string(en, view_all_posts_tagged_with, "View all posts tagged with").
nlp:nlp_string(nl, view_all_posts_tagged_with, "Zie alle posts met tag").
nlp:nlp_string(en, votes, "votes").
nlp:nlp_string(nl, votes, "stemmen").
nlp:nlp_string(en, written_by, "Written by").
nlp:nlp_string(nl, written_by, "Geschreven door").
nlp:nlp_string(en, you, "You").
nlp:nlp_string(nl, you, "Jij").



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
