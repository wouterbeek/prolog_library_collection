:- dynamic(author/2).
:- dynamic(download/1).
:- dynamic(home/1).
:- dynamic(maintainer/2).
:- dynamic(name/1).
:- dynamic(packager/2).
:- dynamic(requires/1).
:- dynamic(title/1).
:- dynamic(version/1).

author('Wouter Beek', 'me@wouterbeek.com').
download('https://github.com/wouterbeek/Prolog_Library_Collection/release/*.zip').
home('https://github.com/wouterbeek/Prolog_Library_Collection').
maintainer('Wouter Beek', 'me@wouterbeek.com').
name('Prolog_Library_Collection').
packager('Wouter Beek', 'me@wouterbeek.com').
requires(lambda).
title('A collection of SWI-Prolog libraries.').
version('0.0.54').
