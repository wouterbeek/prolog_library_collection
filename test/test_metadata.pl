:- module(
  test_metadata,
  [
    test_metadata/1 % ?Name:oneof([call_on_archive,openany])
  ]
).

/* <module> Test metadata

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dict_ext)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/open_any2)).





test_metadata(openany):-
  test_source(Source),
  setup_call_cleanup(
    open_any2(Source, read, _, Close_0, [metadata(M)]),
    print_dict(M),
    close_any2(Close_0)
  ).
test_metadata(call_on_archive):-
  test_source(Source),
  call_on_archive(Source, print_dict0).

print_dict0(M, _):-
  print_dict(M).

/*
test_source('http://csarven.ca/').
test_source('http://rdf.freebase.com/ns/m.012_t_').
test_source('http://datendienst.dnb.de/cgi-bin/mabit.pl?cmd=fetch&userID=opendata&pass=opendata&mabheft=ZDBTitel.ttl.gz').
test_source('http://prologmoo.com/downloads/mud.ttl').
test_source('http://almere.pilod.nl/sparql?default-graph-uri=&query=CONSTRUCT+%0D%0A%7B%0D%0A++++%3Fa+geo%3Ageometry+%3Fc+.%0D%0A%7D%0D%0AWHERE%0D%0A%7B+%3Fa+geo%3Ageometry+%3Fc+.%7D%0D%0A&should-sponge=&format=text%2Fturtle&timeout=0&debug=on').
test_source('http://almere.pilod.nl/sparql?default-graph-uri=&query=construct%0D%0A%7Bgraph+%3Fg+%7B%3Fs+%3Fp+%3Fo%7D%7D%0D%0A%7Bgraph+%3Fg%0D%0A%7B%0D%0A%3Fs+%3Fp+%3Fo%0D%0A%7D%0D%0AFILTER+regex%28str%28%3Fg%29%2C%22%5Ehttp%3A%2F%2Fbag.kadaster.nl%2F%22%29%0D%0A%7D+LIMIT+10000&should-sponge=&format=text%2Fturtle&timeout=0&debug=on').
*/
test_source('http://nxp.dydra.com/nxp/public-data.ttl').
test_source('https://www.dropbox.com/s/okzyej25j2aypkg/BEL2RDFexample.owl?dl=1').
test_source('http://54.213.4.161/lod/load-it/loadit.nq').
test_source('http://aemet.linkeddata.es/resource/WeatherStation/id08001').
test_source('http://aemet.linkeddata.es/resource/WeatherStation/id08001?output=ttl').
test_source('http://alaska.eagle-i.net/sparqler/sparql?view=published-resources&format=text/turtle&query=PREFIX+rdfs%3A+%3Chttp%3A%2F%2Fwww.w3.org%2F2000%2F01%2Frdf-schema%23%3E+PREFIX+%3A+%3Chttp%3A%2F%2Feagle-i.org%2Font%2Frepo%2F1.0%2F%3E+construct+%7B%3Fs+%3Fp+%3Fo+.+%3Fs+a+%3Ftype+.+%3Ftype+rdfs%3Alabel+%3Ftype_label+.+%3Fp+rdfs%3Alabel+%3Fp_label+.+%3Fo+rdfs%3Alabel+%3Fo_label%7D+where+%7Bgraph+%3ANG_Published%7B%3Fs+%3Fp+%3Fo%7D+.+optional%7B%3Fs+a+%3Ftype%7D+.+optional%7B%3Ftype+rdfs%3Alabel+%3Ftype_label%7D+.+optional%7B%3Fp+rdfs%3Alabel+%3Fp_label+%7D+.+optional%7B+%3Fo+rdfs%3Alabel+%3Fo_label%7D%7D+').
test_source('http://api.stlouisfed.org/').
test_source('http://arthroscopyportal.com/en/community/Arthroscopy/advanced-search/tag/Arthroscopy?rdf').
test_source('http://asn.jesandco.org/resources/S2377506.ttl').
test_source('http://athelia.com/comunidad/atheliasolutions/recurso/Django-Maintenance--Inspection/1ea18bcd-ce29-4443-86b7-485df3ef4d0a?rdf').
test_source('~/Git/Prolog-Library-Collection/doc/call_hierarchy.dot').
