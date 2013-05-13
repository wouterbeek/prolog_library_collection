:- module(
  iso_639_5,
  [
    iso_639_5_language_family/2 % ?ISO:atom
                                % ?Lexvo:uri
  ]
).

/** <module> ISO 639-5

The ISO 639-5 standard for language codes with Lexvo Semantic Web URIs.

WWW: [http://www.loc.gov/standards/iso639-5/]

@author Wouter Beek
@version 2013/01
*/



%! iso_639_5_language_family(?ISO:atom, ?Lexvo:uri) is nondet.

iso_639_5_language_family(ISO, Lexvo):-
  maplist(var, [ISO, Lexvo]),
  !,
  language_family0(ISO, _Name, Lexvo).
% All mappings are unique.
iso_639_5_language_family(ISO, Lexvo):-
  language_family0(ISO, _Name, Lexvo),
  !.

language_family0(aav, 'Austro-Asiatic', 'http://lexvo.org/id/iso639-5/aav').
language_family0(afa, 'Afro-Asiatic', 'http://lexvo.org/id/iso639-5/afa').
language_family0(alg, 'Algonquian', 'http://lexvo.org/id/iso639-5/alg').
language_family0(alv, 'Atlantic-Congo', 'http://lexvo.org/id/iso639-5/alv').
language_family0(apa, 'Apache', 'http://lexvo.org/id/iso639-5/apa').
language_family0(aqa, 'Alacalufan', 'http://lexvo.org/id/iso639-5/aqa').
language_family0(aql, 'Algic', 'http://lexvo.org/id/iso639-5/aql').
language_family0(art, 'Artificial', 'http://lexvo.org/id/iso639-5/art').
language_family0(ath, 'Athapascan', 'http://lexvo.org/id/iso639-5/ath').
language_family0(auf, 'Arauan', 'http://lexvo.org/id/iso639-5/auf').
language_family0(aus, 'Australian', 'http://lexvo.org/id/iso639-5/aus').
language_family0(awd, 'Arawakan', 'http://lexvo.org/id/iso639-5/awd').
language_family0(azc, 'Uto-Aztecan', 'http://lexvo.org/id/iso639-5/azc').
language_family0(bad, 'Banda', 'http://lexvo.org/id/iso639-5/bad').
language_family0(bai, 'Bamileke', 'http://lexvo.org/id/iso639-5/bai').
language_family0(bat, 'Baltic', 'http://lexvo.org/id/iso639-5/bat').
language_family0(ber, 'Berber', 'http://lexvo.org/id/iso639-5/ber').
language_family0(bnt, 'Bantu', 'http://lexvo.org/id/iso639-5/bnt').
language_family0(btk, 'Batak', 'http://lexvo.org/id/iso639-5/btk').
language_family0(cai, 'Central American Indian', 'http://lexvo.org/id/iso639-5/cai').
language_family0(cau, 'Caucasian', 'http://lexvo.org/id/iso639-5/cau').
language_family0(cba, 'Chibchan', 'http://lexvo.org/id/iso639-5/cba').
language_family0(ccn, 'North Caucasian', 'http://lexvo.org/id/iso639-5/ccn').
language_family0(ccs, 'South Caucasian', 'http://lexvo.org/id/iso639-5/ccs').
language_family0(cdc, 'Chadic', 'http://lexvo.org/id/iso639-5/cdc').
language_family0(cdd, 'Caddoan', 'http://lexvo.org/id/iso639-5/cdd').
language_family0(cel, 'Celtic', 'http://lexvo.org/id/iso639-5/cel').
language_family0(cmc, 'Chamic', 'http://lexvo.org/id/iso639-5/cmc').
language_family0(cpe, 'Creoles and pidgins, English‑based', 'http://lexvo.org/id/iso639-5/cpe').
language_family0(cpf, 'Creoles and pidgins, French‑based', 'http://lexvo.org/id/iso639-5/cpf').
language_family0(cpp, 'Creoles and pidgins, Portuguese-based', 'http://lexvo.org/id/iso639-5/cpp').
language_family0(crp, 'Creoles and pidgins', 'http://lexvo.org/id/iso639-5/crp').
language_family0(csu, 'Central Sudanic', 'http://lexvo.org/id/iso639-5/csu').
language_family0(cus, 'Cushitic', 'http://lexvo.org/id/iso639-5/cus').
language_family0(day, 'Land Dayak', 'http://lexvo.org/id/iso639-5/day').
language_family0(dmn, 'Mande', 'http://lexvo.org/id/iso639-5/dmn').
language_family0(dra, 'Dravidian', 'http://lexvo.org/id/iso639-5/dra').
language_family0(egx, 'Egyptian', 'http://lexvo.org/id/iso639-5/egx').
language_family0(esx, 'Eskimo-Aleut', 'http://lexvo.org/id/iso639-5/esx').
language_family0(euq, 'Basque', 'http://lexvo.org/id/iso639-5/euq').
language_family0(fiu, 'Finno-Ugrian', 'http://lexvo.org/id/iso639-5/fiu').
language_family0(fox, 'Formosan', 'http://lexvo.org/id/iso639-5/fox').
language_family0(gem, 'Germanic', 'http://lexvo.org/id/iso639-5/gem').
language_family0(gme, 'East Germanic', 'http://lexvo.org/id/iso639-5/gme').
language_family0(gmq, 'North Germanic', 'http://lexvo.org/id/iso639-5/gmq').
language_family0(gmw, 'West Germanic', 'http://lexvo.org/id/iso639-5/gmw').
language_family0(grk, 'Greek', 'http://lexvo.org/id/iso639-5/grk').
language_family0(hmx, 'Hmong-Mien', 'http://lexvo.org/id/iso639-5/hmx').
language_family0(hok, 'Hokan languages', 'http://lexvo.org/id/iso639-5/hok').
language_family0(hyx, 'Armenian', 'http://lexvo.org/id/iso639-5/hyx').
language_family0(iir, 'Indo-Iranian', 'http://lexvo.org/id/iso639-5/iir').
language_family0(ijo, 'Ijo', 'http://lexvo.org/id/iso639-5/ijo').
language_family0(inc, 'Indic', 'http://lexvo.org/id/iso639-5/inc').
language_family0(ine, 'Indo-European', 'http://lexvo.org/id/iso639-5/ine').
language_family0(ira, 'Iranian', 'http://lexvo.org/id/iso639-5/ira').
language_family0(iro, 'Iroquoian', 'http://lexvo.org/id/iso639-5/iro').
language_family0(itc, 'Italic', 'http://lexvo.org/id/iso639-5/itc').
language_family0(jpx, 'Japanese', 'http://lexvo.org/id/iso639-5/jpx').
language_family0(kar, 'Karen', 'http://lexvo.org/id/iso639-5/kar').
language_family0(kdo, 'Kordofanian', 'http://lexvo.org/id/iso639-5/kdo').
language_family0(khi, 'Khoisan', 'http://lexvo.org/id/iso639-5/khi').
language_family0(kro, 'Kru', 'http://lexvo.org/id/iso639-5/kro').
language_family0(map, 'Austronesian', 'http://lexvo.org/id/iso639-5/map').
language_family0(mkh, 'Mon-Khmer', 'http://lexvo.org/id/iso639-5/mkh').
language_family0(mno, 'Manobo', 'http://lexvo.org/id/iso639-5/mno').
language_family0(mun, 'Munda', 'http://lexvo.org/id/iso639-5/mun').
language_family0(myn, 'Mayan', 'http://lexvo.org/id/iso639-5/myn').
language_family0(nah, 'Nahuatl', 'http://lexvo.org/id/iso639-5/nah').
language_family0(nai, 'North American Indian', 'http://lexvo.org/id/iso639-5/nai').
language_family0(ngf, 'Trans-New Guinea', 'http://lexvo.org/id/iso639-5/ngf').
language_family0(nic, 'Niger-Kordofanian', 'http://lexvo.org/id/iso639-5/nic').
language_family0(nub, 'Nubian', 'http://lexvo.org/id/iso639-5/nub').
language_family0(omq, 'Oto-Manguean', 'http://lexvo.org/id/iso639-5/omq').
language_family0(omv, 'Omotic', 'http://lexvo.org/id/iso639-5/omv').
language_family0(oto, 'Otomian', 'http://lexvo.org/id/iso639-5/oto').
language_family0(paa, 'Papuan', 'http://lexvo.org/id/iso639-5/paa').
language_family0(phi, 'Philippine', 'http://lexvo.org/id/iso639-5/phi').
language_family0(plf, 'Central Malayo-Polynesian', 'http://lexvo.org/id/iso639-5/plf').
language_family0(poz, 'Malayo-Polynesian', 'http://lexvo.org/id/iso639-5/poz').
language_family0(pqe, 'Eastern Malayo-Polynesian', 'http://lexvo.org/id/iso639-5/pqe').
language_family0(pqw, 'Western Malayo-Polynesian', 'http://lexvo.org/id/iso639-5/pqw').
language_family0(pra, 'Prakrit', 'http://lexvo.org/id/iso639-5/pra').
language_family0(qwe, 'Quechuan', 'http://lexvo.org/id/iso639-5/qwe').
language_family0(roa, 'Romance', 'http://lexvo.org/id/iso639-5/roa').
language_family0(sai, 'South American Indian', 'http://lexvo.org/id/iso639-5/sai').
language_family0(sal, 'Salishan', 'http://lexvo.org/id/iso639-5/sal').
language_family0(sdv, 'Eastern Sudanic', 'http://lexvo.org/id/iso639-5/sdv').
language_family0(sem, 'Semitic', 'http://lexvo.org/id/iso639-5/sem').
language_family0(sgn, 'sign', 'http://lexvo.org/id/iso639-5/sgn').
language_family0(sio, 'Siouan', 'http://lexvo.org/id/iso639-5/sio').
language_family0(sit, 'Sino-Tibetan', 'http://lexvo.org/id/iso639-5/sit').
language_family0(sla, 'Slavic', 'http://lexvo.org/id/iso639-5/sla').
language_family0(smi, 'Sami', 'http://lexvo.org/id/iso639-5/smi').
language_family0(son, 'Songhai', 'http://lexvo.org/id/iso639-5/son').
language_family0(sqj, 'Albanian', 'http://lexvo.org/id/iso639-5/sqj').
language_family0(ssa, 'Nilo-Saharan', 'http://lexvo.org/id/iso639-5/ssa').
language_family0(syd, 'Samoyedic', 'http://lexvo.org/id/iso639-5/syd').
language_family0(tai, 'Tai', 'http://lexvo.org/id/iso639-5/tai').
language_family0(tbq, 'Tibeto-Burman', 'http://lexvo.org/id/iso639-5/tbq').
language_family0(trk, 'Turkic', 'http://lexvo.org/id/iso639-5/trk').
language_family0(tup, 'Tupi', 'http://lexvo.org/id/iso639-5/tup').
language_family0(tut, 'Altaic', 'http://lexvo.org/id/iso639-5/tut').
language_family0(tuw, 'Tungus', 'http://lexvo.org/id/iso639-5/tuw').
language_family0(urj, 'Uralic', 'http://lexvo.org/id/iso639-5/urj').
language_family0(wak, 'Wakashan', 'http://lexvo.org/id/iso639-5/wak').
language_family0(wen, 'Sorbian', 'http://lexvo.org/id/iso639-5/wen').
language_family0(xgn, 'Mongolian', 'http://lexvo.org/id/iso639-5/xgn').
language_family0(xnd, 'Na-Dene', 'http://lexvo.org/id/iso639-5/xnd').
language_family0(ypk, 'Yupik', 'http://lexvo.org/id/iso639-5/ypk').
language_family0(zhx, 'Chinese', 'http://lexvo.org/id/iso639-5/zhx').
language_family0(zle, 'East Slavic', 'http://lexvo.org/id/iso639-5/zle').
language_family0(zls, 'South Slavic', 'http://lexvo.org/id/iso639-5/zls').
language_family0(zlw, 'West Slavic', 'http://lexvo.org/id/iso639-5/zlw').
language_family0(znd, 'Zande', 'http://lexvo.org/id/iso639-5/znd').

