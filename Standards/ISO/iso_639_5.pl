:- module(
  iso_639_5,
  [
    iso_639_5/3 % ?ISO:atom
                % ?Name:atom
                % ?Lexvo:uri
  ]
).

/** <module> ISO 639-5

The ISO 639-5 standard for language codes with Lexvo Semantic Web URIs.

WWW: [http://www.loc.gov/standards/iso639-5/]

@author Wouter Beek
@version 2013/01, 2013/06
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(iso_639_5, 'http://lexvo.org/id/iso639-5/').

:- rdf_meta(iso_639_5(?,?,r)).



iso_639_5(afa, 'Afro-Asiatic', iso_639_5:afa).
iso_639_5(alg, 'Algonquian', iso_639_5:alg).
iso_639_5(alv, 'Atlantic-Congo', iso_639_5:alv).
iso_639_5(apa, 'Apache', iso_639_5:apa).
iso_639_5(aqa, 'Alacalufan', iso_639_5:aqa).
iso_639_5(aql, 'Algic', iso_639_5:aql).
iso_639_5(art, 'Artificial', iso_639_5:art).
iso_639_5(ath, 'Athapascan', iso_639_5:ath).
iso_639_5(auf, 'Arauan', iso_639_5:auf).
iso_639_5(aus, 'Australian', iso_639_5:aus).
iso_639_5(awd, 'Arawakan', iso_639_5:awd).
iso_639_5(azc, 'Uto-Aztecan', iso_639_5:azc).
iso_639_5(bad, 'Banda', iso_639_5:bad).
iso_639_5(bai, 'Bamileke', iso_639_5:bai).
iso_639_5(bat, 'Baltic', iso_639_5:bat).
iso_639_5(ber, 'Berber', iso_639_5:ber).
iso_639_5(bnt, 'Bantu', iso_639_5:bnt).
iso_639_5(btk, 'Batak', iso_639_5:btk).
iso_639_5(cai, 'Central American Indian', iso_639_5:cai).
iso_639_5(cau, 'Caucasian', iso_639_5:cau).
iso_639_5(cba, 'Chibchan', iso_639_5:cba).
iso_639_5(ccn, 'North Caucasian', iso_639_5:ccn).
iso_639_5(ccs, 'South Caucasian', iso_639_5:ccs).
iso_639_5(cdc, 'Chadic', iso_639_5:cdc).
iso_639_5(cdd, 'Caddoan', iso_639_5:cdd).
iso_639_5(cel, 'Celtic', iso_639_5:cel).
iso_639_5(cmc, 'Chamic', iso_639_5:cmc).
iso_639_5(cpe, 'Creoles and pidgins, English‑based', iso_639_5:cpe).
iso_639_5(cpf, 'Creoles and pidgins, French‑based', iso_639_5:cpf).
iso_639_5(cpp, 'Creoles and pidgins, Portuguese-based', iso_639_5:cpp).
iso_639_5(crp, 'Creoles and pidgins', iso_639_5:crp).
iso_639_5(csu, 'Central Sudanic', iso_639_5:csu).
iso_639_5(cus, 'Cushitic', iso_639_5:cus).
iso_639_5(day, 'Land Dayak', iso_639_5:day).
iso_639_5(dmn, 'Mande', iso_639_5:dmn).
iso_639_5(dra, 'Dravidian', iso_639_5:dra).
iso_639_5(egx, 'Egyptian', iso_639_5:egx).
iso_639_5(esx, 'Eskimo-Aleut', iso_639_5:esx).
iso_639_5(euq, 'Basque', iso_639_5:euq).
iso_639_5(fiu, 'Finno-Ugrian', iso_639_5:fiu).
iso_639_5(fox, 'Formosan', iso_639_5:fox).
iso_639_5(gem, 'Germanic', iso_639_5:gem).
iso_639_5(gme, 'East Germanic', iso_639_5:gme).
iso_639_5(gmq, 'North Germanic', iso_639_5:gmq).
iso_639_5(gmw, 'West Germanic', iso_639_5:gmw).
iso_639_5(grk, 'Greek', iso_639_5:grk).
iso_639_5(hmx, 'Hmong-Mien', iso_639_5:hmx).
iso_639_5(hok, 'Hokan languages', iso_639_5:hok).
iso_639_5(hyx, 'Armenian', iso_639_5:hyx).
iso_639_5(iir, 'Indo-Iranian', iso_639_5:iir).
iso_639_5(ijo, 'Ijo', iso_639_5:ijo).
iso_639_5(inc, 'Indic', iso_639_5:inc).
iso_639_5(ine, 'Indo-European', iso_639_5:ine).
iso_639_5(ira, 'Iranian', iso_639_5:ira).
iso_639_5(iro, 'Iroquoian', iso_639_5:iro).
iso_639_5(itc, 'Italic', iso_639_5:itc).
iso_639_5(jpx, 'Japanese', iso_639_5:jpx).
iso_639_5(kar, 'Karen', iso_639_5:kar).
iso_639_5(kdo, 'Kordofanian', iso_639_5:kdo).
iso_639_5(khi, 'Khoisan', iso_639_5:khi).
iso_639_5(kro, 'Kru', iso_639_5:kro).
iso_639_5(map, 'Austronesian', iso_639_5:map).
iso_639_5(mkh, 'Mon-Khmer', iso_639_5:mkh).
iso_639_5(mno, 'Manobo', iso_639_5:mno).
iso_639_5(mun, 'Munda', iso_639_5:mun).
iso_639_5(myn, 'Mayan', iso_639_5:myn).
iso_639_5(nah, 'Nahuatl', iso_639_5:nah).
iso_639_5(nai, 'North American Indian', iso_639_5:nai).
iso_639_5(ngf, 'Trans-New Guinea', iso_639_5:ngf).
iso_639_5(nic, 'Niger-Kordofanian', iso_639_5:nic).
iso_639_5(nub, 'Nubian', iso_639_5:nub).
iso_639_5(omq, 'Oto-Manguean', iso_639_5:omq).
iso_639_5(omv, 'Omotic', iso_639_5:omv).
iso_639_5(oto, 'Otomian', iso_639_5:oto).
iso_639_5(paa, 'Papuan', iso_639_5:paa).
iso_639_5(phi, 'Philippine', iso_639_5:phi).
iso_639_5(plf, 'Central Malayo-Polynesian', iso_639_5:plf).
iso_639_5(poz, 'Malayo-Polynesian', iso_639_5:poz).
iso_639_5(pqe, 'Eastern Malayo-Polynesian', iso_639_5:pqe).
iso_639_5(pqw, 'Western Malayo-Polynesian', iso_639_5:pqw).
iso_639_5(pra, 'Prakrit', iso_639_5:pra).
iso_639_5(qwe, 'Quechuan', iso_639_5:qwe).
iso_639_5(roa, 'Romance', iso_639_5:roa).
iso_639_5(sai, 'South American Indian', iso_639_5:sai).
iso_639_5(sal, 'Salishan', iso_639_5:sal).
iso_639_5(sdv, 'Eastern Sudanic', iso_639_5:sdv).
iso_639_5(sem, 'Semitic', iso_639_5:sem).
iso_639_5(sgn, 'sign', iso_639_5:sgn).
iso_639_5(sio, 'Siouan', iso_639_5:sio).
iso_639_5(sit, 'Sino-Tibetan', iso_639_5:sit).
iso_639_5(sla, 'Slavic', iso_639_5:sla).
iso_639_5(smi, 'Sami', iso_639_5:smi).
iso_639_5(son, 'Songhai', iso_639_5:son).
iso_639_5(sqj, 'Albanian', iso_639_5:sqj).
iso_639_5(ssa, 'Nilo-Saharan', iso_639_5:ssa).
iso_639_5(syd, 'Samoyedic', iso_639_5:syd).
iso_639_5(tai, 'Tai', iso_639_5:tai).
iso_639_5(tbq, 'Tibeto-Burman', iso_639_5:tbq).
iso_639_5(trk, 'Turkic', iso_639_5:trk).
iso_639_5(tup, 'Tupi', iso_639_5:tup).
iso_639_5(tut, 'Altaic', iso_639_5:tut).
iso_639_5(tuw, 'Tungus', iso_639_5:tuw).
iso_639_5(urj, 'Uralic', iso_639_5:urj).
iso_639_5(wak, 'Wakashan', iso_639_5:wak).
iso_639_5(wen, 'Sorbian', iso_639_5:wen).
iso_639_5(xgn, 'Mongolian', iso_639_5:xgn).
iso_639_5(xnd, 'Na-Dene', iso_639_5:xnd).
iso_639_5(ypk, 'Yupik', iso_639_5:ypk).
iso_639_5(zhx, 'Chinese', iso_639_5:zhx).
iso_639_5(zle, 'East Slavic', iso_639_5:zle).
iso_639_5(zls, 'South Slavic', iso_639_5:zls).
iso_639_5(zlw, 'West Slavic', iso_639_5:zlw).
iso_639_5(znd, 'Zande', iso_639_5:znd).

