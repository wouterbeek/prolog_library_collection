:- module(
  iso_639_1,
  [
    iso_639_1/2 % ?ISO:atom
                % ?Lexvo:uri
  ]
).

/** <module> ISO 639-1

The ISO 639-1 standard for language codes with Lexvo Semantic Web URIs.

!!iso_639_1!!language!!2!!

WWW: [http://www.sil.org/iso639-1/]

@author Wouter Beek
@version 2013/01, 2013/06
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(iso_639_3, 'http://lexvo.org/id/iso639-3/').

:- rdf_meta(iso_639_1(?,r)).



%! iso_639_1(?ISO:atom, ?Lexvo:uri) is nondet.

iso_639_1(aa, iso_639_3:aar).
iso_639_1(ab, iso_639_3:abk).
iso_639_1(ae, iso_639_3:ave).
iso_639_1(af, iso_639_3:afr).
iso_639_1(ak, iso_639_3:aka).
iso_639_1(am, iso_639_3:amh).
iso_639_1(an, iso_639_3:arg).
iso_639_1(ar, iso_639_3:ara).
iso_639_1(as, iso_639_3:asm).
iso_639_1(av, iso_639_3:ava).
iso_639_1(ay, iso_639_3:aym).
iso_639_1(az, iso_639_3:aze).
iso_639_1(ba, iso_639_3:bak).
iso_639_1(be, iso_639_3:bel).
iso_639_1(bg, iso_639_3:bul).
iso_639_1(bi, iso_639_3:bis).
iso_639_1(bm, iso_639_3:bam).
iso_639_1(bn, iso_639_3:ben).
iso_639_1(bo, iso_639_3:bod).
iso_639_1(br, iso_639_3:bre).
iso_639_1(bs, iso_639_3:bos).
iso_639_1(ca, iso_639_3:cat).
iso_639_1(ce, iso_639_3:che).
iso_639_1(ch, iso_639_3:cha).
iso_639_1(co, iso_639_3:cos).
iso_639_1(cr, iso_639_3:cre).
iso_639_1(cs, iso_639_3:ces).
iso_639_1(cu, iso_639_3:chu).
iso_639_1(cv, iso_639_3:chv).
iso_639_1(cy, iso_639_3:cym).
iso_639_1(da, iso_639_3:dan).
iso_639_1(de, iso_639_3:deu).
iso_639_1(dv, iso_639_3:div).
iso_639_1(dz, iso_639_3:dzo).
iso_639_1(ee, iso_639_3:ewe).
iso_639_1(el, iso_639_3:ell).
iso_639_1(en, iso_639_3:eng).
iso_639_1(eo, iso_639_3:epo).
iso_639_1(es, iso_639_3:spa).
iso_639_1(et, iso_639_3:est).
iso_639_1(eu, iso_639_3:eus).
iso_639_1(fa, iso_639_3:fas).
iso_639_1(ff, iso_639_3:ful).
iso_639_1(fi, iso_639_3:fin).
iso_639_1(fj, iso_639_3:fij).
iso_639_1(fo, iso_639_3:fao).
iso_639_1(fr, iso_639_3:fra).
iso_639_1(fy, iso_639_3:fry).
iso_639_1(ga, iso_639_3:gle).
iso_639_1(gd, iso_639_3:gla).
iso_639_1(gl, iso_639_3:glg).
iso_639_1(gn, iso_639_3:grn).
iso_639_1(gu, iso_639_3:guj).
iso_639_1(gv, iso_639_3:glv).
iso_639_1(ha, iso_639_3:hau).
iso_639_1(he, iso_639_3:heb).
iso_639_1(hi, iso_639_3:hin).
iso_639_1(ho, iso_639_3:hmo).
iso_639_1(hr, iso_639_3:hrv).
iso_639_1(ht, iso_639_3:hat).
iso_639_1(hu, iso_639_3:hun).
iso_639_1(hy, iso_639_3:hye).
iso_639_1(hz, iso_639_3:her).
iso_639_1(ia, iso_639_3:ina).
iso_639_1(id, iso_639_3:ind).
iso_639_1(ie, iso_639_3:ile).
iso_639_1(ig, iso_639_3:ibo).
iso_639_1(ii, iso_639_3:iii).
iso_639_1(ik, iso_639_3:ipk).
iso_639_1(io, iso_639_3:ido).
iso_639_1(is, iso_639_3:isl).
iso_639_1(it, iso_639_3:ita).
iso_639_1(iu, iso_639_3:iku).
iso_639_1(ja, iso_639_3:jpn).
iso_639_1(jv, iso_639_3:jav).
iso_639_1(ka, iso_639_3:kat).
iso_639_1(kg, iso_639_3:kon).
iso_639_1(ki, iso_639_3:kik).
iso_639_1(kj, iso_639_3:kua).
iso_639_1(kk, iso_639_3:kaz).
iso_639_1(kl, iso_639_3:kal).
iso_639_1(km, iso_639_3:khm).
iso_639_1(kn, iso_639_3:kan).
iso_639_1(ko, iso_639_3:kor).
iso_639_1(kr, iso_639_3:kau).
iso_639_1(ks, iso_639_3:kas).
iso_639_1(ku, iso_639_3:kur).
iso_639_1(kv, iso_639_3:kom).
iso_639_1(kw, iso_639_3:cor).
iso_639_1(ky, iso_639_3:kir).
iso_639_1(la, iso_639_3:lat).
iso_639_1(lb, iso_639_3:ltz).
iso_639_1(lg, iso_639_3:lug).
iso_639_1(li, iso_639_3:lim).
iso_639_1(ln, iso_639_3:lin).
iso_639_1(lo, iso_639_3:lao).
iso_639_1(lt, iso_639_3:lit).
iso_639_1(lu, iso_639_3:lub).
iso_639_1(lv, iso_639_3:lav).
iso_639_1(mg, iso_639_3:mlg).
iso_639_1(mh, iso_639_3:mah).
iso_639_1(mi, iso_639_3:mri).
iso_639_1(mk, iso_639_3:mkd).
iso_639_1(ml, iso_639_3:mal).
iso_639_1(mn, iso_639_3:mon).
iso_639_1(mr, iso_639_3:mar).
iso_639_1(ms, iso_639_3:msa).
iso_639_1(mt, iso_639_3:mlt).
iso_639_1(my, iso_639_3:mya).
iso_639_1(na, iso_639_3:nau).
iso_639_1(nb, iso_639_3:nob).
iso_639_1(nd, iso_639_3:nde).
iso_639_1(ne, iso_639_3:nep).
iso_639_1(ng, iso_639_3:ndo).
iso_639_1(nl, iso_639_3:nld).
iso_639_1(nn, iso_639_3:nno).
iso_639_1(no, iso_639_3:nor).
iso_639_1(nr, iso_639_3:nbl).
iso_639_1(nv, iso_639_3:nav).
iso_639_1(ny, iso_639_3:nya).
iso_639_1(oc, iso_639_3:oci).
iso_639_1(oj, iso_639_3:oji).
iso_639_1(om, iso_639_3:orm).
iso_639_1(or, iso_639_3:ori).
iso_639_1(os, iso_639_3:oss).
iso_639_1(pa, iso_639_3:pan).
iso_639_1(pi, iso_639_3:pli).
iso_639_1(pl, iso_639_3:pol).
iso_639_1(ps, iso_639_3:pus).
iso_639_1(pt, iso_639_3:por).
iso_639_1(qu, iso_639_3:que).
iso_639_1(rm, iso_639_3:roh).
iso_639_1(rn, iso_639_3:run).
iso_639_1(ro, iso_639_3:ron).
iso_639_1(ru, iso_639_3:rus).
iso_639_1(rw, iso_639_3:kin).
iso_639_1(sa, iso_639_3:san).
iso_639_1(sc, iso_639_3:srd).
iso_639_1(sd, iso_639_3:snd).
iso_639_1(se, iso_639_3:sme).
iso_639_1(sg, iso_639_3:sag).
iso_639_1(sh, iso_639_3:hbs).
iso_639_1(si, iso_639_3:sin).
iso_639_1(sk, iso_639_3:slk).
iso_639_1(sl, iso_639_3:slv).
iso_639_1(sm, iso_639_3:smo).
iso_639_1(sn, iso_639_3:sna).
iso_639_1(so, iso_639_3:som).
iso_639_1(sq, iso_639_3:sqi).
iso_639_1(sr, iso_639_3:srp).
iso_639_1(ss, iso_639_3:ssw).
iso_639_1(st, iso_639_3:sot).
iso_639_1(su, iso_639_3:sun).
iso_639_1(sv, iso_639_3:swe).
iso_639_1(sw, iso_639_3:swa).
iso_639_1(ta, iso_639_3:tam).
iso_639_1(te, iso_639_3:tel).
iso_639_1(tg, iso_639_3:tgk).
iso_639_1(th, iso_639_3:tha).
iso_639_1(ti, iso_639_3:tir).
iso_639_1(tk, iso_639_3:tuk).
iso_639_1(tl, iso_639_3:tgl).
iso_639_1(tn, iso_639_3:tsn).
iso_639_1(to, iso_639_3:ton).
iso_639_1(tr, iso_639_3:tur).
iso_639_1(ts, iso_639_3:tso).
iso_639_1(tt, iso_639_3:tat).
iso_639_1(tw, iso_639_3:twi).
iso_639_1(ty, iso_639_3:tah).
iso_639_1(ug, iso_639_3:uig).
iso_639_1(uk, iso_639_3:ukr).
iso_639_1(ur, iso_639_3:urd).
iso_639_1(uz, iso_639_3:uzb).
iso_639_1(ve, iso_639_3:ven).
iso_639_1(vi, iso_639_3:vie).
iso_639_1(vo, iso_639_3:vol).
iso_639_1(wa, iso_639_3:wln).
iso_639_1(wo, iso_639_3:wol).
iso_639_1(xh, iso_639_3:xho).
iso_639_1(yi, iso_639_3:yid).
iso_639_1(yo, iso_639_3:yor).
iso_639_1(za, iso_639_3:zha).
iso_639_1(zh, iso_639_3:zho).
iso_639_1(zu, iso_639_3:zul).

