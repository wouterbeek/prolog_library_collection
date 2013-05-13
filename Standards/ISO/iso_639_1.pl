:- module(
  iso_639_1,
  [
    iso_639_1_language/2 % ?ISO:atom
                         % ?Lexvo:uri
  ]
).

/** <module> ISO 639-1

The ISO 639-1 standard for language codes with Lexvo Semantic Web URIs.

!!iso_639_1!!language!!2!!

WWW: [http://www.sil.org/iso639-1/]

@author Wouter Beek
@version 2013/01
*/



iso_639_1_language(ISO, Lexvo):-
  language0(ISO, Lexvo).

%! language0(?ISO:atom, ?Lexvo:uri) is nondet.

language0(aa, 'http://lexvo.org/id/iso639-3/aar').
language0(ab, 'http://lexvo.org/id/iso639-3/abk').
language0(ae, 'http://lexvo.org/id/iso639-3/ave').
language0(af, 'http://lexvo.org/id/iso639-3/afr').
language0(ak, 'http://lexvo.org/id/iso639-3/aka').
language0(am, 'http://lexvo.org/id/iso639-3/amh').
language0(an, 'http://lexvo.org/id/iso639-3/arg').
language0(ar, 'http://lexvo.org/id/iso639-3/ara').
language0(as, 'http://lexvo.org/id/iso639-3/asm').
language0(av, 'http://lexvo.org/id/iso639-3/ava').
language0(ay, 'http://lexvo.org/id/iso639-3/aym').
language0(az, 'http://lexvo.org/id/iso639-3/aze').
language0(ba, 'http://lexvo.org/id/iso639-3/bak').
language0(be, 'http://lexvo.org/id/iso639-3/bel').
language0(bg, 'http://lexvo.org/id/iso639-3/bul').
language0(bi, 'http://lexvo.org/id/iso639-3/bis').
language0(bm, 'http://lexvo.org/id/iso639-3/bam').
language0(bn, 'http://lexvo.org/id/iso639-3/ben').
language0(bo, 'http://lexvo.org/id/iso639-3/bod').
language0(br, 'http://lexvo.org/id/iso639-3/bre').
language0(bs, 'http://lexvo.org/id/iso639-3/bos').
language0(ca, 'http://lexvo.org/id/iso639-3/cat').
language0(ce, 'http://lexvo.org/id/iso639-3/che').
language0(ch, 'http://lexvo.org/id/iso639-3/cha').
language0(co, 'http://lexvo.org/id/iso639-3/cos').
language0(cr, 'http://lexvo.org/id/iso639-3/cre').
language0(cs, 'http://lexvo.org/id/iso639-3/ces').
language0(cu, 'http://lexvo.org/id/iso639-3/chu').
language0(cv, 'http://lexvo.org/id/iso639-3/chv').
language0(cy, 'http://lexvo.org/id/iso639-3/cym').
language0(da, 'http://lexvo.org/id/iso639-3/dan').
language0(de, 'http://lexvo.org/id/iso639-3/deu').
language0(dv, 'http://lexvo.org/id/iso639-3/div').
language0(dz, 'http://lexvo.org/id/iso639-3/dzo').
language0(ee, 'http://lexvo.org/id/iso639-3/ewe').
language0(el, 'http://lexvo.org/id/iso639-3/ell').
language0(en, 'http://lexvo.org/id/iso639-3/eng').
language0(eo, 'http://lexvo.org/id/iso639-3/epo').
language0(es, 'http://lexvo.org/id/iso639-3/spa').
language0(et, 'http://lexvo.org/id/iso639-3/est').
language0(eu, 'http://lexvo.org/id/iso639-3/eus').
language0(fa, 'http://lexvo.org/id/iso639-3/fas').
language0(ff, 'http://lexvo.org/id/iso639-3/ful').
language0(fi, 'http://lexvo.org/id/iso639-3/fin').
language0(fj, 'http://lexvo.org/id/iso639-3/fij').
language0(fo, 'http://lexvo.org/id/iso639-3/fao').
language0(fr, 'http://lexvo.org/id/iso639-3/fra').
language0(fy, 'http://lexvo.org/id/iso639-3/fry').
language0(ga, 'http://lexvo.org/id/iso639-3/gle').
language0(gd, 'http://lexvo.org/id/iso639-3/gla').
language0(gl, 'http://lexvo.org/id/iso639-3/glg').
language0(gn, 'http://lexvo.org/id/iso639-3/grn').
language0(gu, 'http://lexvo.org/id/iso639-3/guj').
language0(gv, 'http://lexvo.org/id/iso639-3/glv').
language0(ha, 'http://lexvo.org/id/iso639-3/hau').
language0(he, 'http://lexvo.org/id/iso639-3/heb').
language0(hi, 'http://lexvo.org/id/iso639-3/hin').
language0(ho, 'http://lexvo.org/id/iso639-3/hmo').
language0(hr, 'http://lexvo.org/id/iso639-3/hrv').
language0(ht, 'http://lexvo.org/id/iso639-3/hat').
language0(hu, 'http://lexvo.org/id/iso639-3/hun').
language0(hy, 'http://lexvo.org/id/iso639-3/hye').
language0(hz, 'http://lexvo.org/id/iso639-3/her').
language0(ia, 'http://lexvo.org/id/iso639-3/ina').
language0(id, 'http://lexvo.org/id/iso639-3/ind').
language0(ie, 'http://lexvo.org/id/iso639-3/ile').
language0(ig, 'http://lexvo.org/id/iso639-3/ibo').
language0(ii, 'http://lexvo.org/id/iso639-3/iii').
language0(ik, 'http://lexvo.org/id/iso639-3/ipk').
language0(io, 'http://lexvo.org/id/iso639-3/ido').
language0(is, 'http://lexvo.org/id/iso639-3/isl').
language0(it, 'http://lexvo.org/id/iso639-3/ita').
language0(iu, 'http://lexvo.org/id/iso639-3/iku').
language0(ja, 'http://lexvo.org/id/iso639-3/jpn').
language0(jv, 'http://lexvo.org/id/iso639-3/jav').
language0(ka, 'http://lexvo.org/id/iso639-3/kat').
language0(kg, 'http://lexvo.org/id/iso639-3/kon').
language0(ki, 'http://lexvo.org/id/iso639-3/kik').
language0(kj, 'http://lexvo.org/id/iso639-3/kua').
language0(kk, 'http://lexvo.org/id/iso639-3/kaz').
language0(kl, 'http://lexvo.org/id/iso639-3/kal').
language0(km, 'http://lexvo.org/id/iso639-3/khm').
language0(kn, 'http://lexvo.org/id/iso639-3/kan').
language0(ko, 'http://lexvo.org/id/iso639-3/kor').
language0(kr, 'http://lexvo.org/id/iso639-3/kau').
language0(ks, 'http://lexvo.org/id/iso639-3/kas').
language0(ku, 'http://lexvo.org/id/iso639-3/kur').
language0(kv, 'http://lexvo.org/id/iso639-3/kom').
language0(kw, 'http://lexvo.org/id/iso639-3/cor').
language0(ky, 'http://lexvo.org/id/iso639-3/kir').
language0(la, 'http://lexvo.org/id/iso639-3/lat').
language0(lb, 'http://lexvo.org/id/iso639-3/ltz').
language0(lg, 'http://lexvo.org/id/iso639-3/lug').
language0(li, 'http://lexvo.org/id/iso639-3/lim').
language0(ln, 'http://lexvo.org/id/iso639-3/lin').
language0(lo, 'http://lexvo.org/id/iso639-3/lao').
language0(lt, 'http://lexvo.org/id/iso639-3/lit').
language0(lu, 'http://lexvo.org/id/iso639-3/lub').
language0(lv, 'http://lexvo.org/id/iso639-3/lav').
language0(mg, 'http://lexvo.org/id/iso639-3/mlg').
language0(mh, 'http://lexvo.org/id/iso639-3/mah').
language0(mi, 'http://lexvo.org/id/iso639-3/mri').
language0(mk, 'http://lexvo.org/id/iso639-3/mkd').
language0(ml, 'http://lexvo.org/id/iso639-3/mal').
language0(mn, 'http://lexvo.org/id/iso639-3/mon').
language0(mr, 'http://lexvo.org/id/iso639-3/mar').
language0(ms, 'http://lexvo.org/id/iso639-3/msa').
language0(mt, 'http://lexvo.org/id/iso639-3/mlt').
language0(my, 'http://lexvo.org/id/iso639-3/mya').
language0(na, 'http://lexvo.org/id/iso639-3/nau').
language0(nb, 'http://lexvo.org/id/iso639-3/nob').
language0(nd, 'http://lexvo.org/id/iso639-3/nde').
language0(ne, 'http://lexvo.org/id/iso639-3/nep').
language0(ng, 'http://lexvo.org/id/iso639-3/ndo').
language0(nl, 'http://lexvo.org/id/iso639-3/nld').
language0(nn, 'http://lexvo.org/id/iso639-3/nno').
language0(no, 'http://lexvo.org/id/iso639-3/nor').
language0(nr, 'http://lexvo.org/id/iso639-3/nbl').
language0(nv, 'http://lexvo.org/id/iso639-3/nav').
language0(ny, 'http://lexvo.org/id/iso639-3/nya').
language0(oc, 'http://lexvo.org/id/iso639-3/oci').
language0(oj, 'http://lexvo.org/id/iso639-3/oji').
language0(om, 'http://lexvo.org/id/iso639-3/orm').
language0(or, 'http://lexvo.org/id/iso639-3/ori').
language0(os, 'http://lexvo.org/id/iso639-3/oss').
language0(pa, 'http://lexvo.org/id/iso639-3/pan').
language0(pi, 'http://lexvo.org/id/iso639-3/pli').
language0(pl, 'http://lexvo.org/id/iso639-3/pol').
language0(ps, 'http://lexvo.org/id/iso639-3/pus').
language0(pt, 'http://lexvo.org/id/iso639-3/por').
language0(qu, 'http://lexvo.org/id/iso639-3/que').
language0(rm, 'http://lexvo.org/id/iso639-3/roh').
language0(rn, 'http://lexvo.org/id/iso639-3/run').
language0(ro, 'http://lexvo.org/id/iso639-3/ron').
language0(ru, 'http://lexvo.org/id/iso639-3/rus').
language0(rw, 'http://lexvo.org/id/iso639-3/kin').
language0(sa, 'http://lexvo.org/id/iso639-3/san').
language0(sc, 'http://lexvo.org/id/iso639-3/srd').
language0(sd, 'http://lexvo.org/id/iso639-3/snd').
language0(se, 'http://lexvo.org/id/iso639-3/sme').
language0(sg, 'http://lexvo.org/id/iso639-3/sag').
language0(sh, 'http://lexvo.org/id/iso639-3/hbs').
language0(si, 'http://lexvo.org/id/iso639-3/sin').
language0(sk, 'http://lexvo.org/id/iso639-3/slk').
language0(sl, 'http://lexvo.org/id/iso639-3/slv').
language0(sm, 'http://lexvo.org/id/iso639-3/smo').
language0(sn, 'http://lexvo.org/id/iso639-3/sna').
language0(so, 'http://lexvo.org/id/iso639-3/som').
language0(sq, 'http://lexvo.org/id/iso639-3/sqi').
language0(sr, 'http://lexvo.org/id/iso639-3/srp').
language0(ss, 'http://lexvo.org/id/iso639-3/ssw').
language0(st, 'http://lexvo.org/id/iso639-3/sot').
language0(su, 'http://lexvo.org/id/iso639-3/sun').
language0(sv, 'http://lexvo.org/id/iso639-3/swe').
language0(sw, 'http://lexvo.org/id/iso639-3/swa').
language0(ta, 'http://lexvo.org/id/iso639-3/tam').
language0(te, 'http://lexvo.org/id/iso639-3/tel').
language0(tg, 'http://lexvo.org/id/iso639-3/tgk').
language0(th, 'http://lexvo.org/id/iso639-3/tha').
language0(ti, 'http://lexvo.org/id/iso639-3/tir').
language0(tk, 'http://lexvo.org/id/iso639-3/tuk').
language0(tl, 'http://lexvo.org/id/iso639-3/tgl').
language0(tn, 'http://lexvo.org/id/iso639-3/tsn').
language0(to, 'http://lexvo.org/id/iso639-3/ton').
language0(tr, 'http://lexvo.org/id/iso639-3/tur').
language0(ts, 'http://lexvo.org/id/iso639-3/tso').
language0(tt, 'http://lexvo.org/id/iso639-3/tat').
language0(tw, 'http://lexvo.org/id/iso639-3/twi').
language0(ty, 'http://lexvo.org/id/iso639-3/tah').
language0(ug, 'http://lexvo.org/id/iso639-3/uig').
language0(uk, 'http://lexvo.org/id/iso639-3/ukr').
language0(ur, 'http://lexvo.org/id/iso639-3/urd').
language0(uz, 'http://lexvo.org/id/iso639-3/uzb').
language0(ve, 'http://lexvo.org/id/iso639-3/ven').
language0(vi, 'http://lexvo.org/id/iso639-3/vie').
language0(vo, 'http://lexvo.org/id/iso639-3/vol').
language0(wa, 'http://lexvo.org/id/iso639-3/wln').
language0(wo, 'http://lexvo.org/id/iso639-3/wol').
language0(xh, 'http://lexvo.org/id/iso639-3/xho').
language0(yi, 'http://lexvo.org/id/iso639-3/yid').
language0(yo, 'http://lexvo.org/id/iso639-3/yor').
language0(za, 'http://lexvo.org/id/iso639-3/zha').
language0(zh, 'http://lexvo.org/id/iso639-3/zho').
language0(zu, 'http://lexvo.org/id/iso639-3/zul').

