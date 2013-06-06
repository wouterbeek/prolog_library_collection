:- module(
  iso_3166_1,
  [
    iso_3166_1/3 % ?Code:atom
                 % ?Name:atom
                 % ?Resource:uri
  ]
).

/** <module> ISO 3166-1

Suopport for the ISO 3166-1 country code standard.

@author Wouter Beek
@version 2013/01, 2013/06
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(iso_3166_1, 'http://lexvo.org/id/iso3166/').

:- rdf_meta(iso_3166_1(?,?,r)).



iso_3166_1(ad, 'Andorra', iso_3166_1:'AD').
iso_3166_1(ae, 'United arab emirates', iso_3166_1:'AE').
iso_3166_1(af, 'Afghanistan', iso_3166_1:'AF').
iso_3166_1(ag, 'Antigua and barbuda', iso_3166_1:'AG').
iso_3166_1(ai, 'Anguilla', iso_3166_1:'AI').
iso_3166_1(al, 'Albania', iso_3166_1:'AL').
iso_3166_1(am, 'Armenia', iso_3166_1:'AM').
iso_3166_1(ao, 'Angola', iso_3166_1:'AO').
iso_3166_1(aq, 'Antarctica', iso_3166_1:'AQ').
iso_3166_1(ar, 'Argentina', iso_3166_1:'AR').
iso_3166_1(as, 'American samoa', iso_3166_1:'AS').
iso_3166_1(at, 'Austria', iso_3166_1:'AT').
iso_3166_1(au, 'Australia', iso_3166_1:'AU').
iso_3166_1(aw, 'Aruba', iso_3166_1:'AW').
iso_3166_1(ax, 'Åland islands', iso_3166_1:'AX').
iso_3166_1(az, 'Azerbaijan', iso_3166_1:'AZ').
iso_3166_1(ba, 'Bosnia and herzegovina', iso_3166_1:'BA').
iso_3166_1(bb, 'Barbados', iso_3166_1:'BB').
iso_3166_1(bd, 'Bangladesh', iso_3166_1:'BD').
iso_3166_1(be, 'Belgium', iso_3166_1:'BE').
iso_3166_1(bf, 'Burkina faso', iso_3166_1:'BF').
iso_3166_1(bg, 'Bulgaria', iso_3166_1:'BG').
iso_3166_1(bh, 'Bahrain', iso_3166_1:'BH').
iso_3166_1(bi, 'Burundi', iso_3166_1:'BI').
iso_3166_1(bj, 'Benin', iso_3166_1:'BJ').
iso_3166_1(bl, 'Saint barthélemy', iso_3166_1:'BL').
iso_3166_1(bm, 'Bermuda', iso_3166_1:'BM').
iso_3166_1(bn, 'Brunei darussalam', iso_3166_1:'BN').
iso_3166_1(bo, 'Bolivia, plurinational state of', iso_3166_1:'BO').
iso_3166_1(bq, 'Bonaire, sint eustatius and saba', iso_3166_1:'BQ').
iso_3166_1(br, 'Brazil', iso_3166_1:'BR').
iso_3166_1(bs, 'Bahamas', iso_3166_1:'BS').
iso_3166_1(bt, 'Bhutan', iso_3166_1:'BT').
iso_3166_1(bv, 'Bouvet island', iso_3166_1:'BV').
iso_3166_1(bw, 'Botswana', iso_3166_1:'BW').
iso_3166_1(by, 'Belarus', iso_3166_1:'BY').
iso_3166_1(bz, 'Belize', iso_3166_1:'BZ').
iso_3166_1(ca, 'Canada', iso_3166_1:'CA').
iso_3166_1(cc, 'Cocos (keeling) islands', iso_3166_1:'CC').
iso_3166_1(cd, 'Congo, the democratic republic of the', iso_3166_1:'CD').
iso_3166_1(cf, 'Central african republic', iso_3166_1:'CF').
iso_3166_1(cg, 'Congo', iso_3166_1:'CG').
iso_3166_1(ch, 'Switzerland', iso_3166_1:'CH').
iso_3166_1(ci, 'Côte d\'ivoire', iso_3166_1:'CI').
iso_3166_1(ck, 'Cook islands', iso_3166_1:'CK').
iso_3166_1(cl, 'Chile', iso_3166_1:'CL').
iso_3166_1(cm, 'Cameroon', iso_3166_1:'CM').
iso_3166_1(cn, 'China', iso_3166_1:'CN').
iso_3166_1(co, 'Colombia', iso_3166_1:'CO').
iso_3166_1(cr, 'Costa rica', iso_3166_1:'CR').
iso_3166_1(cu, 'Cuba', iso_3166_1:'CU').
iso_3166_1(cv, 'Cape verde', iso_3166_1:'CV').
iso_3166_1(cw, 'Curaçao', iso_3166_1:'CW').
iso_3166_1(cx, 'Christmas island', iso_3166_1:'CX').
iso_3166_1(cy, 'Cyprus', iso_3166_1:'CY').
iso_3166_1(cz, 'Czech republic', iso_3166_1:'CZ').
iso_3166_1(de, 'Germany', iso_3166_1:'DE').
iso_3166_1(dj, 'Djibouti', iso_3166_1:'DJ').
iso_3166_1(dk, 'Denmark', iso_3166_1:'DK').
iso_3166_1(dm, 'Dominica', iso_3166_1:'DM').
iso_3166_1(do, 'Dominican republic', iso_3166_1:'DO').
iso_3166_1(dz, 'Algeria', iso_3166_1:'DZ').
iso_3166_1(ec, 'Ecuador', iso_3166_1:'EC').
iso_3166_1(ee, 'Estonia', iso_3166_1:'EE').
iso_3166_1(eg, 'Egypt', iso_3166_1:'EG').
iso_3166_1(eh, 'Western sahara', iso_3166_1:'EH').
iso_3166_1(er, 'Eritrea', iso_3166_1:'ER').
iso_3166_1(es, 'Spain', iso_3166_1:'ES').
iso_3166_1(et, 'Ethiopia', iso_3166_1:'ET').
iso_3166_1(fi, 'Finland', iso_3166_1:'FI').
iso_3166_1(fj, 'Fiji', iso_3166_1:'FJ').
iso_3166_1(fk, 'Falkland islands (malvinas)', iso_3166_1:'FK').
iso_3166_1(fm, 'Micronesia, federated states of', iso_3166_1:'FM').
iso_3166_1(fo, 'Faroe islands', iso_3166_1:'FO').
iso_3166_1(fr, 'France', iso_3166_1:'FR').
iso_3166_1(ga, 'Gabon', iso_3166_1:'GA').
iso_3166_1(gb, 'United kingdom', iso_3166_1:'GB').
iso_3166_1(gd, 'Grenada', iso_3166_1:'GD').
iso_3166_1(ge, 'Georgia', iso_3166_1:'GE').
iso_3166_1(gf, 'French guiana', iso_3166_1:'GF').
iso_3166_1(gg, 'Guernsey', iso_3166_1:'GG').
iso_3166_1(gh, 'Ghana', iso_3166_1:'GH').
iso_3166_1(gi, 'Gibraltar', iso_3166_1:'GI').
iso_3166_1(gl, 'Greenland', iso_3166_1:'GL').
iso_3166_1(gm, 'Gambia', iso_3166_1:'GM').
iso_3166_1(gn, 'Guinea', iso_3166_1:'GN').
iso_3166_1(gp, 'Guadeloupe', iso_3166_1:'GP').
iso_3166_1(gq, 'Equatorial guinea', iso_3166_1:'GQ').
iso_3166_1(gr, 'Greece', iso_3166_1:'GR').
iso_3166_1(gs, 'South georgia and the south sandwich islands', iso_3166_1:'GS').
iso_3166_1(gt, 'Guatemala', iso_3166_1:'GT').
iso_3166_1(gu, 'Guam', iso_3166_1:'GU').
iso_3166_1(gw, 'Guinea-bissau', iso_3166_1:'GW').
iso_3166_1(gy, 'Guyana', iso_3166_1:'GY').
iso_3166_1(hk, 'Hong kong', iso_3166_1:'HK').
iso_3166_1(hm, 'Heard island and mcdonald islands', iso_3166_1:'HM').
iso_3166_1(hn, 'Honduras', iso_3166_1:'HN').
iso_3166_1(hr, 'Croatia', iso_3166_1:'HR').
iso_3166_1(ht, 'Haiti', iso_3166_1:'HT').
iso_3166_1(hu, 'Hungary', iso_3166_1:'HU').
iso_3166_1(id, 'Indonesia', iso_3166_1:'ID').
iso_3166_1(ie, 'Ireland', iso_3166_1:'IE').
iso_3166_1(il, 'Israel', iso_3166_1:'IL').
iso_3166_1(im, 'Isle of man', iso_3166_1:'IM').
iso_3166_1(in, 'India', iso_3166_1:'IN').
iso_3166_1(io, 'British indian ocean territory', iso_3166_1:'IO').
iso_3166_1(iq, 'Iraq', iso_3166_1:'IQ').
iso_3166_1(ir, 'Iran, islamic republic of', iso_3166_1:'IR').
iso_3166_1(is, 'Iceland', iso_3166_1:'IS').
iso_3166_1(it, 'Italy', iso_3166_1:'IT').
iso_3166_1(je, 'Jersey', iso_3166_1:'JE').
iso_3166_1(jm, 'Jamaica', iso_3166_1:'JM').
iso_3166_1(jo, 'Jordan', iso_3166_1:'JO').
iso_3166_1(jp, 'Japan', iso_3166_1:'JP').
iso_3166_1(ke, 'Kenya', iso_3166_1:'KE').
iso_3166_1(kg, 'Kyrgyzstan', iso_3166_1:'KG').
iso_3166_1(kh, 'Cambodia', iso_3166_1:'KH').
iso_3166_1(ki, 'Kiribati', iso_3166_1:'KI').
iso_3166_1(km, 'Comoros', iso_3166_1:'KM').
iso_3166_1(kn, 'Saint kitts and nevis', iso_3166_1:'KN').
iso_3166_1(kp, 'Korea, democratic people\'s republic of', iso_3166_1:'KP').
iso_3166_1(kr, 'Korea, republic of', iso_3166_1:'KR').
iso_3166_1(kw, 'Kuwait', iso_3166_1:'KW').
iso_3166_1(ky, 'Cayman islands', iso_3166_1:'KY').
iso_3166_1(kz, 'Kazakhstan', iso_3166_1:'KZ').
iso_3166_1(la, 'Lao people\'s democratic republic', iso_3166_1:'LA').
iso_3166_1(lb, 'Lebanon', iso_3166_1:'LB').
iso_3166_1(lc, 'Saint lucia', iso_3166_1:'LC').
iso_3166_1(li, 'Liechtenstein', iso_3166_1:'LI').
iso_3166_1(lk, 'Sri lanka', iso_3166_1:'LK').
iso_3166_1(lr, 'Liberia', iso_3166_1:'LR').
iso_3166_1(ls, 'Lesotho', iso_3166_1:'LS').
iso_3166_1(lt, 'Lithuania', iso_3166_1:'LT').
iso_3166_1(lu, 'Luxembourg', iso_3166_1:'LU').
iso_3166_1(lv, 'Latvia', iso_3166_1:'LV').
iso_3166_1(ly, 'Libya', iso_3166_1:'LY').
iso_3166_1(ma, 'Morocco', iso_3166_1:'MA').
iso_3166_1(mc, 'Monaco', iso_3166_1:'MC').
iso_3166_1(md, 'Moldova, republic of', iso_3166_1:'MD').
iso_3166_1(me, 'Montenegro', iso_3166_1:'ME').
iso_3166_1(mf, 'Saint martin (french part)', iso_3166_1:'MF').
iso_3166_1(mg, 'Madagascar', iso_3166_1:'MG').
iso_3166_1(mh, 'Marshall islands', iso_3166_1:'MH').
iso_3166_1(mk, 'Macedonia, the former yugoslav republic of', iso_3166_1:'MK').
iso_3166_1(ml, 'Mali', iso_3166_1:'ML').
iso_3166_1(mm, 'Myanmar', iso_3166_1:'MM').
iso_3166_1(mn, 'Mongolia', iso_3166_1:'MN').
iso_3166_1(mo, 'Macao', iso_3166_1:'MO').
iso_3166_1(mp, 'Northern mariana islands', iso_3166_1:'MP').
iso_3166_1(mq, 'Martinique', iso_3166_1:'MQ').
iso_3166_1(mr, 'Mauritania', iso_3166_1:'MR').
iso_3166_1(ms, 'Montserrat', iso_3166_1:'MS').
iso_3166_1(mt, 'Malta', iso_3166_1:'MT').
iso_3166_1(mu, 'Mauritius', iso_3166_1:'MU').
iso_3166_1(mv, 'Maldives', iso_3166_1:'MV').
iso_3166_1(mw, 'Malawi', iso_3166_1:'MW').
iso_3166_1(mx, 'Mexico', iso_3166_1:'MX').
iso_3166_1(my, 'Malaysia', iso_3166_1:'MY').
iso_3166_1(mz, 'Mozambique', iso_3166_1:'MZ').
iso_3166_1(na, 'Namibia', iso_3166_1:'NA').
iso_3166_1(nc, 'New caledonia', iso_3166_1:'NC').
iso_3166_1(ne, 'Niger', iso_3166_1:'NE').
iso_3166_1(nf, 'Norfolk island', iso_3166_1:'NF').
iso_3166_1(ng, 'Nigeria', iso_3166_1:'NG').
iso_3166_1(ni, 'Nicaragua', iso_3166_1:'NI').
iso_3166_1(nl, 'Netherlands', iso_3166_1:'NL').
iso_3166_1(no, 'Norway', iso_3166_1:'NO').
iso_3166_1(np, 'Nepal', iso_3166_1:'NP').
iso_3166_1(nr, 'Nauru', iso_3166_1:'NR').
iso_3166_1(nu, 'Niue', iso_3166_1:'NU').
iso_3166_1(nz, 'New zealand', iso_3166_1:'NZ').
iso_3166_1(om, 'Oman', iso_3166_1:'OM').
iso_3166_1(pa, 'Panama', iso_3166_1:'PA').
iso_3166_1(pe, 'Peru', iso_3166_1:'PE').
iso_3166_1(pf, 'French polynesia', iso_3166_1:'PF').
iso_3166_1(pg, 'Papua new guinea', iso_3166_1:'PG').
iso_3166_1(ph, 'Philippines', iso_3166_1:'PH').
iso_3166_1(pk, 'Pakistan', iso_3166_1:'PK').
iso_3166_1(pl, 'Poland', iso_3166_1:'PL').
iso_3166_1(pm, 'Saint pierre and miquelon', iso_3166_1:'PM').
iso_3166_1(pn, 'Pitcairn', iso_3166_1:'PN').
iso_3166_1(pr, 'Puerto rico', iso_3166_1:'PR').
iso_3166_1(ps, 'Palestinian territory, occupied', iso_3166_1:'PS').
iso_3166_1(pt, 'Portugal', iso_3166_1:'PT').
iso_3166_1(pw, 'Palau', iso_3166_1:'PW').
iso_3166_1(py, 'Paraguay', iso_3166_1:'PY').
iso_3166_1(qa, 'Qatar', iso_3166_1:'QA').
iso_3166_1(re, 'Réunion', iso_3166_1:'RE').
iso_3166_1(ro, 'Romania', iso_3166_1:'RO').
iso_3166_1(rs, 'Serbia', iso_3166_1:'RS').
iso_3166_1(ru, 'Russian federation', iso_3166_1:'RU').
iso_3166_1(rw, 'Rwanda', iso_3166_1:'RW').
iso_3166_1(sa, 'Saudi arabia', iso_3166_1:'SA').
iso_3166_1(sb, 'Solomon islands', iso_3166_1:'SB').
iso_3166_1(sc, 'Seychelles', iso_3166_1:'SC').
iso_3166_1(sd, 'Sudan', iso_3166_1:'SD').
iso_3166_1(se, 'Sweden', iso_3166_1:'SE').
iso_3166_1(sg, 'Singapore', iso_3166_1:'SG').
iso_3166_1(sh, 'Saint helena, ascension and tristan da cunha', iso_3166_1:'SH').
iso_3166_1(si, 'Slovenia', iso_3166_1:'SI').
iso_3166_1(sj, 'Svalbard and jan mayen', iso_3166_1:'SJ').
iso_3166_1(sk, 'Slovakia', iso_3166_1:'SK').
iso_3166_1(sl, 'Sierra leone', iso_3166_1:'SL').
iso_3166_1(sm, 'San marino', iso_3166_1:'SM').
iso_3166_1(sn, 'Senegal', iso_3166_1:'SN').
iso_3166_1(so, 'Somalia', iso_3166_1:'SO').
iso_3166_1(sr, 'Suriname', iso_3166_1:'SR').
iso_3166_1(ss, 'South sudan', iso_3166_1:'SS').
iso_3166_1(st, 'Sao tome and principe', iso_3166_1:'ST').
iso_3166_1(sv, 'El salvador', iso_3166_1:'SV').
iso_3166_1(sx, 'Sint maarten (dutch part)', iso_3166_1:'SX').
iso_3166_1(sy, 'Syrian arab republic', iso_3166_1:'SY').
iso_3166_1(sz, 'Swaziland', iso_3166_1:'SZ').
iso_3166_1(tc, 'Turks and caicos islands', iso_3166_1:'TC').
iso_3166_1(td, 'Chad', iso_3166_1:'TD').
iso_3166_1(tf, 'French southern territories', iso_3166_1:'TF').
iso_3166_1(tg, 'Togo', iso_3166_1:'TG').
iso_3166_1(th, 'Thailand', iso_3166_1:'TH').
iso_3166_1(tj, 'Tajikistan', iso_3166_1:'TJ').
iso_3166_1(tk, 'Tokelau', iso_3166_1:'TK').
iso_3166_1(tl, 'Timor-leste', iso_3166_1:'TL').
iso_3166_1(tm, 'Turkmenistan', iso_3166_1:'TM').
iso_3166_1(tn, 'Tunisia', iso_3166_1:'TN').
iso_3166_1(to, 'Tonga', iso_3166_1:'TO').
iso_3166_1(tr, 'Turkey', iso_3166_1:'TR').
iso_3166_1(tt, 'Trinidad and tobago', iso_3166_1:'TT').
iso_3166_1(tv, 'Tuvalu', iso_3166_1:'TV').
iso_3166_1(tw, 'Taiwan, province of china', iso_3166_1:'TW').
iso_3166_1(tz, 'Tanzania, united republic of', iso_3166_1:'TZ').
iso_3166_1(ua, 'Ukraine', iso_3166_1:'UA').
iso_3166_1(ug, 'Uganda', iso_3166_1:'UG').
iso_3166_1(um, 'United states minor outlying islands', iso_3166_1:'UM').
iso_3166_1(us, 'United states', iso_3166_1:'US').
iso_3166_1(uy, 'Uruguay', iso_3166_1:'UY').
iso_3166_1(uz, 'Uzbekistan', iso_3166_1:'UZ').
iso_3166_1(va, 'Holy see (vatican city state)', iso_3166_1:'VA').
iso_3166_1(vc, 'Saint vincent and the grenadines', iso_3166_1:'VC').
iso_3166_1(ve, 'Venezuela, bolivarian republic of', iso_3166_1:'VE').
iso_3166_1(vg, 'Virgin islands, british', iso_3166_1:'VG').
iso_3166_1(vi, 'Virgin islands, u.s.', iso_3166_1:'VI').
iso_3166_1(vn, 'Viet nam', iso_3166_1:'VN').
iso_3166_1(vu, 'Vanuatu', iso_3166_1:'VU').
iso_3166_1(wf, 'Wallis and futuna', iso_3166_1:'WF').
iso_3166_1(ws, 'Samoa', iso_3166_1:'WS').
iso_3166_1(ye, 'Yemen', iso_3166_1:'YE').
iso_3166_1(yt, 'Mayotte', iso_3166_1:'YT').
iso_3166_1(za, 'South africa', iso_3166_1:'ZA').
iso_3166_1(zm, 'Zambia', iso_3166_1:'ZM').
iso_3166_1(zw, 'Zimbabwe', iso_3166_1:'ZW').

