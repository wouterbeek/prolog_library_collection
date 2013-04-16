:- module(
  iso_3166_1,
  [
    country/2 % ?Country:atom
              % ?Code:atom
  ]
).

/** <module> ISO 3166-1

Suopport for the ISO 3166-1 country code standard.

@author Wouter Beek
@version 2013/01
*/



%% country(?Code:atom, ?Country:atom) is nondet.

country(Code, Country):-
  maplist(var, [Code, Country]),
  !,
  country0(Code, _Name, Country).
country(Code, Country):-
  country0(Code, _Name, Country),
  !.

country0(ad, 'Andorra', 'http://lexvo.org/id/iso3166/AD').
country0(ae, 'United arab emirates', 'http://lexvo.org/id/iso3166/AE').
country0(af, 'Afghanistan', 'http://lexvo.org/id/iso3166/AF').
country0(ag, 'Antigua and barbuda', 'http://lexvo.org/id/iso3166/AG').
country0(ai, 'Anguilla', 'http://lexvo.org/id/iso3166/AI').
country0(al, 'Albania', 'http://lexvo.org/id/iso3166/AL').
country0(am, 'Armenia', 'http://lexvo.org/id/iso3166/AM').
country0(ao, 'Angola', 'http://lexvo.org/id/iso3166/AO').
country0(aq, 'Antarctica', 'http://lexvo.org/id/iso3166/AQ').
country0(ar, 'Argentina', 'http://lexvo.org/id/iso3166/AR').
country0(as, 'American samoa', 'http://lexvo.org/id/iso3166/AS').
country0(at, 'Austria', 'http://lexvo.org/id/iso3166/AT').
country0(au, 'Australia', 'http://lexvo.org/id/iso3166/AU').
country0(aw, 'Aruba', 'http://lexvo.org/id/iso3166/AW').
country0(ax, 'Åland islands', 'http://lexvo.org/id/iso3166/AX').
country0(az, 'Azerbaijan', 'http://lexvo.org/id/iso3166/AZ').
country0(ba, 'Bosnia and herzegovina', 'http://lexvo.org/id/iso3166/BA').
country0(bb, 'Barbados', 'http://lexvo.org/id/iso3166/BB').
country0(bd, 'Bangladesh', 'http://lexvo.org/id/iso3166/BD').
country0(be, 'Belgium', 'http://lexvo.org/id/iso3166/BE').
country0(bf, 'Burkina faso', 'http://lexvo.org/id/iso3166/BF').
country0(bg, 'Bulgaria', 'http://lexvo.org/id/iso3166/BG').
country0(bh, 'Bahrain', 'http://lexvo.org/id/iso3166/BH').
country0(bi, 'Burundi', 'http://lexvo.org/id/iso3166/BI').
country0(bj, 'Benin', 'http://lexvo.org/id/iso3166/BJ').
country0(bl, 'Saint barthélemy', 'http://lexvo.org/id/iso3166/BL').
country0(bm, 'Bermuda', 'http://lexvo.org/id/iso3166/BM').
country0(bn, 'Brunei darussalam', 'http://lexvo.org/id/iso3166/BN').
country0(bo, 'Bolivia, plurinational state of', 'http://lexvo.org/id/iso3166/BO').
country0(bq, 'Bonaire, sint eustatius and saba', 'http://lexvo.org/id/iso3166/BQ').
country0(br, 'Brazil', 'http://lexvo.org/id/iso3166/BR').
country0(bs, 'Bahamas', 'http://lexvo.org/id/iso3166/BS').
country0(bt, 'Bhutan', 'http://lexvo.org/id/iso3166/BT').
country0(bv, 'Bouvet island', 'http://lexvo.org/id/iso3166/BV').
country0(bw, 'Botswana', 'http://lexvo.org/id/iso3166/BW').
country0(by, 'Belarus', 'http://lexvo.org/id/iso3166/BY').
country0(bz, 'Belize', 'http://lexvo.org/id/iso3166/BZ').
country0(ca, 'Canada', 'http://lexvo.org/id/iso3166/CA').
country0(cc, 'Cocos (keeling) islands', 'http://lexvo.org/id/iso3166/CC').
country0(cd, 'Congo, the democratic republic of the', 'http://lexvo.org/id/iso3166/CD').
country0(cf, 'Central african republic', 'http://lexvo.org/id/iso3166/CF').
country0(cg, 'Congo', 'http://lexvo.org/id/iso3166/CG').
country0(ch, 'Switzerland', 'http://lexvo.org/id/iso3166/CH').
country0(ci, 'Côte d\'ivoire', 'http://lexvo.org/id/iso3166/CI').
country0(ck, 'Cook islands', 'http://lexvo.org/id/iso3166/CK').
country0(cl, 'Chile', 'http://lexvo.org/id/iso3166/CL').
country0(cm, 'Cameroon', 'http://lexvo.org/id/iso3166/CM').
country0(cn, 'China', 'http://lexvo.org/id/iso3166/CN').
country0(co, 'Colombia', 'http://lexvo.org/id/iso3166/CO').
country0(cr, 'Costa rica', 'http://lexvo.org/id/iso3166/CR').
country0(cu, 'Cuba', 'http://lexvo.org/id/iso3166/CU').
country0(cv, 'Cape verde', 'http://lexvo.org/id/iso3166/CV').
country0(cw, 'Curaçao', 'http://lexvo.org/id/iso3166/CW').
country0(cx, 'Christmas island', 'http://lexvo.org/id/iso3166/CX').
country0(cy, 'Cyprus', 'http://lexvo.org/id/iso3166/CY').
country0(cz, 'Czech republic', 'http://lexvo.org/id/iso3166/CZ').
country0(de, 'Germany', 'http://lexvo.org/id/iso3166/DE').
country0(dj, 'Djibouti', 'http://lexvo.org/id/iso3166/DJ').
country0(dk, 'Denmark', 'http://lexvo.org/id/iso3166/DK').
country0(dm, 'Dominica', 'http://lexvo.org/id/iso3166/DM').
country0(do, 'Dominican republic', 'http://lexvo.org/id/iso3166/DO').
country0(dz, 'Algeria', 'http://lexvo.org/id/iso3166/DZ').
country0(ec, 'Ecuador', 'http://lexvo.org/id/iso3166/EC').
country0(ee, 'Estonia', 'http://lexvo.org/id/iso3166/EE').
country0(eg, 'Egypt', 'http://lexvo.org/id/iso3166/EG').
country0(eh, 'Western sahara', 'http://lexvo.org/id/iso3166/EH').
country0(er, 'Eritrea', 'http://lexvo.org/id/iso3166/ER').
country0(es, 'Spain', 'http://lexvo.org/id/iso3166/ES').
country0(et, 'Ethiopia', 'http://lexvo.org/id/iso3166/ET').
country0(fi, 'Finland', 'http://lexvo.org/id/iso3166/FI').
country0(fj, 'Fiji', 'http://lexvo.org/id/iso3166/FJ').
country0(fk, 'Falkland islands (malvinas)', 'http://lexvo.org/id/iso3166/FK').
country0(fm, 'Micronesia, federated states of', 'http://lexvo.org/id/iso3166/FM').
country0(fo, 'Faroe islands', 'http://lexvo.org/id/iso3166/FO').
country0(fr, 'France', 'http://lexvo.org/id/iso3166/FR').
country0(ga, 'Gabon', 'http://lexvo.org/id/iso3166/GA').
country0(gb, 'United kingdom', 'http://lexvo.org/id/iso3166/GB').
country0(gd, 'Grenada', 'http://lexvo.org/id/iso3166/GD').
country0(ge, 'Georgia', 'http://lexvo.org/id/iso3166/GE').
country0(gf, 'French guiana', 'http://lexvo.org/id/iso3166/GF').
country0(gg, 'Guernsey', 'http://lexvo.org/id/iso3166/GG').
country0(gh, 'Ghana', 'http://lexvo.org/id/iso3166/GH').
country0(gi, 'Gibraltar', 'http://lexvo.org/id/iso3166/GI').
country0(gl, 'Greenland', 'http://lexvo.org/id/iso3166/GL').
country0(gm, 'Gambia', 'http://lexvo.org/id/iso3166/GM').
country0(gn, 'Guinea', 'http://lexvo.org/id/iso3166/GN').
country0(gp, 'Guadeloupe', 'http://lexvo.org/id/iso3166/GP').
country0(gq, 'Equatorial guinea', 'http://lexvo.org/id/iso3166/GQ').
country0(gr, 'Greece', 'http://lexvo.org/id/iso3166/GR').
country0(gs, 'South georgia and the south sandwich islands', 'http://lexvo.org/id/iso3166/GS').
country0(gt, 'Guatemala', 'http://lexvo.org/id/iso3166/GT').
country0(gu, 'Guam', 'http://lexvo.org/id/iso3166/GU').
country0(gw, 'Guinea-bissau', 'http://lexvo.org/id/iso3166/GW').
country0(gy, 'Guyana', 'http://lexvo.org/id/iso3166/GY').
country0(hk, 'Hong kong', 'http://lexvo.org/id/iso3166/HK').
country0(hm, 'Heard island and mcdonald islands', 'http://lexvo.org/id/iso3166/HM').
country0(hn, 'Honduras', 'http://lexvo.org/id/iso3166/HN').
country0(hr, 'Croatia', 'http://lexvo.org/id/iso3166/HR').
country0(ht, 'Haiti', 'http://lexvo.org/id/iso3166/HT').
country0(hu, 'Hungary', 'http://lexvo.org/id/iso3166/HU').
country0(id, 'Indonesia', 'http://lexvo.org/id/iso3166/ID').
country0(ie, 'Ireland', 'http://lexvo.org/id/iso3166/IE').
country0(il, 'Israel', 'http://lexvo.org/id/iso3166/IL').
country0(im, 'Isle of man', 'http://lexvo.org/id/iso3166/IM').
country0(in, 'India', 'http://lexvo.org/id/iso3166/IN').
country0(io, 'British indian ocean territory', 'http://lexvo.org/id/iso3166/IO').
country0(iq, 'Iraq', 'http://lexvo.org/id/iso3166/IQ').
country0(ir, 'Iran, islamic republic of', 'http://lexvo.org/id/iso3166/IR').
country0(is, 'Iceland', 'http://lexvo.org/id/iso3166/IS').
country0(it, 'Italy', 'http://lexvo.org/id/iso3166/IT').
country0(je, 'Jersey', 'http://lexvo.org/id/iso3166/JE').
country0(jm, 'Jamaica', 'http://lexvo.org/id/iso3166/JM').
country0(jo, 'Jordan', 'http://lexvo.org/id/iso3166/JO').
country0(jp, 'Japan', 'http://lexvo.org/id/iso3166/JP').
country0(ke, 'Kenya', 'http://lexvo.org/id/iso3166/KE').
country0(kg, 'Kyrgyzstan', 'http://lexvo.org/id/iso3166/KG').
country0(kh, 'Cambodia', 'http://lexvo.org/id/iso3166/KH').
country0(ki, 'Kiribati', 'http://lexvo.org/id/iso3166/KI').
country0(km, 'Comoros', 'http://lexvo.org/id/iso3166/KM').
country0(kn, 'Saint kitts and nevis', 'http://lexvo.org/id/iso3166/KN').
country0(kp, 'Korea, democratic people\'s republic of', 'http://lexvo.org/id/iso3166/KP').
country0(kr, 'Korea, republic of', 'http://lexvo.org/id/iso3166/KR').
country0(kw, 'Kuwait', 'http://lexvo.org/id/iso3166/KW').
country0(ky, 'Cayman islands', 'http://lexvo.org/id/iso3166/KY').
country0(kz, 'Kazakhstan', 'http://lexvo.org/id/iso3166/KZ').
country0(la, 'Lao people\'s democratic republic', 'http://lexvo.org/id/iso3166/LA').
country0(lb, 'Lebanon', 'http://lexvo.org/id/iso3166/LB').
country0(lc, 'Saint lucia', 'http://lexvo.org/id/iso3166/LC').
country0(li, 'Liechtenstein', 'http://lexvo.org/id/iso3166/LI').
country0(lk, 'Sri lanka', 'http://lexvo.org/id/iso3166/LK').
country0(lr, 'Liberia', 'http://lexvo.org/id/iso3166/LR').
country0(ls, 'Lesotho', 'http://lexvo.org/id/iso3166/LS').
country0(lt, 'Lithuania', 'http://lexvo.org/id/iso3166/LT').
country0(lu, 'Luxembourg', 'http://lexvo.org/id/iso3166/LU').
country0(lv, 'Latvia', 'http://lexvo.org/id/iso3166/LV').
country0(ly, 'Libya', 'http://lexvo.org/id/iso3166/LY').
country0(ma, 'Morocco', 'http://lexvo.org/id/iso3166/MA').
country0(mc, 'Monaco', 'http://lexvo.org/id/iso3166/MC').
country0(md, 'Moldova, republic of', 'http://lexvo.org/id/iso3166/MD').
country0(me, 'Montenegro', 'http://lexvo.org/id/iso3166/ME').
country0(mf, 'Saint martin (french part)', 'http://lexvo.org/id/iso3166/MF').
country0(mg, 'Madagascar', 'http://lexvo.org/id/iso3166/MG').
country0(mh, 'Marshall islands', 'http://lexvo.org/id/iso3166/MH').
country0(mk, 'Macedonia, the former yugoslav republic of', 'http://lexvo.org/id/iso3166/MK').
country0(ml, 'Mali', 'http://lexvo.org/id/iso3166/ML').
country0(mm, 'Myanmar', 'http://lexvo.org/id/iso3166/MM').
country0(mn, 'Mongolia', 'http://lexvo.org/id/iso3166/MN').
country0(mo, 'Macao', 'http://lexvo.org/id/iso3166/MO').
country0(mp, 'Northern mariana islands', 'http://lexvo.org/id/iso3166/MP').
country0(mq, 'Martinique', 'http://lexvo.org/id/iso3166/MQ').
country0(mr, 'Mauritania', 'http://lexvo.org/id/iso3166/MR').
country0(ms, 'Montserrat', 'http://lexvo.org/id/iso3166/MS').
country0(mt, 'Malta', 'http://lexvo.org/id/iso3166/MT').
country0(mu, 'Mauritius', 'http://lexvo.org/id/iso3166/MU').
country0(mv, 'Maldives', 'http://lexvo.org/id/iso3166/MV').
country0(mw, 'Malawi', 'http://lexvo.org/id/iso3166/MW').
country0(mx, 'Mexico', 'http://lexvo.org/id/iso3166/MX').
country0(my, 'Malaysia', 'http://lexvo.org/id/iso3166/MY').
country0(mz, 'Mozambique', 'http://lexvo.org/id/iso3166/MZ').
country0(na, 'Namibia', 'http://lexvo.org/id/iso3166/NA').
country0(nc, 'New caledonia', 'http://lexvo.org/id/iso3166/NC').
country0(ne, 'Niger', 'http://lexvo.org/id/iso3166/NE').
country0(nf, 'Norfolk island', 'http://lexvo.org/id/iso3166/NF').
country0(ng, 'Nigeria', 'http://lexvo.org/id/iso3166/NG').
country0(ni, 'Nicaragua', 'http://lexvo.org/id/iso3166/NI').
country0(nl, 'Netherlands', 'http://lexvo.org/id/iso3166/NL').
country0(no, 'Norway', 'http://lexvo.org/id/iso3166/NO').
country0(np, 'Nepal', 'http://lexvo.org/id/iso3166/NP').
country0(nr, 'Nauru', 'http://lexvo.org/id/iso3166/NR').
country0(nu, 'Niue', 'http://lexvo.org/id/iso3166/NU').
country0(nz, 'New zealand', 'http://lexvo.org/id/iso3166/NZ').
country0(om, 'Oman', 'http://lexvo.org/id/iso3166/OM').
country0(pa, 'Panama', 'http://lexvo.org/id/iso3166/PA').
country0(pe, 'Peru', 'http://lexvo.org/id/iso3166/PE').
country0(pf, 'French polynesia', 'http://lexvo.org/id/iso3166/PF').
country0(pg, 'Papua new guinea', 'http://lexvo.org/id/iso3166/PG').
country0(ph, 'Philippines', 'http://lexvo.org/id/iso3166/PH').
country0(pk, 'Pakistan', 'http://lexvo.org/id/iso3166/PK').
country0(pl, 'Poland', 'http://lexvo.org/id/iso3166/PL').
country0(pm, 'Saint pierre and miquelon', 'http://lexvo.org/id/iso3166/PM').
country0(pn, 'Pitcairn', 'http://lexvo.org/id/iso3166/PN').
country0(pr, 'Puerto rico', 'http://lexvo.org/id/iso3166/PR').
country0(ps, 'Palestinian territory, occupied', 'http://lexvo.org/id/iso3166/PS').
country0(pt, 'Portugal', 'http://lexvo.org/id/iso3166/PT').
country0(pw, 'Palau', 'http://lexvo.org/id/iso3166/PW').
country0(py, 'Paraguay', 'http://lexvo.org/id/iso3166/PY').
country0(qa, 'Qatar', 'http://lexvo.org/id/iso3166/QA').
country0(re, 'Réunion', 'http://lexvo.org/id/iso3166/RE').
country0(ro, 'Romania', 'http://lexvo.org/id/iso3166/RO').
country0(rs, 'Serbia', 'http://lexvo.org/id/iso3166/RS').
country0(ru, 'Russian federation', 'http://lexvo.org/id/iso3166/RU').
country0(rw, 'Rwanda', 'http://lexvo.org/id/iso3166/RW').
country0(sa, 'Saudi arabia', 'http://lexvo.org/id/iso3166/SA').
country0(sb, 'Solomon islands', 'http://lexvo.org/id/iso3166/SB').
country0(sc, 'Seychelles', 'http://lexvo.org/id/iso3166/SC').
country0(sd, 'Sudan', 'http://lexvo.org/id/iso3166/SD').
country0(se, 'Sweden', 'http://lexvo.org/id/iso3166/SE').
country0(sg, 'Singapore', 'http://lexvo.org/id/iso3166/SG').
country0(sh, 'Saint helena, ascension and tristan da cunha', 'http://lexvo.org/id/iso3166/SH').
country0(si, 'Slovenia', 'http://lexvo.org/id/iso3166/SI').
country0(sj, 'Svalbard and jan mayen', 'http://lexvo.org/id/iso3166/SJ').
country0(sk, 'Slovakia', 'http://lexvo.org/id/iso3166/SK').
country0(sl, 'Sierra leone', 'http://lexvo.org/id/iso3166/SL').
country0(sm, 'San marino', 'http://lexvo.org/id/iso3166/SM').
country0(sn, 'Senegal', 'http://lexvo.org/id/iso3166/SN').
country0(so, 'Somalia', 'http://lexvo.org/id/iso3166/SO').
country0(sr, 'Suriname', 'http://lexvo.org/id/iso3166/SR').
country0(ss, 'South sudan', 'http://lexvo.org/id/iso3166/SS').
country0(st, 'Sao tome and principe', 'http://lexvo.org/id/iso3166/ST').
country0(sv, 'El salvador', 'http://lexvo.org/id/iso3166/SV').
country0(sx, 'Sint maarten (dutch part)', 'http://lexvo.org/id/iso3166/SX').
country0(sy, 'Syrian arab republic', 'http://lexvo.org/id/iso3166/SY').
country0(sz, 'Swaziland', 'http://lexvo.org/id/iso3166/SZ').
country0(tc, 'Turks and caicos islands', 'http://lexvo.org/id/iso3166/TC').
country0(td, 'Chad', 'http://lexvo.org/id/iso3166/TD').
country0(tf, 'French southern territories', 'http://lexvo.org/id/iso3166/TF').
country0(tg, 'Togo', 'http://lexvo.org/id/iso3166/TG').
country0(th, 'Thailand', 'http://lexvo.org/id/iso3166/TH').
country0(tj, 'Tajikistan', 'http://lexvo.org/id/iso3166/TJ').
country0(tk, 'Tokelau', 'http://lexvo.org/id/iso3166/TK').
country0(tl, 'Timor-leste', 'http://lexvo.org/id/iso3166/TL').
country0(tm, 'Turkmenistan', 'http://lexvo.org/id/iso3166/TM').
country0(tn, 'Tunisia', 'http://lexvo.org/id/iso3166/TN').
country0(to, 'Tonga', 'http://lexvo.org/id/iso3166/TO').
country0(tr, 'Turkey', 'http://lexvo.org/id/iso3166/TR').
country0(tt, 'Trinidad and tobago', 'http://lexvo.org/id/iso3166/TT').
country0(tv, 'Tuvalu', 'http://lexvo.org/id/iso3166/TV').
country0(tw, 'Taiwan, province of china', 'http://lexvo.org/id/iso3166/TW').
country0(tz, 'Tanzania, united republic of', 'http://lexvo.org/id/iso3166/TZ').
country0(ua, 'Ukraine', 'http://lexvo.org/id/iso3166/UA').
country0(ug, 'Uganda', 'http://lexvo.org/id/iso3166/UG').
country0(um, 'United states minor outlying islands', 'http://lexvo.org/id/iso3166/UM').
country0(us, 'United states', 'http://lexvo.org/id/iso3166/US').
country0(uy, 'Uruguay', 'http://lexvo.org/id/iso3166/UY').
country0(uz, 'Uzbekistan', 'http://lexvo.org/id/iso3166/UZ').
country0(va, 'Holy see (vatican city state)', 'http://lexvo.org/id/iso3166/VA').
country0(vc, 'Saint vincent and the grenadines', 'http://lexvo.org/id/iso3166/VC').
country0(ve, 'Venezuela, bolivarian republic of', 'http://lexvo.org/id/iso3166/VE').
country0(vg, 'Virgin islands, british', 'http://lexvo.org/id/iso3166/VG').
country0(vi, 'Virgin islands, u.s.', 'http://lexvo.org/id/iso3166/VI').
country0(vn, 'Viet nam', 'http://lexvo.org/id/iso3166/VN').
country0(vu, 'Vanuatu', 'http://lexvo.org/id/iso3166/VU').
country0(wf, 'Wallis and futuna', 'http://lexvo.org/id/iso3166/WF').
country0(ws, 'Samoa', 'http://lexvo.org/id/iso3166/WS').
country0(ye, 'Yemen', 'http://lexvo.org/id/iso3166/YE').
country0(yt, 'Mayotte', 'http://lexvo.org/id/iso3166/YT').
country0(za, 'South africa', 'http://lexvo.org/id/iso3166/ZA').
country0(zm, 'Zambia', 'http://lexvo.org/id/iso3166/ZM').
country0(zw, 'Zimbabwe', 'http://lexvo.org/id/iso3166/ZW').

