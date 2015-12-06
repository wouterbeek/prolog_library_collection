:- module(
  fca_test,
  [
    fca_test/2 % ?ContextName:compound
               % ?Context:compound
  ]
).

fca_test(tab(1), context([x1,x2,x3,x4,x5],[y1,y2,y3,y4],fca_test:f1)).
f1(x1, y1).
f1(x1, y2).
f1(x1, y3).
f1(x1, y4).
f1(x2, y1).
f1(x2, y3).
f1(x2, y4).
f1(x3, y2).
f1(x3, y3).
f1(x3, y4).
f1(x4, y2).
f1(x4, y3).
f1(x4, y4).
f1(x5, y1).

fca_test(vychodil(1), context([a,b,c,d,e],[s,t,u,v,w,x,y,z],fca_test:f2)).
f2(a, t).
f2(a, u).
f2(a, w).
f2(a, x).
f2(a, z).
f2(b, s).
f2(b, v).
f2(b, y).
f2(b, z).
f2(c, s).
f2(c, t).
f2(c, u).
f2(c, v).
f2(c, x).
f2(c, y).
f2(d, t).
f2(d, w).
f2(d, x).
f2(e, s).
f2(e, t).
f2(e, w).
f2(e, x).
f2(e, y).
f2(e, z).

fca_test(vychodil(2), context([0,1,2,3,4],[0,1,2,3,4,5,6,7],fca_test:f3)).
f3(0, 1).
f3(0, 2).
f3(0, 4).
f3(0, 5).
f3(0, 7).
f3(1, 0).
f3(1, 3).
f3(1, 6).
f3(1, 7).
f3(2, 0).
f3(2, 1).
f3(2, 2).
f3(2, 3).
f3(2, 5).
f3(2, 6).
f3(3, 1).
f3(3, 4).
f3(3, 5).
f3(4, 0).
f3(4, 1).
f3(4, 4).
f3(4, 5).
f3(4, 6).
f3(4, 7).
