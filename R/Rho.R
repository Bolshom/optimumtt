p = function (t) {
  c0 = 0.99999683
  c1 = -0.90826951e-02
  c2 = 0.78736169e-04
  c3 = -0.61117958e-06
  c4 = 0.43884187e-08
  c5 = -0.29883885e-10
  c6 = 0.21874425e-12
  c7 = -0.17892321e-14
  c8 = 0.11112018e-16
  c9 = -0.30994571e-19
  p = c0+t*(c1+t*(c2+t*(c3+t*(c4+t*(c5+t*(c6+t*(c7+t*(c8+t*(c9)))))))))
  return(p)
}
Es = function (t) (6.1078/(p(t)^8))
P.v = function (t.dew) (Es(t.dew)*100)
P.d = function (hpa,t.dew) ((hpa*100)-P.v(t.dew))
Rho = function (t,t.dew,hpa) {
  rho=((P.d(hpa,t.dew)/(287.0531*(t+273.15)))+(P.v(t.dew)/(461.4964*(t+273.15))))
  return(rho)
}
