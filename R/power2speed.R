f.grav = function (G,W) (9.80665*sin(atan(G/100))*W)
f.roll = function (G,W,Crr) (9.80665*cos(atan(G/100))*W*Crr)
v.wind = function (Vw,phi) (Vw*cos(rad(phi))) #input in m/s
f.drag = function (CdA,Rho,V,Vw,phi) (0.5*CdA*Rho*(((V+v.wind(Vw,phi))/3.6)^2))
f.resist = function (G,W,Crr,CdA,Rho,V,Vw,phi) (f.grav(G,W)+f.roll(G,W,Crr)+f.drag(CdA,Rho,V,Vw,phi))
speed2power = function (G,W,Crr,CdA,Rho,Ldt,Vw,phi,V) {
  power=(((1-(Ldt/100))^(-1))*f.resist(G,W,Crr,CdA,Rho,V,Vw,phi)*(V/3.6))
  return(power)}
power2speed = function (G,W,Crr,CdA,Rho,Ldt,Vw,phi,P) {
  v=seq(0,100,0.001)
  p=speed2power(G,W,Crr,CdA,Rho,Ldt,Vw,phi,v)
  spd=v[which.min(abs(p-P))]
  return(spd)}
