Phi = function (Ta,La,Tb,Lb,date) {
  phi = abs(ang_head(Ta,La,Tb,Lb)-climate(Ta,La,date)[4])
  return(phi)
}
