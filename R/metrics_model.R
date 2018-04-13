metrics_model=function(mmp_file,wpk=F,weight){
  require(changepoint)
  if (!wpk){
    mmp_file=mmp_file/weight
  }
  if (mmp_file[1]==0){
    mm=mmp_file[-1]
  } else {mm=mmp_file}

  # bounds on anaerobic interval in minutes
  i1 <- 15
  i2 <- 90
  # bounds on anaerobic interval in minutes
  i3 <- 120
  i4 <- 300
  # bounds on aerobic interval in minutes
  i5 <- 600
  i6 <- 3000
  # bounds on long aerobic interval in minutes
  i7 <- 4000
  i8 <- 40000
  if ( i7 > length(mm) )
  { i7 <- (length(mm)-1) }
  if ( i8 > length(mm) )
  { i8 <- length(mm) }
  paa = 15
  etau = 1
  ecp = 5
  paa_dec = -2
  ecp_del = -0.9
  tau_del = -4.8
  ecp_dec = -1
  ecp_dec_del = -180
  paa_pow = 1.05
  paa_min = 5
  etau_min = 0.5
  paa_dec_max = -0.25
  paa_dec_min = -3
  ecp_dec_min = -5
  etau_delta_max = 1e-4
  paa_delta_max = 1e-2
  paa_dec_delta_max = 1e-4
  ecp_del_delta_max = 1e-4
  ecp_dec_delta_max  = 1e-8
  max_loops = 100
  iteration = 0
  repeat{
    iteration <- iteration + 1
    if (iteration > max_loops) {break}
    etau_prev = etau
    paa_prev = paa
    paa_dec_prev = paa_dec
    ecp_del_prev = ecp_del
    ecp_dec_prev = ecp_dec
    # estimate ecp
    ecp = 0
    avg_ecp = 0.0
    count=1
    for (i in i5:i6) {
      ecpn = (mm[i] - paa * exp(paa_dec*((i/60.0)^(paa_pow)))) / (1-exp(tau_del*i/60.0)) / (1-exp(ecp_del*i/60.0)) / (1+ecp_dec*exp(ecp_dec_del/(i/60.0))) / ( 1 + etau/(i/60.0))
      avg_ecp = ((count-1)*avg_ecp+ecpn)/count;
      if (ecp < ecpn){
        ecp = ecpn}
      count <- count + 1
    }
    # estimate etau
    etau = etau_min
    avg_etau = 0.0
    count=1;
    for (i in i3:i4) {
      etaun = ((mm[i] - paa * exp(paa_dec*((i/60.0)^(paa_pow)))) /ecp / (1-exp(tau_del*i/60.0)) / (1-exp(ecp_del*i/60.0)) / (1+ecp_dec*exp(ecp_dec_del/(i/60.0))) - 1) * (i/60.0)
      #print(etaun)
      avg_etau = ((count-1)*avg_etau+etaun)/count
      if (etau < etaun)
      {etau = etaun}
      count <- count + 1
    }
    paa_dec = paa_dec_min
    avg_paa_dec = 0.0
    count=1
    for (i in i1:i2) {
      paa_decn = log((mm[i] - ecp * (1-exp(tau_del*i/60.0)) * (1-exp(ecp_del*i/60.0)) * (1+ecp_dec*exp(ecp_dec_del/(i/60.0))) * ( 1 + etau/(i/60.0)) ) / paa ) / ((i/60.0)^( paa_pow))
      avg_paa_dec = ((count-1)*avg_paa_dec+paa_decn)/count
      if (is.na(paa_decn)){paa_decn<-paa_dec} else{
        if ((paa_dec < paa_decn) && (paa_decn < paa_dec_max)){
          paa_dec = paa_decn
        }
      }
      count <- count + 1
    }
    paa = paa_min
    avg_paa = 0.0
    count=1
    for (i in 1:8) {
      paan = (mm[i] - ecp * (1-exp(tau_del*i/60.0)) * (1-exp(ecp_del*i/60.0)) * (1+ecp_dec*exp(ecp_dec_del/(i/60.0))) * ( 1 + etau/(i/60.0))) / exp(paa_dec*((i/60.0)^paa_pow))
      avg_paa = ((count-1)*avg_paa+paan)/count
      if (paa < paan){
        paa = paan}
      count <- count + 1
    }
    if (avg_paa<0.95*paa) {
      paa = avg_paa
    }
    ecp_dec = ecp_dec_min
    avg_ecp_dec = 0.0
    count=1
    for (i in seq(i7, i8, 120)) {
      ecp_decn = ((mm[i] - paa * exp(paa_dec*((i/60.0)^paa_pow))) / ecp / (1-exp(tau_del*i/60.0)) / (1-exp(ecp_del*i/60.0)) / ( 1 + etau/(i/60.0)) -1 ) / exp(ecp_dec_del/(i / 60.0))
      avg_ecp_dec = ((count-1)*avg_ecp_dec+ecp_decn)/count
      if (ecp_decn > 0){
        ecp_decn = 0}
      if (ecp_dec < ecp_decn){
        ecp_dec = ecp_decn}
      count <- count + 1
    }
    if (!((abs(etau - etau_prev) > etau_delta_max) || (abs(paa - paa_prev) > paa_delta_max)  || (abs(paa_dec - paa_dec_prev) > paa_dec_delta_max) || (abs(ecp_del - ecp_del_prev) > ecp_del_delta_max)  || (abs(ecp_dec - ecp_dec_prev) > ecp_dec_delta_max))) {break}
  }
  pMax = paa*exp(paa_dec*((1/60.0)^paa_pow)) + ecp * (1-exp(tau_del*(1/60.0))) * (1-exp(ecp_del*(1/60.0))) * (1+ecp_dec*exp(ecp_dec_del/(1/60.0))) * ( 1 + etau/(1/60.0))
  mmp60 = paa*exp(paa_dec*((60.0)^paa_pow)) + ecp * (1-exp(tau_del*(60.0))) * (1-exp(ecp_del*60.0)) * (1+ecp_dec*exp(ecp_dec_del/60.0)) * ( 1 + etau/(60.0))
  xemmp <- 1:620
  yemmp <- 1:620
  for (i in 1:620){
    t <- 5*10^(i/210)-3
    yemmp[i] <- paa*exp(paa_dec*(((t/60)^paa_pow))) + ecp * (1-exp(tau_del*(t/60))) * (1-exp(ecp_del*(t/60))) * (1+ecp_dec*exp(ecp_dec_del/(t/60))) * ( 1 + etau/((t/60)))
    xemmp[i] <- t
  }
  #Interval Discovery
  myts=ts(yemmp,start=c(1),end=c(length(yemmp)),frequency= 1)
  disc=cpt.mean(myts,penalty="Manual",pen.value="log(n)",method="PELT")
  low_avg=which((disc@cpts>sum(xemmp<i5)+1)&(disc@cpts<sum(xemmp<=i7)))[1]
  if (is.na(low_avg)){
    low_avg=length(disc@cpts)-1
  }
  mftp=mean(yemmp[disc@cpts[low_avg]:sum(xemmp<=i7)])*weight
  wbal=ecp*etau*60*weight
  pmax=pMax*weight
  return(c(pmax,wbal,mftp))
}
