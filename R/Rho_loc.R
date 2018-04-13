climate = function (Ta,La,date) {
  require(darksky)
  require(weathermetrics)
  c1 = darksky::get_forecast_for(Ta, La, date)
  c2=0
  c2[1] = weathermetrics::fahrenheit.to.celsius(c1$currently$temperature,2)
  c2[2] = weathermetrics::fahrenheit.to.celsius(c1$currently$dewPoint,2)
  c2[3] = c1$currently$pressure
  c2[4] = c1$currently$windBearing
  c2[5] = c1$currently$windSpeed*(16.0934/36) #conversion to m/s
  return(c2)}

Rho_loc = function (Ta,La,date) {
  clim=climate(Ta,La,date)
  t=clim[1]
  t.dew=clim[2]
  hpa=clim[3]
  b=((P.d(hpa,t.dew)/(287.0531*(t+273.15)))+(P.v(t.dew)/(461.4964*(t+273.15))))
  return(b)}
