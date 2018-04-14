mmp_alldata_update=function(dates_list_updt,dates_list,power_list_updt,mmp_alldata){
#checking if update is necessary:
updt=length(which(!(dates_list_updt %in% dates_list)))==0

#criando mmp por treino para os treinos novos
if (updt){mmp_alldata_updt=mmp_alldata} else{
  mmp_updt=list()
  condition=which(!(dates_list_updt %in% dates_list))
  for (i in condition) {
    mmp_updt[[i]]=mmp_act(power_list_updt[[i]])
  }
  mmp_alldata_updt=list()
  l_lu=length(dates_list_updt)
  condition_2=which((dates_list_updt %in% dates_list))
  for (i in 1:l_lu){
    if (i %in% condition_2){
      mmp_alldata_updt[[i]]=mmp_alldata[[which(dates_list==dates_list_updt[i])]]} else {
        mmp_alldata_updt[[i]]=mmp_updt[[i]]}
  }
  print(i)
}
return(mmp_alldata_updt)
}
