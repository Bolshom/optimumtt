mmp_alldata=function(act_power_list){
  mmp_alldata=list()
  l_pl=length(act_power_list)
  for (i in 1:l_pl) {
    mmp_alldata[[i]]=mmp_act(act_power_list[[i]])
    print(i)
  }
  return(mmp_alldata)
}