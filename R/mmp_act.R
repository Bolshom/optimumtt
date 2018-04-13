mmp_act=function(activity){
  require(zoo)
  mmp_vec=c()
  l_act=length(activity)
  for (i in 1:l_act){
    mmp_vec[i]=max(rollmean(activity,i))
  }
  return(mmp_vec)
}
