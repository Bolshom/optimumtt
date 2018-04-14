metrics_moving_avg=function(updt,mmp_alldata,dates_list,period=90,wpk=F,weight,pmax_all=NULL,wbal_all=NULL,mftp_all=NULL) {
  require(zoo)
  require(changepoint)
  require(lubridate)
  options(warn=-1)
  #data framing the list
  #which training session is the longest?
  len=c()
  l_ma=length(mmp_alldata)
  for (i in 1:l_ma){
    len[i]=length(mmp_alldata[[i]])
  }

  #transforming and completing with zeros
  mmp_all_df=data.frame(1:max(len))
  for (i in 1:l_ma){
    if (length(mmp_alldata[[i]]<max(len))){
      mmp_all_df[,i]=c(mmp_alldata[[i]],rep(0,max(len)-length(mmp_alldata[[i]])))} else {
        mmp_all_df[,i]=mmp_alldata[[i]]
      }
  }

  if (!wpk){
    mmp_all_df=mmp_all_df/weight
  }

  #getting only dates from dates_list
  dates_list_2=lubridate::date(dates_list)
  dates=dates_list_2[1]:tail(dates_list_2,1)
  #creating vector with all data from begging of period to the end
  dates=as.Date(dates, origin = "1970-01-01")


  #calculating rolling metrics with a given period
  if (!updt){
    pmax_all=c()
    mftp_all=c()
    wbal_all=c()
    condition=as.numeric(tail(dates_list_2,1)-dates_list_2[1])-period+1
    for (i in 1:condition){
      if (length(which(dates_list_2 %in% (dates[i]-period):dates[i]))==1){
        max_mmp=mmp_all_df[,which(dates_list_2 %in% (dates[i]-period):dates[i])]
        metrics=metrics_model(max_mmp,T,weight)
        pmax_all[i]=metrics[1]
        mftp_all[i]=metrics[2]
        wbal_all[i]=metrics[3]
        print(i)
      } else{
        max_mmp=apply(mmp_all_df[,which(dates_list_2 %in% dates[i]:(dates[i]+period))],1,max)
        metrics=metrics_model(max_mmp,T,weight)
        pmax_all[i]=metrics[1]
        mftp_all[i]=metrics[2]
        wbal_all[i]=metrics[3]
        print(i)}
    }} else {
      condition1=length(pmax_all)+1
      condition2=as.numeric(tail(dates_list_2,1)-dates_list_2[1])-period+1
      for (i in condition1:condition2){
        max_mmp=apply(mmp_all_df[,which(dates_list_2 %in% dates[i]:(dates[i]+period))],1,max)
        metrics=metrics_model(max_mmp,T,weight)
        pmax_all[i]=metrics[1]
        mftp_all[i]=metrics[2]
        wbal_all[i]=metrics[3]
        print(i)
      }
    }
  options(warn=0)
  return(list(pmax_all,mftp_all,wbal_all))
}
