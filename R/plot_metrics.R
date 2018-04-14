plot_metrics=function(dates_list,pmax_all=NULL,wbal_all=NULL,mftp_all=NULL,period=90){

  if(length(pmax_all)!=0){
    plot(seq(dates_list[1]+period,tail(dates_list,1),length.out=length(pmax_all)),pmax_all,type="l",lwd=2,col="purple",xaxt="n",main=paste("PMax: Rolling",period,"days"),xlab="Date",ylab="Watts")
    axis(side=1,at=seq(dates_list[1]+period,tail(dates_list,1),length.out=8),labels=format(seq(dates_list[1]+period,tail(dates_list,1),length.out=8),'%b-%y'))
    abline(v=seq(dates_list[1]+period,tail(dates_list,1),length.out=8),col="grey",lwd=0.5,lty="dotted")
    grid(NA,NULL)}

  if(length(wbal_all)!=0){
    plot(seq(dates_list[1]+period,tail(dates_list,1),length.out=length(wbal_all)),wbal_all,type="l",lwd=2,col="orange",xaxt="n",main=paste("W': Rolling",period,"days"),xlab="Date",ylab="Joules")
    axis(side=1,at=seq(dates_list[1]+period,tail(dates_list,1),length.out=8),labels=format(seq(dates_list[1]+period,tail(dates_list,1),length.out=8),'%b-%y'))
    abline(v=seq(dates_list[1]+period,tail(dates_list,1),length.out=8),col="grey",lwd=0.5,lty="dotted")
    grid(NA,NULL)}

  if(length(mftp_all)!=0){
    plot(seq(dates_list[1]+period,tail(dates_list,1),length.out=length(mftp_all)),mftp_all,type="l",lwd=2,col="red",xaxt="n",main=paste("Modeled FTP: Rolling",period,"days"),xlab="Date",ylab="Watts")
    axis(side=1,at=seq(dates_list[1]+period,tail(dates_list,1),length.out=8),labels=format(seq(dates_list[1]+period,tail(dates_list,1),length.out=8),'%b-%y'))
    abline(v=seq(dates_list[1]+period,tail(dates_list,1),length.out=8),col="grey",lwd=0.5,lty="dotted")
    grid(NA,NULL)}}
