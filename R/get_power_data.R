get_power_data=function(){
## R script will run on selection.
metrics=GC.season.metrics()

#removing activites withou power
metrics=subset(metrics,!Average_Power==0)
rownames(metrics)=1:dim(metrics)[1]

#getting dates and times of selected period activities
dates_list_updt=metrics$time
#getting power data from those activities
power_list_updt=list()
for (i in 1:length(dates_list_updt)){
  power_list_updt[[i]]=GC.activity(activity=dates_list_updt[i])[[1]]$power
  }
return(list(dates_list_updt,power_list_updt))
}
