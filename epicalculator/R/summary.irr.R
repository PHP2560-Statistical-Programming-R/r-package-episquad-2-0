#function to calculate summary IRR using Mantel‐Haenszel weights
summary.irr<-function(data) #data should be a list of tables
{
  weight.strata<-c() #vector that will contain the weight of each stratum
  irr.strata <- c() #vector that will contain the RR of each stratum
  if (length(data) %in% c(2,3,4,5)){
    for (l in 1:length(data)){ #this loop will calculate the RR and weight of each stratum
      weight.strata[[l]]<-data[[l]][1,2]*data[[l]][2,1]/data[[l]][2,3]
      irr.strata[[l]]<-crude.Rate(data[[l]], measure = "IRR")
      summary.rate.ratio<-sum(weight.strata*irr.strata)/sum(weight.strata)}
  }

  else {summary.rate.ratio<-c("this function is designed for 2 to 5 stratified tables to calculate MH weighted IRR")}
  return(summary.rate.ratio)
}

