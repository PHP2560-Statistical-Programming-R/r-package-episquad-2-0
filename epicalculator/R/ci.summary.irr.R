#function to calculate confidence interval for summary IRR
ci.summary.irr<-function(data,ci)  #data should be a list of tables
{
  alpha<-(100-ci)/200
  z<-qnorm(alpha,lower.tail=FALSE)
  if(length(data) %in% c(2,3,4,5)){
    upper <- c() #will contain the numerator of variance
    lowerA <- c() #will keep a part of denominator
    lowerB <- c() #will keep another part of denominator
    for (l in 1:length(data)){ #this loop will calculate the stratum-specific estimates that we need for variance
      upper[[l]]<-data[[l]][1,3]*data[[l]][2,1]*data[[l]][2,2]/(data[[l]][2,3]*data[[l]][2,3])
      lowerA[[l]]<-data[[l]][1,1]*data[[l]][2,2]/data[[l]][2,3]
      lowerB[[l]]<-data[[l]][1,2]*data[[l]][2,1]/data[[l]][2,3]
      x<-log(summary.irr(data))
      varx<-sum(upper)/(sum(lowerA)*sum(lowerB))
      lower.ci<-exp(x-z*sqrt(varx))
      upper.ci<-exp(x+z*sqrt(varx)) #calculate upper and lower confidence interval
      ci<-paste(round(lower.ci,2),round(upper.ci,2))
    }
  }
  else {ci<-c("this function is designed for 2 to 5 stratified tables to calculate summary IRR confidence interval")}
  return(ci)}

