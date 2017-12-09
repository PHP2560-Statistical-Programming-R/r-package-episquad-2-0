#function to calculate confidence interval for summary IRR
summary.rate<-function(data, ci, measure=c("IRR","IRD"))  #data should be a list of tables
{
  alpha<-(100-ci)/200
  z<-qnorm(alpha,lower.tail=FALSE)
  weight.strata<-c() #vector that will contain the weight of each stratum
  irr.strata <- c() #vector that will contain the IRR of each stratum
  ird.strata<-c() #vector that will contain the IRD of each stratum
  if(length(data) %in% c(2,3,4,5)){
    upper <- c() #will contain the numerator of variance
    lowerA <- c() #will keep a part of denominator
    lowerB <- c() #will keep another part of denominator
    
    if(measure == "IRR")
      {for (l in 1:length(data)){ #this loop will calculate the stratum-specific estimates that we need for variance
        
        weight.strata[[l]]<-data[[l]][1,2]*data[[l]][2,1]/data[[l]][2,3]
      irr.strata[[l]]<-(data[[l]][1,1]/data[[l]][2,1]) / (data[[l]][1,2]/data[[l]][2,2])
      summary.rate.ratio<-sum(weight.strata*irr.strata)/sum(weight.strata)
       upper[[l]]<-data[[l]][1,3]*data[[l]][2,1]*data[[l]][2,2]/(data[[l]][2,3]*data[[l]][2,3])
      lowerA[[l]]<-data[[l]][1,1]*data[[l]][2,2]/data[[l]][2,3]
      lowerB[[l]]<-data[[l]][1,2]*data[[l]][2,1]/data[[l]][2,3]
      x<-log(summary.rate.ratio)
      varx<-sum(upper)/(sum(lowerA)*sum(lowerB))
      lower.ci<-exp(x-z*sqrt(varx))
      upper.ci<-exp(x+z*sqrt(varx)) #calculate upper and lower confidence interval
      output<-paste("Summary IRR: ", round(summary.rate.ratio,2), " ", ci, "%CI: ", "(", round(lower.ci,2), " to ", round(upper.ci,2), ")", sep="")
      }} else
      {for (l in 1:length(data)){ #this loop will calculate the stratum-specific estimates that we need for variance
        weight.strata[[l]]<-1/((1/data[[l]][1,1])+(1/data[[l]][1,2]))
        ird.strata[[l]]<-(data[[l]][1,1]/data[[l]][2,1]) - (data[[l]][1,2]/data[[l]][2,2])
        summary.rate.difference<-sum(weight.strata*ird.strata)/sum(weight.strata)
      
        x<-summary.rate.difference
        varx<-1/sum(weight.strata)
        lower.ci<-x-z*sqrt(varx)
        upper.ci<-x+z*sqrt(varx) #calculate upper and lower confidence interval
        output<-paste("Summary IRD: ", round(summary.rate.difference,2), " ", ci, "%CI: ", "(", round(lower.ci,2), " to ", round(upper.ci,2), ")", sep="")
      }}
    
      }

  else {output<-c("this function is designed for 2 to 5 stratified tables to calculate summary IRR confidence interval")}
  return(output)}

