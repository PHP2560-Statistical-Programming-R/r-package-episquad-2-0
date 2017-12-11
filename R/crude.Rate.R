#function for confidence interval of rate ratio or rate difference
crude.rate<-function(table, ci, measure=c("IRR","IRD")) {#will calculate rate ratio and rate difference confidence interval
  alpha<-(100-ci)/200
  z<-qnorm(alpha,lower.tail=FALSE)
  if (measure == "IRR") {
    irr<-(table[1,1]/table[2,1]) / (table[1,2]/table[2,2]) #irr is the rate ratio
    x<-log((table[1,1]/table[2,1])/(table[1,2]/table[2,2]))
    varx<-sqrt(1/table[1,1]+1/table[1,2])
    lower.ci<-exp(x-z*varx)
    upper.ci<-exp(x+z*varx)#calculate upper and lower confidence interval
    print(paste("IRR: ",round(irr,5), " ", ci, "%CI: ", "( ", round(lower.ci,5), " to ", round(upper.ci,5), " )", sep=""))
  }
  else {
  ird<-(table[1,1]/table[2,1]) - (table[1,2]/table[2,2])
  x<-table[1,1]/table[2,1]-table[1,2]/table[2,2]
  varx<-table[1,1]/(table[2,1]*table[2,1])+table[1,2]/(table[2,2]*table[2,2])
  lower.ci<-x-z*sqrt(varx)
  upper.ci<-x+z*sqrt(varx)#calculate upper and lower confidence interval
  print(paste( "IRD: ",round(ird,5)," ",ci, "%CI: ", "( ", round(lower.ci,5), " to ", round(upper.ci,5), " )", sep=""))
  }
}


