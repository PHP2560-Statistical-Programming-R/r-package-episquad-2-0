#Hypothesis Test Chi Square test for stratified data:
stratified.test<-function(data) {#data should be a list of tables
  if(length(data) %in% c(2,3,4,5)){
    x<-c()
    ex<-c()
    varx<-c()
    for (l in 1:length(data)) {
      x[[l]]<-data[[l]][1,1]
      ex[[l]]<-data[[l]][1,3]*data[[l]][2,1]/data[[l]][2,3]
      varx[[l]]<-data[[l]][1,3]*data[[l]][2,1]*data[[l]][2,2]/(data[[l]][2,3]*data[[l]][2,3])
      zsquare<-(sum(x)-sum(ex))^2/sum(varx) #calculate the z-square in the hypothesis test
      pvalue<-pchisq(zsquare,1,lower.tail = F) #calculate the p-value in the hypothesis test
      output<-cbind(zsquare,pvalue)}
  }
  else{output<-c("this function is designed for 2 to 5 stratified tables to do the Chi square test")}
  return(output)
}



