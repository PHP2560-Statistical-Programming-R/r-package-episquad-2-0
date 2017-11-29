tab<-c(a,b,c,d)

CIRD<-function(a,b,c,d){
  IRD<-(a/c)-(b/d)
print(paste("IRD:",IRD))
}

CIRR=function(a,b,c,d){
  IRR<-(a/c)/(b/d)
  print(paste("IRR:",IRR))
}


CICIRD<-function(a,b,c,d,ci){
  x<-(a/c)-(b/d)
  y<-(100-ci)/200
  z<-qnorm(y,lower.tail=FALSE)
  varx<-a/(c*c)+b/(d*d)
  lower.ci<-x-z*sqrt(varx)
  upper.ci<-x+z*sqrt(varx)
  print(paste(ci, "%CI:", "(", lower.ci, ",", upper.ci,")", sep = ""))
}

CICIRR<-function(a,b,c,d,ci){
  x<-log((a/c)/(b/d))
  y<-(100-ci)/200
  z<-qnorm(y,lower.tail=FALSE)
  varx<-sqrt(1/a+1/b)
  lower.ci<-exp(x-z*varx)
  upper.ci<-exp(x+z*varx)
  print(paste(ci, "%CI:", "(", round(lower.ci,2), ",", round(upper.ci,2),")", sep = ""))}

HpTest<-function(a,b,c,d){
  ex<-(a+b)*c/(c+d)
  varx<-(a+b)*c*d/((c+d)*(c+d))
  zsquare<-(a-ex)*(a-ex)/varx
  pvalue<-pchisq(zsquare,1,lower.tail = F)
  cbind(zsquare,pvalue)
}

#Analysis of Stratified Data
