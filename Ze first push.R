tab<-c(a,b,c,d)

CIRD=(a/c)-(b/d)
CIRR=(a/c)/(b/d)


CICIRD<-function(a,b,c,d,ci){
  x<-(a/c)-(b/d)
  a<-(100-ci)/200
  z<-qnorm(a,lower.tail=FALSE)
  varx<-sqrt(a/(c*c)+b/(d*d))
  lower.ci<-x-z*varx
  upper.ci<-x+z*varx
  print(paste(ci, "%CI:", "(", round(lower.ci,2), ",", round(upper.ci,2),")", sep = ""))
}

CICIRR<-function(a,b,c,d,ci){
  x<-log((a/c)/(b/d))
  z<-qnorm((100-ci)/200,lower.tail=FALSE)
  varx<-sqrt(1/a+1/b)
  lower.ci<-exp(x-z*varx)
  upper.ci<-exp(x+z*varx)
  print(paste(ci, "%CI:", "(", round(lower.ci,2), ",", round(upper.ci,2),")", sep = ""))}

