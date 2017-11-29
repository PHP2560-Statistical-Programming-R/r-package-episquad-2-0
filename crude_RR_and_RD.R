#input data is in a 2X2 table format where a represent exposed and diseased, b represent not exposed but diseased, c represent exposed but not diseased and d represent not exposed and no disease

tab<-c(a,b,c,d)
n1<-a+c
n0<-b+d
m1<-a+b
m0<-c+d

#function to calculate crude risk ratio
crudeRR<-function(a,b,c,d){
  (a/(a+c))/(b/(b+d))}

#function to calculate crude risk difference
crudeRD<-function(a,b,c,d){
  (a/(a+c))-(b/(b+d))}


#function for confidence interval of risk ratio
ciRR<-function(a,b,c,d, ci=95){
  x<-log((a/(a+c))/(b/(b+d)))
  varx<-(c/(a*(a+c))) + (d/(b*(b+d)))
  alpha<-(100-ci)/100
  z<-1-alpha/2
  upper.ci<- exp(x + (qnorm(z) * sqrt(varx)))
  lower.ci<- exp(x - (qnorm(z) * sqrt(varx)))
  print(paste(ci, "% CI: ", "(", round(lower.ci,2), " to ", round(upper.ci,2), ")", sep=""))
  print(x)
}
ciRR(45, 14, 642, 474)
