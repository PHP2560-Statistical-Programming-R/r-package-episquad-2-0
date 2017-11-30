#input data is in a 2X2 table format where a represent exposed and diseased, b represent not exposed but diseased, c represent exposed but not diseased and d represent not exposed and no disease
#function to create a 2X2 table
my.table<- function(a,b,c,d){
  m1<-a+b #total diseased
  m0<-c+d #total no disease
  n1<-a+c #total exposed
  n0<-b+d #total unexposed
  t<-a+b+c+d #total people
  tab<- matrix(c(a, b, m1, c, d, m0, n1, n0, t), ncol=3, byrow = T) #create a matrix with the marginal values
  colnames(tab) <- c('Exposure', 'No Exposure', "Total")
  rownames(tab) <- c('Disease', 'No Disease', "Total")
  tab.table <- as.table(tab)
  return(tab.table)
}
my.table(45, 14, 642, 474)


#function to calculate crude risk ratio and risk difference
crude.Risk<-function(a,b,c,d, measure=c("RR", "RD")){ #will calculate risk ratio and risk difference
  dat<-my.table(a,b,c,d) # my.table function will calculate the margianl totals
  if (measure == "RR") {
  rr<-(dat[1,1]/dat[3,1]) / (dat[1,2]/dat[3,2]) #rr is the risk ratio
  return(round(rr,4))}
  else {rd<-(dat[1,1]/dat[3,1]) - (dat[1,2]/dat[3,2]) #rd is risk difference
  return(round(rd,4))}
}
crude.Risk(45, 14, 642, 474, measure="RR")




#function for confidence interval of risk ratio or risk difference
ci.RR<-function(a,b,c,d, ci=95){
  dat<-my.table(a,b,c,d) # my.table function will calculate the margianl totals
  alpha<-(100-ci)/100
  z<-1-alpha/2
  x<-log((dat[1,1]/dat[3,1])/(dat[1,2]/dat[3,2]))
  varx<-(dat[2,1]/(dat[1,1]*(dat[1,1]+dat[2,1]))) + (dat[2,2]/(dat[1,2]*(dat[1,2]+dat[2,2])))
  upper.ci<- exp(x + (qnorm(z) * sqrt(varx)))
  lower.ci<- exp(x - (qnorm(z) * sqrt(varx)))
  print(paste(ci, "% CI: ", "(", round(lower.ci,2), " to ", round(upper.ci,2), ")", sep=""))
}

ci.RR(45, 14, 642, 474, ci=90)

#function for confidence interval of risk ratio
ci.RD<-function(a,b,c,d, ci=95){
  dat<-my.table(a,b,c,d) # my.table function will calculate the margianl totals
  alpha<-(100-ci)/100
  z<-1-alpha/2
  x<-(dat[1,1]/dat[3,1])-(dat[1,2]/dat[3,2])
  varx<-((dat[1,1]*dat[2,1])/dat[3,1]^3) + ((dat[1,2]*dat[2,2])/dat[3,2]^3)
  upper.ci<- x + (qnorm(z) * sqrt(varx))
  lower.ci<- x - (qnorm(z) * sqrt(varx))
  print(paste(ci, "% CI: ", "(", round(lower.ci,2), " to ", round(upper.ci,2), ")", sep=""))
}

ci.RD(45, 14, 642, 474)

#hypothesis testing for RR and RD :
test.Risk<-function(a,b,c,d){
  dat<-my.table(a,b,c,d) # my.table function will calculate the margianl totals
  x<-dat[1,1]
  expected<-(dat[1,3]*dat[3,1]) / dat[3,3]
  var.x<-(dat[1,3]*dat[2,3]*dat[3,1]*dat[3,2]) / dat[3,3]^3
  z.square<-round(((x-expected)^2 / var.x), 3)
  p<-round((pchisq(z.square, df= 1, lower.tail = F)), 5)
  out<-cbind(z.square, p)
  return(out)
  }
test.Risk(45, 14, 642, 474)




