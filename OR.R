#Basic Input Information
tab<-c(a,b,c,d) 
#'a' = exposed and diseased 
#'b' = non-exposed but diseased
#'c' = exposed but not diseased 
#'d' = non-exposed and not diseased

n1<-a+c # total exposed
n0<-b+d # total non-exposed
m1<-a+b # total diseased
m0<-c+d # total non-diseased

#Function for calculating the Crude Odds Ratio
crudeOR<- function(a,b,c,d){
  ((a*d)/(b*c))
}

#Function for calculating the Odds Ratio with Confidence Intervals
# 95% Confidence Interval is the default setting
ciOR<-function(a,b,c,d,ci=95){
  x<-log((a*d)/(b*c))
  OR<- ((a*d)/(b*c))
  var<-((1/a)+(1/b)+(1/c)+(1/d))
  se<- sqrt(var)
  z<- 1-(.5*((100-ci)/100))
  upper.CI <- exp(x + (qnorm(z) * se))
  lower.CI <- exp(x - (qnorm(z) * se))
  print(OR)
  print(paste(ci, "% CI: ", "(", round(lower.CI,2), " to ", round(upper.CI,2), ")", sep=""))
  if (a<0 | b<0 | c<0| d<0){
    print("Warning: cannot have negative value")
  }
  if (ci<0){
    print("Warning: cannot have negative value for confidence interval")
  }
}





