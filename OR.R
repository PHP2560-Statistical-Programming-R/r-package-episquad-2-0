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


#Function for calculating the Odds Ratio with Confidence Intervals
# 95% Confidence Interval is the default setting
OR<-function(a,b,c,d,ci=95){
  x<-log((a*d)/(b*c))
  OR<- ((a*d)/(b*c))
  var<-((1/a)+(1/b)+(1/c)+(1/d))
  se<- sqrt(var)
  z<- 1-(.5*((100-ci)/100))
  upper.CI <- exp(x + (qnorm(z) * se))
  lower.CI <- exp(x - (qnorm(z) * se))
  print(paste("The Odds Ratio is", round(OR, 2), sep=" "))
  print(paste(ci, "% CI: ", "(", round(lower.CI,2), " to ", round(upper.CI,2), ")", sep=""))
  if (a<0 | b<0 | c<0| d<0){
    print("Warning: cannot have negative value")
  }
  if (ci<0){
    print("Warning: cannot have negative value for confidence interval")
  }
}

#Function for calculating the Odds Ratio Using Mantel-Haenszel Weights
ORmh<- function(a,b,c,d, ci=95){
  ORmh<- (b/c)
  x<- log(ORmh)
  var<- (1/b) + (1/c)
  se<- sqrt(var)
  z<- 1-(.5*((100-ci)/100))
  upper.CI <- exp(x + (qnorm(z) * se))
  lower.CI <- exp(x - (qnorm(z) * se))
  print(paste("the Mantel-Haenszel Odds Ratio is", round(ORmh, 2), sep=" "))
  print(paste(ci, "% CI: ", "(", round(lower.CI,2), " to ", round(upper.CI,2), ")", sep=""))
}

#Hypothesis Testing

testOR <- function(a,b,c,d){
  Expected<- ((a+b)*(a+c)/(a+b+c+d))
  Variance<- (((a+b)*(b+d)*(c+d)*(a+c))/(((a+b+c+d)^2)*((a+b+c+d)-1)))
  Zsquared <-((a-Expected)^2)/Variance
  pvalue <-round((pchisq(Zsquared, df= 1, lower.tail = F)), 5)
  output<-cbind(Zsquared, pvalue)
  print(output)
  if (pvalue< 0.05){
    print ("Reject the null hypothesis at p < 0.05")
  } else print("Fail to reject the null hypothesis")
  if (a<0 | b<0 | c<0| d<0){
    print("Warning: cannot have negative value")
  }
}

testORmh <- function(a,b,c,d){
  Zsquared <-((b-c)^2)/(b+c)
  pvalue <-round((pchisq(Zsquared, df= 1, lower.tail = F)), 5)
  output<-cbind(Zsquared, pvalue)
  print(output)
  if (pvalue< 0.05){
    print ("Reject the null hypothesis at p<0.05")
  } else print("Fail to reject the null hypothesis")
  if (a<0 | b<0 | c<0| d<0){
    print("Warning: cannot have negative value")
  }
}






