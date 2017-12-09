#Function for calculating the Odds Ratio with Confidence Intervals
# 95% Confidence Interval is the default setting
OR<- function(table, ci=95){
  x<- log((table[1,1]*table[2,2]/table[1,2]*table[2,1]))
  OR<- (table[1,1]*table[2,2]/table[1,2]*table[2,1])
  var<- ((1/table[1,1])+(1/table[1,2])+(1/table[2,1])+(1/table[2,2]))
  se<- sqrt(var)
  z<- 1-(.5*((100-ci)/100))
  upper.CI <- exp(x + (qnorm(z) * se))
  lower.CI <- exp(x - (qnorm(z) * se))
  print(paste("Odds Ratio", round(OR, 2), ", ", ci, "% CI: ", "(", round(lower.CI,2), " to ", round(upper.CI,2), ")", sep=""))

}


#Function for calculating the Odds Ratio Using Mantel-Haenszel Weights


ORmh <- function(table, ci=95){
  ORmh<- (table[2,1]/table[1,2])
  x<- log(ORmh)
  var<- (1/table[1,2])+(1/table[2,1])
  se<-sqrt(var)
  z<- 1-(.5*((100-ci)/100))
  upper.CI <- exp(x + (qnorm(z) * se))
  lower.CI <- exp(x - (qnorm(z) * se))
  print(paste("the Mantel-Haenszel Odds Ratio is", round(ORmh, 2), sep=" "))
  print(paste(ci, "% CI: ", "(", round(lower.CI,2), " to ", round(upper.CI,2), ")", sep=""))
}







