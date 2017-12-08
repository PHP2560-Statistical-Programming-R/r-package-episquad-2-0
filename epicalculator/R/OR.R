#Basic Input Information

#'a' = exposed and diseased
#'b' = non-exposed but diseased
#'c' = exposed but not diseased
#'d' = non-exposed and not diseased

tablex<- function(a,b,c,d){
  n1<-a+c # total exposed
  n0<-b+d # total non-exposed
  m1<-a+b # total diseased
  m0<-c+d # total non-diseased
  N<- a+b+c+d # total
  tab<- matrix(c(a, b, m1, c, d, m0, n1, n0, N), ncol=3, byrow = T) #create a matrix with the marginal values
  colnames(tab) <- c('Exposure', 'No Exposure', 'Total')
  rownames(tab) <- c('Disease', 'No Disease', 'Total')
  tab.table <- as.table(tab)
  return(tab.table)
  if (a<0 | b<0 | c<0| d<0){
    print("Warning: cannot have negative value")
  }
}


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







