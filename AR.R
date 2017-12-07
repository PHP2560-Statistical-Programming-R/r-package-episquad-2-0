#Basic Input Information

#'a' = exposed and diseased
#'b' = non-exposed but diseased
#'c' = exposed but not diseased
#'d' = non-exposed and not diseased


table<- function(a,b,c,d){
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


AR<- function(table, ci=95){
  AR<- ((table[1,1]/table[3,1])-(table[1,2]/table[3,2]))
  se<- sqrt(((1/table[3,1])+(1/table[3,2]))*(1-(table[1,3]/table[3,3]))*(table[1,3]/table[3,3]))
  z<- 1-(.5*((100-ci)/100))
  upper.CI <- AR + (qnorm(z)*se)
  lower.CI <- AR - (qnorm(z)*se)
  print(paste("The Attributable Risk is", round(AR,3)))
  print(paste(ci, "% CI: ", "(", round(lower.CI,3), " to ", round(upper.CI,3), ")", sep=""))
  if (ci<0){
    print("Warning: cannot have negative value for confidence interval")
  }
}

ARpercent<- function(table, ci=95){
  AR<- ((table[1,1]/table[3,1])-(table[1,2]/table[3,2]))
  ARpercent<- (((table[1,1]/table[3,1])-(table[1,2]/table[3,2]))/(table[1,1]/table[3,1]))*100
  se<- sqrt(((1/table[3,1])+(1/table[3,2]))*(1-(table[1,3]/table[3,3]))*(table[1,3]/table[3,3]))
  z<- 1-(.5*((100-ci)/100))
  ARpercent.upper.CI <- ARpercent + ARpercent*(qnorm(z)*se/AR)
  ARpercent.lower.CI <- ARpercent - ARpercent*(qnorm(z)*se/AR)
  print(paste("The Attributable Risk Percent is", round(ARpercent, 3), "%", sep=" "))
  print(paste(ci, "% CI: ", "(", round(ARpercent.lower.CI,3), " to ", round(ARpercent.upper.CI,3), ")", sep=""))
  if (ci<0){
    print("Warning: cannot have negative value for confidence interval")
  }
}
#Confidence Interval around AR% calculated according to Rosenberg 1998

