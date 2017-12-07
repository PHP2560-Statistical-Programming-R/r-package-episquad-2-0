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


#PAR
PAR<- function(table, ci=95){
  PAR<- ((table[1,1]/table[3,1])-(table[1,2]/table[3,2]))*((table[3,1]/table[3,3]))
  se<- sqrt((((table[2,1]*table[3,2])/(table[2,2]*table[3,1]))^2)*((table[1,1]/(table[2,1]*table[3,1]))+(table[1,2]/(table[2,2]*table[3,2]))))
  z<- 1-(.5*((100-ci)/100))
  PAR.upper.CI <- PAR + (qnorm(z) * se)
  PAR.lower.CI <- PAR - (qnorm(z) * se)
  print(paste("The Population Attributable Risk is", round(PAR,3)))
  print(paste(ci, "% CI: ", "(", round(PAR.lower.CI,2), " to ", round(PAR.upper.CI,2), ")", sep=""))
  if (ci<0){
    print("Warning: cannot have negative value for confidence interval")
  }
}

#PAR % with Confidence Intervals
PARpercent<- function(table, ci=95){
  PARpercent<- ((table[1,3]/table[3,3]-(table[1,2]/table[3,2]))/(table[1,3]/table[3,3])*100)
  sePARpercent<- sqrt((((table[2,1]*table[3,2])/(table[2,2]*table[3,1]))^2)*((table[1,1]/(table[2,1]*table[3,1]))+(table[1,2]/(table[2,2]*table[3,2]))))*100
  z<- 1-(.5*((100-ci)/100))
  PARpercent.upper.CI <- PARpercent + (qnorm(z) * sePARpercent)
  PARpercent.lower.CI <- PARpercent - (qnorm(z) * sePARpercent)
  print(paste("The Population Attributable Risk Percent is", round(PARpercent,3), "%", sep=" "))
  print(paste(ci, "% CI: ", "(", round(PARpercent.lower.CI,2), " to ", round(PARpercent.upper.CI,2), ")", sep=""))
  if (ci<0){
    print("Warning: cannot have negative value for confidence interval")
  }
}

