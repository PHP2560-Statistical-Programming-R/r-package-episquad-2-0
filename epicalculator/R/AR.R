AR<- function(table, ci=95){
  AR<- ((table[1,1]/table[3,1])-(table[1,2]/table[3,2]))
  se<- sqrt(((1/table[3,1])+(1/table[3,2]))*(1-(table[1,3]/table[3,3]))*(table[1,3]/table[3,3]))
  z<- 1-(.5*((100-ci)/100))
  upper.CI <- AR + (qnorm(z)*se)
  lower.CI <- AR - (qnorm(z)*se)
  print(paste("Attributable Risk ", round(AR,3), ", ", ci, "% CI: ", "(", round(lower.CI,3), " to ", round(upper.CI,3), ")", sep=""))
}

ARpercent<- function(table, ci=95){
  AR<- ((table[1,1]/table[3,1])-(table[1,2]/table[3,2]))
  ARpercent<- (((table[1,1]/table[3,1])-(table[1,2]/table[3,2]))/(table[1,1]/table[3,1]))*100
  se<- sqrt(((1/table[3,1])+(1/table[3,2]))*(1-(table[1,3]/table[3,3]))*(table[1,3]/table[3,3]))
  z<- 1-(.5*((100-ci)/100))
  ARpercent.upper.CI <- ARpercent + ARpercent*(qnorm(z)*se/AR)
  ARpercent.lower.CI <- ARpercent - ARpercent*(qnorm(z)*se/AR)
  print(paste("Attributable Risk Percent ", round(ARpercent,3), "%", ", ", ci, "% CI: ", "(", round(ARpercent.lower.CI,2), "% to ", round(ARpercent.upper.CI,2), "%)", sep=""))
  }
#Confidence Interval around AR% calculated according to Rosenberg 1998

