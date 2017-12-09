#PAR
PAR<- function(table, ci=95){
  PAR<- ((table[1,1]/table[3,1])-(table[1,2]/table[3,2]))*((table[3,1]/table[3,3]))
  se<- sqrt((((table[2,1]*table[3,2])/(table[2,2]*table[3,1]))^2)*((table[1,1]/(table[2,1]*table[3,1]))+(table[1,2]/(table[2,2]*table[3,2]))))
  z<- 1-(.5*((100-ci)/100))
  PAR.upper.CI <- PAR + (qnorm(z) * se)
  PAR.lower.CI <- PAR - (qnorm(z) * se)
  print(paste("Population Attributable Risk ", round(PAR,3), ", ", ci, "% CI: ", "(", round(PAR.lower.CI,3), " to ", round(PAR.upper.CI,3), ")", sep=""))


}

#PAR % with Confidence Intervals
PARpercent<- function(table, ci=95){
  PARpercent<- ((table[1,3]/table[3,3]-(table[1,2]/table[3,2]))/(table[1,3]/table[3,3])*100)
  sePARpercent<- sqrt((((table[2,1]*table[3,2])/(table[2,2]*table[3,1]))^2)*((table[1,1]/(table[2,1]*table[3,1]))+(table[1,2]/(table[2,2]*table[3,2]))))*100
  z<- 1-(.5*((100-ci)/100))
  PARpercent.upper.CI <- PARpercent + (qnorm(z) * sePARpercent)
  PARpercent.lower.CI <- PARpercent - (qnorm(z) * sePARpercent)
  print(paste("Population Attributable Risk Percent ", round(PARpercent,3), "%", ", ", ci, "% CI: ", "(", round(PARpercent.lower.CI,2), "% to ", round(PARpercent.upper.CI,2), "%)", sep=""))
}

