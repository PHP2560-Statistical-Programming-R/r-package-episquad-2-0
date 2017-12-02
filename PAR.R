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
N<- a+b+c+d # total

#PAR and PAR % with Confidence Intervals
PAR<- function(a,b,c,d,ci=95){
  PAR<- ((a/(a+c))-(b/(b+d))) * ((a+c)/(a+b+c+d))
  PARpercent<- (((a+b)/(a+b+c+d)-(b/(b+d)))/((a+b)/(a+b+c+d))*100)
  se<-sqrt((((c*(b+d)) / (d*(a+c)))^2)* ((a/(c*(a+c))) + (b/(d*(b+d)))))
  sePARpercent<-sqrt((((c*(b+d)) / (d*(a+c)))^2)* ((a/(c*(a+c))) + (b/(d*(b+d))))) *100
  z<- 1-(.5*((100-ci)/100))
  PAR.upper.CI <- PAR + (qnorm(z) * se)
  PAR.lower.CI <- PAR - (qnorm(z) * se)
  PARpercent.upper.CI <- PARpercent + (qnorm(z) * sePARpercent)
  PARpercent.lower.CI <- PARpercent - (qnorm(z) * sePARpercent)
  print(paste("The Population Attributable Risk is", round(PAR,3)))
  print(paste(ci, "% CI: ", "(", round(PAR.lower.CI,2), " to ", round(PAR.upper.CI,2), ")", sep=""))
  print(paste("The Population Attributable Risk Percent is", round(PARpercent,3), "%", sep=" "))
  print(paste(ci, "% CI: ", "(", round(PARpercent.lower.CI,2), " to ", round(PARpercent.upper.CI,2), ")", sep=""))
}