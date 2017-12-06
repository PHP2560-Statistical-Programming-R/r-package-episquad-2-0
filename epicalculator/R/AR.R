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


AR<- function(a,b,c,d, ci=95){
  AR<- ((a/(a+c))-(b/(b+d)))
  se<- sqrt(((1/(a+c))+(1/(b+d)))*(1-((a+b)/(a+b+c+d)))*((a+b)/(a+b+c+d)))
  z<- 1-(.5*((100-ci)/100))
  upper.CI <- AR + (qnorm(z)*se)
  lower.CI <- AR - (qnorm(z)*se)
  ARpercent <- (((a/(a+c)) - (b/(b+d)))/ (a/(a+c)))*100
  print(paste("The Attributable Risk is", round(AR,3)))
  print(paste(ci, "% CI: ", "(", round(lower.CI,3), " to ", round(upper.CI,3), ")", sep=""))
  print(paste("The Attributable Risk Percent is", round(ARpercent, 3), "%", sep=" "))
  ARpercent.upper.CI <- ARpercent + ARpercent*(qnorm(z)*se/AR)
  ARpercent.lower.CI <- ARpercent - ARpercent*(qnorm(z)*se/AR)
  print(paste(ci, "% CI: ", "(", round(ARpercent.lower.CI,3), " to ", round(ARpercent.upper.CI,3), ")", sep=""))
}

#Confidence Interval around AR% calculated according to Rosenberg 1998


