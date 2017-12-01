#input data is in a 2X2 table format where a represent exposed and diseased, b represent not exposed but diseased, c represent exposed person-time and d represent not exposed person-time
crude.table<- function(a,b,c,d){
  m1<-a+b #total diseased
  t<-c+d #total person-time
  tab<- matrix(c(a, b, m1, c, d, t), ncol=3, byrow = T) #create a matrix with the marginal values
  colnames(tab) <- c('Exposure', 'No Exposure', "Total")
  rownames(tab) <- c('Disease', 'PersonTime')
  tab.table <- as.table(tab)
  return(tab.table)
}
t1<-crude.table(41,15,28010,19017)



#function to calculate crude rate ratio and rate difference
crude.Rate<-function(table, measure=c("IRR", "IRD")){ #will calculate rate ratio and rate difference
  if (measure == "IRR") {
    irr<-(table[1,1]/table[2,1]) / (table[1,2]/table[2,2]) #irr is the rate ratio
    return(round(irr,5))}
  else {ird<-(table[1,1]/table[2,1]) - (table[1,2]/table[2,2]) #ird is risk difference
  return(round(ird,5))}
}
crude.Rate(t1,measure = "IRR")

#function for confidence interval of rate ratio or rate difference
rate.ci<-function(table, ci, measure=c("IRR","IRD")) {#will calculate rate ratio and rate difference confidence interval
  alpha<-(100-ci)/200
  z<-qnorm(alpha,lower.tail=FALSE)
  if (measure == "IRR") {
  x<-log((table[1,1]/table[2,1])/(table[1,2]/table[2,2]))
  varx<-sqrt(1/table[1,1]+1/table[1,2])
  lower.ci<-exp(x-z*varx)
  upper.ci<-exp(x+z*varx)
  print(paste("IRD ", ci, "%CI: ", "(", round(lower.ci,5), " to ", round(upper.ci,5), ")", sep=""))
}
else {x<-table[1,1]/table[2,1]-table[1,2]/table[2,2]
      varx<-table[1,1]/(table[2,1]*table[2,1])+table[1,2]/(table[2,2]*table[2,2])
      lower.ci<-x-z*sqrt(varx)
      upper.ci<-x+z*sqrt(varx)
      print(paste("IRR ", ci, "%CI: ", "(", round(lower.ci,5), " to ", round(upper.ci,5), ")", sep=""))
}
}


#Hypothesis Test Chi Square test for crude data:

test.Rate<-function(table){
  ex<-table[1,3]*table[2,1]/table[2,3]
  varx<-table[1,3]*table[2,1]*table[2,2]/(table[2,3]*table[2,3])
  zsquare<-(table[1,1]-ex)*(table[1,1]-ex)/varx
  pvalue<-pchisq(zsquare,1,lower.tail = F)
  cbind(zsquare,pvalue)
}

#Analysis of Stratified Data
#create table for stratified data
stratified.table<-function(data) # data should be a set of vectors
  {
  if (length(data) == 8){
    tab1<- matrix(c(data[1], data[2], (data[1]+data[2]), data[3], data[4], (data[3]+data[4])), ncol=3, byrow = T) #create a matrix with the marginal values
    colnames(tab1) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab1) <- c('Disease', 'PersonTime')
    tab.table1 <- as.table(tab1) #change the matrix as a table
    tab2<- matrix(c(data[5], data[6], (data[5]+data[6]), data[7], data[8], (data[7]+data[8])), ncol=3, byrow = T) #create a matrix with the marginal values
    colnames(tab2) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab2) <- c('Disease', 'PersonTime')
    tab.table2 <- as.table(tab2)#change the matrix as a table
    tab.table<-list(stratification.level.1= tab.table1, stratification.level.2= tab.table2)} #put table1 and table 2 in a list
   else if (length(data) == 12){
     tab1<- matrix(c(data[1], data[2], (data[1]+data[2]), data[3], data[4], (data[3]+data[4])), ncol=3, byrow = T) #create a matrix with the marginal values
     colnames(tab1) <- c('Exposure', 'No Exposure', "Total")
     rownames(tab1) <- c('Disease', 'PersonTime')
     tab.table1 <- as.table(tab1) #change the matrix as a table
     tab2<- matrix(c(data[5], data[6], (data[5]+data[6]), data[7], data[8], (data[7]+data[8])), ncol=3, byrow = T) #create a matrix with the marginal values
     colnames(tab2) <- c('Exposure', 'No Exposure', "Total")
     rownames(tab2) <- c('Disease', 'PersonTime')
     tab.table2 <- as.table(tab2)#change the matrix as a table
     tab3<- matrix(c(data[9], data[10], (data[9]+data[10]), data[11], data[12], (data[11]+data[12])), ncol=3, byrow = T) #create a matrix with the marginal values
     colnames(tab3) <- c('Exposure', 'No Exposure', "Total")
     rownames(tab3) <- c('Disease', 'PersonTime')
     tab.table3 <- as.table(tab3)#change the matrix as a table
     tab.table<-list(stratification.level.1= tab.table1, stratification.level.2= tab.table2, stratification.level.3= tab.table3)
   } else {tab.table<-c("the data should contain 8 or 12 numbers because this function is designed for 2 or 3 stratified tables")}
  return(tab.table)} #this function is designed for 2 or 3 stratified tables


  
#function to calculate summary IRR using Mantel‐Haenszel weights
summary.irr<-function(data) #data should be a list of tables
  {
  if(length(data) == 2){
    tab1<-data$stratification.level.1 
    tab2<-data$stratification.level.2 #seperate tables for calculations
    w1<-tab1[1,2]*tab1[2,1]/tab1[2,3]
    w2<-tab2[1,2]*tab2[2,1]/tab2[2,3]
    irr1<-crude.Rate(tab1,measure = "IRR")
    irr2<-crude.Rate(tab2,measure = "IRR") #calculate crude IRR for each table
    summary.rate.ratio<-(w1*irr1+w2*irr2)/(w1+w2) #combine IRR from each table to get summary IRR
  }
  else if (length(data) == 3){
    tab1<-data$stratification.level.1 
    tab2<-data$stratification.level.2
    tab3<-data$stratification.level.3 #seperate tables for calculations
    w1<-tab1[1,2]*tab1[2,1]/tab1[2,3]
    w2<-tab2[1,2]*tab2[2,1]/tab2[2,3]
    w3<-tab3[1,2]*tab3[2,1]/tab3[2,3]
    irr1<-crude.Rate(tab1,measure = "IRR")
    irr2<-crude.Rate(tab2,measure = "IRR")
    irr3<-crude.Rate(tab3,measure = "IRR")#using the function created before to calculate crude IRR for each table
    summary.rate.ratio<-(w1*irr1+w2*irr2+w3*irr3)/(w1+w2+w3) #combine IRR from each table to get summary IRR
  }
  else {summary.rate.ratio<-c("this function is designed for 2 or 3 stratified tables to calculate MH weighted IRR")}
  return(summary.rate.ratio)
}

#function to calculate confidence interval for summary IRR
ci.summary.irr<-function(data,ci)  #data should be a list of tables
{
  alpha<-(100-95)/200
  z<-qnorm(alpha,lower.tail=FALSE)
  if(length(data) == 2){
    tab1<-data$stratification.level.1 
    tab2<-data$stratification.level.2 #seperate tables for calculations
    x<-log(summary.irr(data))
    varx<-(tab1[1,3]*tab1[2,1]*tab1[2,2]/(tab1[2,3]*tab1[2,3])+tab2[1,3]*tab2[2,1]*tab2[2,2]/(tab2[2,3]*tab2[2,3]))/((tab1[1,1]*tab1[2,2]/tab1[2,3]+tab2[1,1]*tab2[2,2]/tab2[2,3])*(tab1[1,2]*tab1[2,1]/tab1[2,3]+tab2[1,2]*tab2[2,1]/tab2[2,3]))
    lower.ci<-exp(x-z*sqrt(varx))
    upper.ci<-exp(x+z*sqrt(varx)) #calculate upper and lower confidence interval
    print(paste("IRR ", ci, "%CI: ", "(", round(lower.ci,2), " to ", round(upper.ci,2), ")", sep=""))
    }
  else if(length(data) == 3){
    tab1<-data$stratification.level.1 
    tab2<-data$stratification.level.2
    tab3<-data$stratification.level.3 #seperate tables for calculations
    x<-log(summary.irr(data))
    varx<-(tab1[1,3]*tab1[2,1]*tab1[2,2]/(tab1[2,3]*tab1[2,3])+tab2[1,3]*tab2[2,1]*tab2[2,2]/(tab2[2,3]*tab2[2,3])+tab3[1,3]*tab3[2,1]*tab3[2,2]/(tab3[2,3]*tab3[2,3]))/((tab1[1,1]*tab1[2,2]/tab1[2,3]+tab2[1,1]*tab2[2,2]/tab2[2,3]+tab3[1,1]*tab3[2,2]/tab3[2,3])*(tab1[1,2]*tab1[2,1]/tab1[2,3]+tab2[1,2]*tab2[2,1]/tab2[2,3]+tab3[1,2]*tab3[2,1]/tab3[2,3]))
    lower.ci<-exp(x-z*sqrt(varx))
    upper.ci<-exp(x+z*sqrt(varx)) #calculate upper and lower confidence interval
    print(paste("IRR ", ci, "%CI: ", "(", round(lower.ci,2), " to ", round(upper.ci,2), ")", sep=""))
    }
   else {print(c("this function is designed for 2 or 3 stratified tables to calculate summary IRR confidence interval"))}
}

#Hypothesis Test Chi Square test for stratified data:
stratified.test<-function(data) {
  if (length(data) == 2){
    tab1<-data$stratification.level.1 
    tab2<-data$stratification.level.2 #seperate tables for calculations
    x<-tab1[1,1]+tab2[1,1]
    ex<-tab1[1,3]*tab1[2,1]/tab1[2,3]+tab2[1,3]*tab2[2,1]/tab2[2,3]
    varx<-tab1[1,3]*tab1[2,1]*tab1[2,2]/(tab1[2,3]*tab1[2,3])+tab2[1,3]*tab2[2,1]*tab2[2,2]/(tab2[2,3]*tab2[2,3])
    zsquare<-(x-ex)*(x-ex)/varx #calculate the z-square in the hypothesis test
    pvalue<-pchisq(zsquare,1,lower.tail = F) #calculate the p-value in the hypothesis test
    cbind(zsquare,pvalue)
  }
  else if (length(data) == 3){
    tab1<-data$stratification.level.1 
    tab2<-data$stratification.level.2
    tab3<-data$stratification.level.3 #seperate tables for calculations
    x<-tab1[1,1]+tab2[1,1]+tab3[1,1]
    ex<-tab1[1,3]*tab1[2,1]/tab1[2,3]+tab2[1,3]*tab2[2,1]/tab2[2,3]+tab3[1,3]*tab3[2,1]/tab3[2,3]
    varx<-tab1[1,3]*tab1[2,1]*tab1[2,2]/(tab1[2,3]*tab1[2,3])+tab2[1,3]*tab2[2,1]*tab2[2,2]/(tab2[2,3]*tab2[2,3])+tab3[1,3]*tab3[2,1]*tab3[2,2]/(tab3[2,3]*tab3[2,3])
    zsquare<-(x-ex)*(x-ex)/varx #calculate the z-square in the hypothesis test
    pvalue<-pchisq(zsquare,1,lower.tail = F) #calculate the p-value in the hypothesis test
    cbind(zsquare,pvalue)
  }
    else{print(c("this function is designed for 2 or 3 stratified tables to do the Chi square test"))}
}
