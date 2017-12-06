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




#function to calculate crude rate ratio and rate difference
crude.Rate<-function(table, measure=c("IRR", "IRD")){ #will calculate rate ratio and rate difference
  if (measure == "IRR") {
    irr<-(table[1,1]/table[2,1]) / (table[1,2]/table[2,2]) #irr is the rate ratio
    return(round(irr,5))}
  else {ird<-(table[1,1]/table[2,1]) - (table[1,2]/table[2,2]) #ird is risk difference
  return(round(ird,5))}
}


#function for confidence interval of rate ratio or rate difference
rate.ci<-function(table, ci, measure=c("IRR","IRD")) {#will calculate rate ratio and rate difference confidence interval
  alpha<-(100-ci)/200
  z<-qnorm(alpha,lower.tail=FALSE)
  if (measure == "IRR") {
  x<-log((table[1,1]/table[2,1])/(table[1,2]/table[2,2]))
  varx<-sqrt(1/table[1,1]+1/table[1,2])
  lower.ci<-exp(x-z*varx)
  upper.ci<-exp(x+z*varx)#calculate upper and lower confidence interval
  print(paste("IRD ", ci, "%CI: ", "(", round(lower.ci,5), " to ", round(upper.ci,5), ")", sep=""))
}
else {x<-table[1,1]/table[2,1]-table[1,2]/table[2,2]
      varx<-table[1,1]/(table[2,1]*table[2,1])+table[1,2]/(table[2,2]*table[2,2])
      lower.ci<-x-z*sqrt(varx)
      upper.ci<-x+z*sqrt(varx)#calculate upper and lower confidence interval
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
   } 
  else if(length(data) == 16){
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
    tab4<- matrix(c(data[13], data[14], (data[13]+data[14]), data[15], data[16], (data[15]+data[16])), ncol=3, byrow = T) #create a matrix with the marginal values
    colnames(tab4) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab4) <- c('Disease', 'PersonTime')
    tab.table4 <- as.table(tab4)#change the matrix as a table
    tab.table<-list(stratification.level.1= tab.table1, stratification.level.2= tab.table2, stratification.level.3= tab.table3, stratification.level.4= tab.table4)
  } 
  else if(length(data) == 20){
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
    tab4<- matrix(c(data[13], data[14], (data[13]+data[14]), data[15], data[16], (data[15]+data[16])), ncol=3, byrow = T) #create a matrix with the marginal values
    colnames(tab4) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab4) <- c('Disease', 'PersonTime')
    tab.table4 <- as.table(tab4)#change the matrix as a table
    tab5<- matrix(c(data[17], data[18], (data[17]+data[18]), data[19], data[20], (data[19]+data[20])), ncol=3, byrow = T) #create a matrix with the marginal values
    colnames(tab5) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab5) <- c('Disease', 'PersonTime')
    tab.table5 <- as.table(tab5)#change the matrix as a table
    tab.table<-list(stratification.level.1= tab.table1, stratification.level.2= tab.table2, stratification.level.3= tab.table3, stratification.level.4= tab.table4, stratification.level.5= tab.table5)
  } 
  else {tab.table<-c("this function is designed for 2 to 5 stratified tables")}
  return(tab.table)} #this function is designed for 2 or 5 stratified tables


  
#function to calculate summary IRR using Mantel‐Haenszel weights
summary.irr<-function(data) #data should be a list of tables
  {
  weight.strata<-c() #vector that will contain the weight of each stratum
  irr.strata <- c() #vector that will contain the RR of each stratum
  if (length(data) %in% c(2,3,4,5)){
  for (l in 1:length(data)){ #this loop will calculate the RR and weight of each stratum 
    weight.strata[[l]]<-data[[l]][1,2]*data[[l]][2,1]/data[[l]][2,3]
    irr.strata[[l]]<-crude.Rate(data[[l]], measure = "IRR")
    summary.rate.ratio<-sum(weight.strata*irr.strata)/sum(weight.strata)}
    }
 
  else {summary.rate.ratio<-c("this function is designed for 2 to 5 stratified tables to calculate MH weighted IRR")}
  return(summary.rate.ratio)
}

#function to calculate confidence interval for summary IRR
 ci.summary.irr<-function(data,ci)  #data should be a list of tables
   {
       alpha<-(100-ci)/200
       z<-qnorm(alpha,lower.tail=FALSE)
       if(length(data) %in% c(2,3,4,5)){
           upper <- c() #will contain the numerator of variance
           lowerA <- c() #will keep a part of denominator
           lowerB <- c() #will keep another part of denominator
           for (l in 1:length(data)){ #this loop will calculate the stratum-specific estimates that we need for variance
             upper[[l]]<-data[[l]][1,3]*data[[l]][2,1]*data[[l]][2,2]/(data[[l]][2,3]*data[[l]][2,3])
             lowerA[[l]]<-data[[l]][1,1]*data[[l]][2,2]/data[[l]][2,3]
             lowerB[[l]]<-data[[l]][1,2]*data[[l]][2,1]/data[[l]][2,3]
             x<-log(summary.irr(data))
             varx<-sum(upper)/(sum(lowerA)*sum(lowerB))
             lower.ci<-exp(x-z*sqrt(varx))
             upper.ci<-exp(x+z*sqrt(varx)) #calculate upper and lower confidence interval
             ci<-paste(round(lower.ci,2),round(upper.ci,2))
                           }
                       }
              else {ci<-c("this function is designed for 2 to 5 stratified tables to calculate summary IRR confidence interval")}
             return(ci)}
 
#Hypothesis Test Chi Square test for stratified data:
 stratified.test<-function(data) {
      if(length(data) %in% c(2,3,4,5)){
        x<-c()
        ex<-c()
        varx<-c()
          for (l in 1:length(data)) {
            x[[l]]<-data[[l]][1,1]
            ex[[l]]<-data[[l]][1,3]*data[[l]][2,1]/data[[l]][2,3]
            varx[[l]]<-data[[l]][1,3]*data[[l]][2,1]*data[[l]][2,2]/(data[[l]][2,3]*data[[l]][2,3])
            zsquare<-(sum(x)-sum(ex))^2/sum(varx) #calculate the z-square in the hypothesis test
            pvalue<-pchisq(zsquare,1,lower.tail = F) #calculate the p-value in the hypothesis test
            output<-cbind(zsquare,pvalue)}
         }
       else{output<-c("this function is designed for 2 to 5 stratified tables to do the Chi square test")}
   return(output)
 }
 
 
 
 