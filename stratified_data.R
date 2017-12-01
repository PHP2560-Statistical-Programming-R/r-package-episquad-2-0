#function to create a 2X2 table from a vector. Can be used for both crude data and stratified data

my.table<- function(data){
  if (length(data) == 4){ #make one table
    tab1<- matrix(c(data[1], data[2], (data[1]+data[2]), data[3], data[4], (data[3]+data[4]), (data[1]+data[3]),
                    (data[2]+data[4]), (data[1]+data[2]+data[3]+data[4])), ncol=3, byrow = T) #create a matrix with the marginal values
    #if you do not need the total column just omit the "+" values while creating the matrix
      colnames(tab1) <- c('Exposure', 'No Exposure', "Total")
      rownames(tab1) <- c('Disease', 'No Disease', "Total")
      tab.table <- as.table(tab1) #change the matrix as a table
  }  else if (length(data) == 8){ #make 2 tables
  tab1<- matrix(c(data[1], data[2], (data[1]+data[2]), data[3], data[4], (data[3]+data[4]), (data[1]+data[3]),
                    (data[2]+data[4]), (data[1]+data[2]+data[3]+data[4])), ncol=3, byrow = T) #create a matrix with the marginal values
  #if you do not need the total column just omit the "+" values while creating the matrix
    colnames(tab1) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab1) <- c('Disease', 'No Disease', "Total")
    tab.table1 <- as.table(tab1) #change the matrix as a table
  tab2<- matrix(c(data[5], data[6], (data[5]+data[6]), data[7], data[8], (data[7]+data[8]), (data[5]+data[7]), 
                  (data[6]+data[8]), (data[5]+data[6]+data[7]+data[8])), ncol=3, byrow = T) #create a matrix with the marginal values
  #if you do not need the total column just omit the "+" values while creating the matrix
    colnames(tab2) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab2) <- c('Disease', 'No Disease', "Total")
    tab.table2 <- as.table(tab2)#change the matrix as a table
  tab.table<-list(stratification.level.1= tab.table1, stratification.level.2= tab.table2) #put table1 and table 2 in a list
  }  else if (length(data) == 12){#make 3 tables
  tab1<- matrix(c(data[1], data[2], (data[1]+data[2]), data[3], data[4], (data[3]+data[4]), (data[1]+data[3]),
                  (data[2]+data[4]), (data[1]+data[2]+data[3]+data[4])), ncol=3, byrow = T) #create a matrix with the marginal values
  #if you do not need the total column just omit the "+" values while creating the matrix
    colnames(tab1) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab1) <- c('Disease', 'No Disease', "Total")
    tab.table1 <- as.table(tab1) #change the matrix as a table
  tab2<- matrix(c(data[5], data[6], (data[5]+data[6]), data[7], data[8], (data[7]+data[8]), (data[5]+data[7]), 
                  (data[6]+data[8]), (data[5]+data[6]+data[7]+data[8])), ncol=3, byrow = T) #create a matrix with the marginal values
  #if you do not need the total column just omit the "+" values while creating the matrix
    colnames(tab2) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab2) <- c('Disease', 'No Disease', "Total")
    tab.table2 <- as.table(tab2)#change the matrix as a table
  tab3<- matrix(c(data[9], data[10], (data[9]+data[10]), data[11], data[12], (data[11]+data[12]), (data[9]+data[11]), 
                  (data[10]+data[12]), (data[9]+data[10]+data[11]+data[12])), ncol=3, byrow = T) #create a matrix with the marginal values
  #if you do not need the total column just omit the "+" values while creating the matrix
    colnames(tab3) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab3) <- c('Disease', 'No Disease', "Total")
    tab.table3 <- as.table(tab3)#change the matrix as a table
  tab.table<-list(stratification.level.1= tab.table1, stratification.level.2= tab.table2,
                  stratification.level.3= tab.table3)
  }  else if (length(data) == 16){ #make 4 tables
  tab1<- matrix(c(data[1], data[2], (data[1]+data[2]), data[3], data[4], (data[3]+data[4]), (data[1]+data[3]),
                  (data[2]+data[4]), (data[1]+data[2]+data[3]+data[4])), ncol=3, byrow = T) #create a matrix with the marginal values
  #if you do not need the total column just omit the "+" values while creating the matrix
    colnames(tab1) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab1) <- c('Disease', 'No Disease', "Total")
    tab.table1 <- as.table(tab1) #change the matrix as a table
  tab2<- matrix(c(data[5], data[6], (data[5]+data[6]), data[7], data[8], (data[7]+data[8]), (data[5]+data[7]), 
                  (data[6]+data[8]), (data[5]+data[6]+data[7]+data[8])), ncol=3, byrow = T) #create a matrix with the marginal values
  #if you do not need the total column just omit the "+" values while creating the matrix
    colnames(tab2) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab2) <- c('Disease', 'No Disease', "Total")
    tab.table2 <- as.table(tab2)#change the matrix as a table
  tab3<- matrix(c(data[9], data[10], (data[9]+data[10]), data[11], data[12], (data[11]+data[12]), (data[9]+data[11]), 
                  (data[10]+data[12]), (data[9]+data[10]+data[11]+data[12])), ncol=3, byrow = T) #create a matrix with the marginal values
  #if you do not need the total column just omit the "+" values while creating the matrix
    colnames(tab3) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab3) <- c('Disease', 'No Disease', "Total")
    tab.table3 <- as.table(tab3)#change the matrix as a table
  tab4<- matrix(c(data[13], data[14], (data[13]+data[14]), data[15], data[16], (data[15]+data[16]), (data[13]+data[15]), 
                  (data[14]+data[16]), (data[13]+data[14]+data[15]+data[16])), ncol=3, byrow = T) #create a matrix with the marginal values
  #if you do not need the total column just omit the "+" values while creating the matrix
    colnames(tab4) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab4) <- c('Disease', 'No Disease', "Total")
    tab.table4 <- as.table(tab4)#change the matrix as a table
  tab.table<-list(stratification.level.1= tab.table1, stratification.level.2= tab.table2,
                  stratification.level.3= tab.table3, stratification.level.4= tab.table4)
  }  else { #make 5 tables
  tab1<- matrix(c(data[1], data[2], (data[1]+data[2]), data[3], data[4], (data[3]+data[4]), (data[1]+data[3]),
                  (data[2]+data[4]), (data[1]+data[2]+data[3]+data[4])), ncol=3, byrow = T) #create a matrix with the marginal values
  #if you do not need the total column just omit the "+" values while creating the matrix
    colnames(tab1) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab1) <- c('Disease', 'No Disease', "Total")
    tab.table1 <- as.table(tab1) #change the matrix as a table
  tab2<- matrix(c(data[5], data[6], (data[5]+data[6]), data[7], data[8], (data[7]+data[8]), (data[5]+data[7]), 
                  (data[6]+data[8]), (data[5]+data[6]+data[7]+data[8])), ncol=3, byrow = T) #create a matrix with the marginal values
  #if you do not need the total column just omit the "+" values while creating the matrix
    colnames(tab2) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab2) <- c('Disease', 'No Disease', "Total")
    tab.table2 <- as.table(tab2)#change the matrix as a table
  tab3<- matrix(c(data[9], data[10], (data[9]+data[10]), data[11], data[12], (data[11]+data[12]), (data[9]+data[11]), 
                  (data[10]+data[12]), (data[9]+data[10]+data[11]+data[12])), ncol=3, byrow = T) #create a matrix with the marginal values
  #if you do not need the total column just omit the "+" values while creating the matrix
    colnames(tab3) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab3) <- c('Disease', 'No Disease', "Total")
    tab.table3 <- as.table(tab3)#change the matrix as a table
  tab4<- matrix(c(data[13], data[14], (data[13]+data[14]), data[15], data[16], (data[15]+data[16]), (data[13]+data[15]), 
                  (data[14]+data[16]), (data[13]+data[14]+data[15]+data[16])), ncol=3, byrow = T) #create a matrix with the marginal values
  #if you do not need the total column just omit the "+" values while creating the matrix
    colnames(tab4) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab4) <- c('Disease', 'No Disease', "Total")
    tab.table4 <- as.table(tab4)#change the matrix as a table
  tab5<- matrix(c(data[17], data[18], (data[17]+data[18]), data[19], data[20], (data[19]+data[20]), (data[17]+data[19]), 
                  (data[18]+data[20]), (data[17]+data[18]+data[19]+data[20])), ncol=3, byrow = T) #create a matrix with the marginal values
  #if you do not need the total column just omit the "+" values while creating the matrix
    colnames(tab5) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab5) <- c('Disease', 'No Disease', "Total")
    tab.table5 <- as.table(tab5)#change the matrix as a table
  tab.table<-list(stratification.level.1= tab.table1, stratification.level.2= tab.table2,
                  stratification.level.3= tab.table3, stratification.level.4= tab.table4,
                  stratification.level.5= tab.table5)
  }
return(tab.table)
}

#suppose the a,b,c,d for BMI group 1 are 1,17,7,257 and 
# for BMI group 2 are 3,7,14,52
# for BMI group 3 are 9,15,30,107 and 
# for BMI group 4 are 14,5,44,27. Then you will enter the stratified data as the following vector:

test.data<- c(1,17,7,257,3,7,14,52,9,15,30,107,14,5,44,27) #test data from epi calculator
my.table(test.data)


#create function for summary RR using MH weight:
summary.RR<-function(data){
  dat<-my.table(data)#arrange data in tables to calculate the stratum specific estimates
  weight.strata<-c() #vector that will contain the weight of each stratum
  rr.strata <- c() #vector that will contain the RR of each stratum
  for (l in 1:length(dat)){ #this loop will calculate the RR and weight of each stratum 
    weight.strata[[l]] <- (dat[[l]][[4]]*dat[[l]][[3]]) / dat[[l]][[9]]
    rr.strata[[l]] <-  (dat[[l]][[1]]/dat[[l]][[3]]) / (dat[[l]][[4]]/dat[[l]][[6]])
  }
  weighted.rr <- weight.strata * rr.strata #multiply the RR of each stratum with the weight of each stratum
  rr <- sum(weighted.rr)/sum(weight.strata) 
  return(rr)
}


summary.RR(test.data)

#function for CI of summary RR using MH weight:
summary.CI.RR<-function(data, ci=95){
  alpha <- (100-ci)/100
  z <- 1-alpha/2
  x <- log(summary.RR(data))
  dat <- my.table(data) #arranging data in tables to calculate the stratum specific estimates
  upper <- c() #will contain the numerator of variance
  lowerA <- c() #will keep a part of denominator
  lowerB <- c() #will keep another part of denominator
  for (l in 1:length(dat)){ #this loop will calculate the stratum-specific estimates that we need for variance
    upper[[l]] <- ((dat[[l]][[7]]*dat[[l]][[3]]*dat[[l]][[6]]) - (dat[[l]][[1]]*dat[[l]][[4]]*dat[[l]][[9]])) / (dat[[l]][[9]]^2)
    lowerA[[l]] <-  ((dat[[l]][[1]]*dat[[l]][[6]])/dat[[l]][[9]]) 
    lowerB[[l]] <-  ((dat[[l]][[4]]*dat[[l]][[3]])/dat[[l]][[9]])
  }
  var.x <- sum(upper) / (sum(lowerA) * sum(lowerB)) #final weighted variance
  upper.ci<- exp(x + (qnorm(z) * sqrt(var.x)))
  lower.ci<- exp(x - (qnorm(z) * sqrt(var.x)))
  print(paste(ci, "% CI: ", "(", round(lower.ci,2), " to ", round(upper.ci,2), ")", sep="")) 
}

summary.CI.RR(test.data)


#function for summary hypothesis testing:
summary.Test<-function(data){
  dat<-my.table(data) # my.table function will calculate the margianl totals
  x <- c() #will contain the numerator of variance
  expected.x <- c() #will keep a part of denominator
  var.x <- c() #will keep another part of denominator
  for (l in 1:length(dat)){ #this loop will calculate the stratum-specific estimates that we need for variance
    x[[l]] <- dat[[l]][[1]]
    expected.x[[l]] <-  ((dat[[l]][[3]]*dat[[l]][[7]])/dat[[l]][[9]]) 
    var.x[[l]] <-  (dat[[l]][[7]]*dat[[l]][[8]]*dat[[l]][[3]]*dat[[l]][[6]]) / (dat[[l]][[9]]^3)
  }
  z.square<-round(((sum(x)- sum(expected.x))^2 / sum(var.x)), 3)
  p.value<-round((pchisq(z.square, df= 1, lower.tail = F)), 5)
  output<-cbind(z.square, p.value)
  return(output)
}

summary.Test(test.data)

