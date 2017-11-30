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

#suppose the a,b,c,d for males are 1,2,3,4 and for females are 5,6,7,8. Then you will enter the stratified data as the following vector:
strat.dat <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17, 18, 19, 20)
my.table(strat.dat)


#create function for stratified RR:
stratRR<-function(data){
  dat<-my.table(data)
  weight.strata<-c() #vector that will contain the weight of each stratum
  rr.strata <- c() #vector that will contain the RR of each stratum
  for (l in 1:length(dat)){ #this loop will calculate the RR and weight of each stratum 
    weight.strata[[l]] <- (dat[[l]][[4]]*dat[[l]][[3]]) / dat[[l]][[9]]
    rr.strata[[l]] <-  (dat[[l]][[1]]/dat[[l]][[3]]) / (dat[[l]][[4]]/dat[[l]][[6]])
  }
  weighted.rr <- weight.strata * rr.strata
  rr <- sum(weighted.rr)/sum(weight.strata) 
  return(rr)
}

test.data<-c(150,50,20,30,50,10,80,70) #test data from HW4 of PHP 2200
stratRR(test.data)
