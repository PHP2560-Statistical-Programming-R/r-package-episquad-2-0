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
  else {stop("this function is designed for 2 to 5 stratified tables")}
  return(tab.table)} #this function is designed for 2 or 5 stratified tables



