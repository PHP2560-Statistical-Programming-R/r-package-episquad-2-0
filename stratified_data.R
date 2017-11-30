#function to create a 2X2 table from a vector. Can be used for both crude data and stratified data

my.table<- function(data){
  if (length(data) == 4){
    tab1<- matrix(c(data[1], data[2], (data[1]+data[2]), data[3], data[4], (data[3]+data[4]), (data[1]+data[3]),
                    (data[2]+data[4]), (data[1]+data[2]+data[3]+data[4])), ncol=3, byrow = T) #create a matrix with the marginal values
    #if you do not need the total column just omit the "+" values while creating the matrix
    colnames(tab1) <- c('Exposure', 'No Exposure', "Total")
    rownames(tab1) <- c('Disease', 'No Disease', "Total")
    tab.table <- as.table(tab1) #change the matrix as a table
  }  else {
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
  tab.table<-list(stratification.level.1= tab.table1, stratification.level.2= tab.table2)} #put table1 and table 2 in a list
  return(tab.table)
}

#suppose the a,b,c,d for males are 1,2,3,4 and for females are 5,6,7,8. Then you will enter the stratified data as the following vector:
strat.dat <- c(1,2,3,4,5,6,7,8)
my.table(strat.dat)

