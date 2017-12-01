my.table <- function (data) {
  if (length(data)==4){
    a<- aperm(array(data[1]:data[4], c(2,2,1)),perm=c(2,1,3)) #creates an array of 1 table 
    dimnames(a) <- list(c("Yes","No"), c("Yes","No"))
    names(dimnames(a)) <- c("Disease", "Exposure")
    tab.table<-list(factor.1=a[,,1]) #final list of tables
  } else if (length(data)==8){
    a<-  aperm(array(data[1]:data[8], dim=c(2,2,2)), perm=c(2,1,3))#creates an array of 2 tables
    dimnames(a) <- list(c("Yes","No"), c("Yes","No"))
    names(dimnames(a)) <- c("Disease", "Exposure")
    tab.table<-list(factor.1=a[,,1], factor.2=a[,,2])#final list of tables
  } else if (length(data)==12){
    a<- aperm(array(data[1]:data[12], c(2,2,3)),perm=c(2,1,3))#creates an array of 3 tables
    dimnames(a) <- list(c("Yes","No"), c("Yes","No"))
    names(dimnames(a)) <- c("Disease", "Exposure")
    tab.table<-list(factor.1=a[,,1], factor.2=a[,,2], factor.3=a[,,3])#final list of tables
  }  else if (length(data)==16){
    a<- aperm(array(data[1]:data[16], c(2,2,4)),perm=c(2,1,3))#creates an array of 4 tables
    dimnames(a) <- list(c("Yes","No"), c("Yes","No"))
    names(dimnames(a)) <- c("Disease", "Exposure")
    tab.table<-list(factor.1=a[,,1], factor.2=a[,,2], factor.3=a[,,3], factor.4=a[,,4])#final list of tables
  }  else {
    a<- aperm(array(data[1]:data[20], c(2,2,5)),perm=c(2,1,3))#creates an array of 5 table
    dimnames(a) <- list(c("Yes","No"), c("Yes","No"))
    names(dimnames(a)) <- c("Disease", "Exposure")
    tab.table<-list(factor.1=a[,,1], factor.2=a[,,2], factor.3=a[,,3], factor.4=a[,,4], factor.5=a[,,5])#final list of tables
  }
  return(tab.table)
}

test.data<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
z<-my.table(test.data)
str(z)

#indexing the list element (a=1, b=3, c=2, d=4 in each table)
#a in table 1:
z[[1]][1]
#b in table 1:
z[[1]][3]
#c in table 1:
z[[1]][2]
#c in table 1:
z[[1]][4]


