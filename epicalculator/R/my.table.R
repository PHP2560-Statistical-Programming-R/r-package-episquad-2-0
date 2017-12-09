#function to create contingency tables
#can create upto five 2X2 tables depending on the length of input vector

my.table <- function (x) {
  l <- length (x)
  n <- c("Disease", "Exposure") #row and column headings
  y <- list(c("Yes","No"), c("Yes","No")) #row and column subheadings of table

 if (l == 4){ #create an array of 1 table (array contains the tables like a list)
    a <- aperm(array(x[c(1:4)], c(2,2,1)), perm=c(2,1,3))
    dimnames(a) <- y #assinging subheadings
    names(dimnames(a)) <- n #assigning headings
    tab<-list(factor.1=a[,,1]) #final list of tables

  } else if (l>4 & l<=8){ #create an array of 2 tables
    a<-  aperm(array(x[c(1:8)], dim=c(2,2,2)), perm=c(2,1,3))
    dimnames(a) <- y
    names(dimnames(a)) <- n
    tab<-list(factor.1=a[,,1], factor.2=a[,,2])#final list of tables

  } else if (l>8 & l<=12){ #create an array of 3 tables
    a<- aperm(array(x[c(1:12)], c(2,2,3)),perm=c(2,1,3))
    dimnames(a) <- y
    names(dimnames(a)) <- n
    tab<-list(factor.1=a[,,1], factor.2=a[,,2], factor.3=a[,,3])#final list of tables

  }  else if (l>12 & l<=16){ #create an array of 4 tables
    a<- aperm(array(x[c(1:16)], c(2,2,4)),perm=c(2,1,3))
    dimnames(a) <- y
    names(dimnames(a)) <- n
    tab<-list(factor.1=a[,,1], factor.2=a[,,2], factor.3=a[,,3], factor.4=a[,,4])#final list of tables

  }  else { #create an array of 5 table
    a<- aperm(array(x[c(1:20)], c(2,2,5)),perm=c(2,1,3))
    dimnames(a) <- y
    names(dimnames(a)) <- n
    tab<-list(factor.1=a[,,1], factor.2=a[,,2], factor.3=a[,,3], factor.4=a[,,4], factor.5=a[,,5])#final list of tables
  }
  return(tab)
}
