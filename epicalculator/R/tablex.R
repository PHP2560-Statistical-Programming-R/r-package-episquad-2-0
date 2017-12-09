#Basic Input Information

#'a' = exposed and diseased
#'b' = non-exposed but diseased
#'c' = exposed but not diseased
#'d' = non-exposed and not diseased

tablex<- function(a,b,c,d){
  n1<-a+c # total exposed
  n0<-b+d # total non-exposed
  m1<-a+b # total diseased
  m0<-c+d # total non-diseased
  N<- a+b+c+d # total
  tab<- matrix(c(a, b, m1, c, d, m0, n1, n0, N), ncol=3, byrow = T) #create a matrix with the marginal values
  colnames(tab) <- c('Exposure', 'No Exposure', 'Total')
  rownames(tab) <- c('Disease', 'No Disease', 'Total')
  tab.table <- as.table(tab)
  return(tab.table)
  if (a<0 | b<0 | c<0| d<0){
    print("Warning: cannot have negative value")
  }
}
