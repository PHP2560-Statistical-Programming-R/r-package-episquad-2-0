#input data is in a 2X2 table format where a represent exposed and diseased, b represent not exposed but diseased, c represent exposed person-time and d represent not exposed person-time
crude.table<- function(a,b,c,d){if (a<0 | b<0 | c<0| d<0){
  print("Warning: cannot have negative value")
}else{
  m1<-a+b #total diseased
  t<-c+d #total person-time
  tab<- matrix(c(a, b, m1, c, d, t), ncol=3, byrow = T) #create a matrix with the marginal values
  colnames(tab) <- c('Exposure', 'No Exposure', "Total")
  rownames(tab) <- c('Disease', 'PersonTime')
  tab.table <- as.table(tab)
  return(tab.table)}
}
