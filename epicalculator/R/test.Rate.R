#Hypothesis Test Chi Square test for crude data:

test.Rate<-function(table){
  ex<-table[1,3]*table[2,1]/table[2,3]
  varx<-table[1,3]*table[2,1]*table[2,2]/(table[2,3]*table[2,3])
  zsquare<-(table[1,1]-ex)*(table[1,1]-ex)/varx
  pvalue<-pchisq(zsquare,1,lower.tail = F)
  cbind(zsquare,pvalue)
}

