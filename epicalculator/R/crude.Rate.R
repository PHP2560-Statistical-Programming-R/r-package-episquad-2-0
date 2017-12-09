#function to calculate crude rate ratio and rate difference
crude.Rate<-function(table, measure=c("IRR", "IRD")){ #will calculate rate ratio and rate difference
  if (measure == "IRR") {
    irr<-(table[1,1]/table[2,1]) / (table[1,2]/table[2,2]) #irr is the rate ratio
    return(round(irr,5))}
  else {ird<-(table[1,1]/table[2,1]) - (table[1,2]/table[2,2]) #ird is risk difference
  return(round(ird,5))}
}

