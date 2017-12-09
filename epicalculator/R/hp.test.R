#Function for hypothesis (HP) testing of crude and summary data:

hp.test <- function(data, ci=95, estimate = c("crude", "summary")){
  d <- my.table(data) # convert the input data vector to 2X2 table
  z <-1-((100-ci)/100)/2

  if (estimate == "crude") { #do HP test for crude data
    if (length(d) > 1)
      stop("can not have more than 4 values for 'crude data'")
    a <- d[[1]][1]
    b <- d[[1]][3]
    c <- d[[1]][2]
    d1 <- d[[1]][4]
    if (any(a <0 | b <0 | c <0 | d1 < 0))
      stop("all entries of 'data' must be nonnegative and finite")
    m1 <- a + b
    m0 <- c + d1
    n1 <- a + c
    n0 <- b + d1
    t <- a + b + c + d1
    df <- (1- dim(d[[1]])[1]) * (1-dim(d[[1]])[2])
    x.t <- a
    ex.t <- (m1*n1) / t
    if (any(ex.t < 5))  warning("Chi-squared approximation may be incorrect")
    var.x.t <- (m1*m0*n1*n0) / t^3
    z.sq <- round(((x.t-ex.t)^2 / var.x.t), 2)

  } else {  #do summary hypothesis testing:
    x.t <- c() #will contain the numerator of variance
    ex.x.t <- c() #will keep a part of denominator
    var.x.t <- c() #will keep another part of denominator
    for (i in 1:length(d)){ #this loop will calculate the RR and weight of each stratum
      a <- d[[i]][1]
      b <- d[[i]][3]
      c <- d[[i]][2]
      d1 <- d[[i]][4]
      m1 <- a + b
      m0 <- c + d1
      n1 <- a + c
      n0 <- b + d1
      t <- a + b + c + d1
      df <- (1-dim(d[[1]])[1]) * (1-dim(d[[1]])[2])
      x.t [[i]] <- a
      ex.x.t[[i]] <-  ((m1*n1)/t)
      var.x.t[[i]] <-  (m1*m0*n1*n0) / (t^3)
    }
    z.sq <-round(((sum(x.t)- sum(ex.x.t))^2 / sum(var.x.t)), 2)
  }
  p <- round((pchisq(z.sq, df= 1, lower.tail = F)), 5)
  m <- "Pearson's Chi-squared test"
  test <-cbind(method = m,
              z.square = z.sq,
              df,
              p.value = p) #contains method, test statistic, df and p
  return(test)
}





