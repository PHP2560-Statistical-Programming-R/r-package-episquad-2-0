# risk function to use with crude or stratified data
# will show the estimate (RR/RD), CI and hypothesis test results

risk <- function (data, measure = c("RR", "RD"),
                  ci=95, estimate = c("crude", "summary")){

  d <- my.table(data) # convert the input data vector to 2X2 table
  dt <- paste(deparse(data)) #to show data in vector format in the output
  z <-1-((100-ci)/100)/2


  if (estimate == "crude"){ #will calculate the crude estimate with CI and hypothesis test
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

    if (measure == "RR"){ #calculate the RR and CI for crude data
      r <- (a / n1) / (b / n0) #RR
      x <- log(r) #x is the log of RR
      var.x <-(c / (a*n1)) + (d1 /(b*n0))
      upper.ci <- exp(x + (qnorm(z) * sqrt(var.x)))
      lower.ci <- exp(x - (qnorm(z) * sqrt(var.x)))

    } else { #calculate the RD and CI
      r <-(a / n1) - (b / n0) #RD
      var.x <-((a*c) / n1^3) + ((b*d1) / n0^3)
      upper.ci <- r + (qnorm(z) * sqrt(var.x))
      lower.ci <- r - (qnorm(z) * sqrt(var.x))
    }

    #hypothesis test of crude data for both RR and RD:
    x.t <- a
    ex.t <- (m1*n1) / t
    if (any(ex.t < 5))  warning("Chi-squared approximation may be incorrect")
    var.x.t <- (m1*m0*n1*n0) / t^3
    z.sq <- round(((x.t-ex.t)^2 / var.x.t), 2)
    df <- (1- dim(d[[1]])[1]) * (1-dim(d[[1]])[2])


  } else { #will calculate the summary estimate, CI and hypothesis test for stratified data

    wt.st <-c()  #will contain the weight of each stratum
    r.st <- c() #will contain the risk (RR/RD) of each stratum
    u <- c()    #will contain the numerator of variance
    l.A <- c()  #will keep a part of denominator
    l.B <- c()  #will keep another part of denominator

    for (i in 1:length(d)){ #this loop will calculate the RR and weight of each stratum
      a <- d[[i]][1]
      b <- d[[i]][3]
      c <- d[[i]][2]
      d1 <- d[[i]][4]
      if (any(a <0 | b <0 | c <0 | d1 < 0))
        stop("all entries of 'data' must be nonnegative and finite")

      m1 <- a + b
      m0 <- c + d1
      n1 <- a + c
      n0 <- b + d1
      t <- a + b + c + d1

      if (measure == "RR") { #calculate summary RR
        wt.st[[i]] <- (b*n1 / t)
        r.st[[i]] <-  (a/n1) / (b/n0)
        wt.r <- wt.st * r.st #multiply the RR of each stratum with the weight of each stratum
        r <- sum(wt.r)/sum(wt.st) # summary RR
        #calculations for ci:
        x <- log(r)
        u[[i]] <- ((m1*n1*n0) - (a*b*t)) / (t^2)
        l.A[[i]] <-  ((a*n0)/t)
        l.B[[i]] <-  ((b*n1)/t)
        var.x <- sum(u) / (sum(l.A) * sum(l.B)) #final weighted variance
        upper.ci <- exp(x + (qnorm(z) * sqrt(var.x)))
        lower.ci <- exp(x - (qnorm(z) * sqrt(var.x)))


      } else { #calculate summary RD
        wt.st[[i]] <- (n1^3 * n0^3) / ((a*c*n0^3 ) + (b* d1*n1^3))
        r.st[[i]] <-  (a/n1) - (b/n0)
        wt.r <- wt.st * r.st #multiply the RR of each stratum with the weight of each stratum
        r <- sum(wt.r)/sum(wt.st) # summary RD
        x <- r
        var.x <- 1 / sum(wt.st)
        upper.ci <- x + (qnorm(z) * sqrt(var.x))
        lower.ci <- x - (qnorm(z) * sqrt(var.x))
      }
    }
#summary hypothesis testing (HP test) for RR and RD:
    x.t <- c() #will contain the numerator of variance
    ex.x.t <- c() #will keep a part of denominator
    var.x.t <- c() #will keep another part of denominator
    df <- (1-dim(d[[1]])[1]) * (1-dim(d[[1]])[2])

for (i in 1:length(d)){ #this loop will calculate the stratum-specific estimates for HP test
      a <- d[[i]][1]
      b <- d[[i]][3]
      c <- d[[i]][2]
      d1 <- d[[i]][4]
      if (any(a <0 | b <0 | c <0 | d1 < 0))
        stop("all entries of 'data' must be nonnegative and finite")
      m1 <- a + b
      m0 <- c + d1
      n1 <- a + c
      n0 <- b + d1
      t <- a + b + c + d1
      x.t [[i]] <- a
      ex.x.t[[i]] <-  ((m1*n1)/t)
      var.x.t[[i]] <-  (m1*m0*n1*n0) / (t^3)
    }

    if (any(ex.x.t < 5))  warning("Chi-squared approximation may be incorrect")
    z.sq <-round(((sum(x.t)- sum(ex.x.t))^2 / sum(var.x.t)), 2)
  }

  #saving the results for final output
  risk <-rbind(Data=dt, round(r,4)) #risk contains data and RR/RD
  rownames(risk)[2] <- paste(estimate, measure, sep = " ") #paste RR or RD as the rowname
  ci <-cbind(round(lower.ci,5), round(upper.ci,5)) # contains upper and lower confidence interval
  p <- round((pchisq(z.sq, df= 1, lower.tail = F)), 5)
  m <- "Pearson's Chi-squared test"
  test <-cbind(method = m,
              z.square = z.sq,
              df,
              p.value = p) #contains method, test statistic, df and p value

  structure(list(risk,
                 Confidence.interval = ci,
                 Hypothesis.test = test)) # final output
}





