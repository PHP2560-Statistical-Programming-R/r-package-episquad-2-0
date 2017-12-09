\name{risk}
\alias{risk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Risk Calculation
}
\description{
This function takes a vector of data and calculates risk ratio or risk difference along with confidence interval and result of hypothesis testing for crude and stratified data. Data will be weighted using Mantel‐Haenszel weights for risk ratio and Inverse variance weights for risk difference.
}
\usage{
risk (data, measure = c("RR", "RD"), ci = 95, estimate = c("crude", "summary"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a vector of crude or stratified data
}
\item{measure}{
the measure to be used. Measures could be RR (risk ratio) or RD (risk difference)
}
\item{ci}{
an object which indicates the percentage of confidence interval, default is set to 95
}
\item{estimate}{
a vector indicating crude or summary estimate
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Rothman KJ, Greenland S (1998). Modern Epidemiology. Lippincott Williams, & Wilkins, Philadelphia, pp. 271.
}
\author{
Sadia Sharmin
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
crude.data <- c (27,44,95,443)
risk (crude.data, measure = "RR", ci = 95, estimate = "crude")

strat.data <- c (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
risk (strat.data, measure = "RD", ci = 95, estimate = "summary")}



