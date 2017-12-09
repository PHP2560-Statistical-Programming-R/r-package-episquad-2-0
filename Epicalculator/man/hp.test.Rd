\name{hp.test}
\alias{hp.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hypothesis Testing
}
\description{
This function takes a vector of data and calculates the results of hypothesis testing by Pearson's Chi-square test for both crude and stratified data.
}
\usage{
hp.test(data, ci = 95, estimate = c("crude", "summary"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a vector of crude or stratified data
}
\item{ci}{
an object which indicates the percentage of confidence interval, default is set to 95
}
\item{estimate}{
a vector indicating crude or summary estimate
}
}\details{
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
Kenneth J. Rothman and Sander Greenland (1998), Modern Epidemiology, Lippincott-Raven Publishers
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
hp.test (crude.data, ci = 95, estimate = "crude")

strat.data <- c (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
hp.test (strat.data, ci = 95, estimate = "summary")}
