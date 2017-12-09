\name{crude.rate}
\alias{crude.rate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Crude Rate Calculation
}
\description{
This function takes 2 by 2 table and calculate crude rate ratio and rate difference with confidence interval
}
\usage{
crude.rate(table, ci, measure)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{
2 by 2 table can be created using crude.table function
}
  \item{ci}{
indicate confidence interval
}
 \item{measure}{
indicate you want to calculate either rate ratio or rate difference
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
Kenneth J. Rothman and Sander Greenland (1998), Modern Epidemiology, Lippincott-Raven Publishers
}
\author{
Ze Zhang
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
table1 <- crude.table(41,15,28010,19017)
crude.Rate (table1, 95,measure="IRR")
crude.Rate (table1, 90,measure="IRD")}
