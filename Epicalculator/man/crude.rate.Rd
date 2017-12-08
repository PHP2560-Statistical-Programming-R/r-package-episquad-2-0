\name{crude.rate}
\alias{crude.rate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Crude Rate Calculation
}
\description{
This function takes 2 by 2 table and calculate crude rate ratio and rate difference
}
\usage{
crude.rate(table, measure)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{
2 by 2 table can be created using crude.table function
}
 \item{measure}{
indicate you want to calculate either rate ratio or rate difference
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
%% ~put references to the literature/web site here ~
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
crude.Rate (table1, measure=”IRR)
crude.Rate (table1, measure=”IRD)}
