\name{summary.rate}
\alias{summary.rate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary Rate  Calculation
}
\description{
This function calculate summary rate ratio using Mantel‐Haenszel weights for stratified data and summary rate difference using Inverse variance weights
}
\usage{
summary.rate(data,ci,measure=c("IRR","IRD"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data should be a list of tables, it could be created by using stratified.table()
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
data1<-stratified.table(c(41,15,28010,19017,23,42,14679,45363))
summary.irr(data1),95}
