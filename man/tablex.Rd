\name{tablex}
\alias{tablex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Contingency Table
}
\description{
This function takes raw numbers and makes a three-by-three table for further analysis. The third row and columns are sums of the traditional two-by-two table. The tablex function is required for Arributable Risk (AR), Attributable Risk Percent (ARpercent), Population Arributable Risk (PAR), Population Attributable Risk Percent (PARpercent) analysis.
}
\usage{
tablex(a,b,c,d)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{a}{Represents the number of exposed and diseased. It will not accept a value less than 0.
%%     ~~Describe \code{a} here~~
}
  \item{b}{Represents the number of non-exposed but diseased. It will not accept a value less than 0.
%%     ~~Describe \code{b} here~~
}
  \item{c}{Represents the number of exposed but not diseased. It will not accept a value less than 0.
%%     ~~Describe \code{c} here~~
}
  \item{d}{Represents the number of non-exposed and not diseased. It will not accept a value less than 0.
%%     ~~Describe \code{d} here~~
}
}
\details{
The tablex function is required for Arributable Risk (AR), Attributable Risk Percent (ARpercent), Population Arributable Risk (PAR), Population Attributable Risk Percent (PARpercent) analysis.
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

Szklo M, Nieto J (2006). Epidemiology: Beyond the Basics 2nd Edition. Jones & Bartlett Learning, Burlington, MA, Appendix A.
}
\author{
Catrina Mueller-Leonhard
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#creates a three-by-three matrix from a vector
tablex(12,23,13,41)
}
