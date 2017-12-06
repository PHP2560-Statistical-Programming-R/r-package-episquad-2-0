\name{AR}
\alias{AR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AR Function
}
\description{
Calculates the attributable risk (risk difference) and the percent attributable risk with confidence intervals. The variance is calculated based on Greenwood’s formula. The default confidence interval setting is at 95% but can be altered as needed.
}
\usage{
AR (a , b , c , d, ci= 95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
represents the number of exposed and diseased. Will not accept a value less than 0.
}
\item{b}{
represents the number of non-exposed but diseased. Will not accept a value less than 0.
}
\item{c}{
represents the number of exposed but not diseased. Will not accept a value less than 0.
}
\item{d}{
represents the number of non-exposed and not diseased. Will not accept a value less than 0.
}
\item{ci}{
abbreviation for confidence interval. It should be a number between 1 to 100. The default setting is 95. The confidence interval will not accept a value of 0.
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
Szklo M, Nieto J (2006). Epidemiology: Beyond the Basics 2nd Edition. Jones & Bartlett Learning, Burlington, MA, Appendix A.
Moyses Szklo, Javier Nieto

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
AR ( a = 2, b = 45, c = 77, d = 789, ci = 75)
