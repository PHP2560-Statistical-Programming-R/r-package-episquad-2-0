\name{OR}
\alias{OR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Odds Ratio Calculation
}
\description{
Calculates the crude odds ratio with confidence intervals. The default confidence interval setting is 95 percent but can be altered as needed.
}
\usage{
OR(a, b, c, d, ci = 95)
}
% maybe also 'usage' for other objects documented here.
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
%% ~put references to the literature/web site here ~
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
OR( a=2, b= 45, c=77, d=789, ci= 75)}
