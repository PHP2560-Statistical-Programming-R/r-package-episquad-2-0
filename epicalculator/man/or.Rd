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
OR(table, ci=95)
}
% maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{requires the table function which creates a matrix where 'a'
represents the number of exposed and diseased, 'b' represents the number of non-exposed but diseased, 'c' represents the number of exposed but not diseased, and 'd' represents the number of non-exposed and not diseased. The table function also calulates row and column totals The table function will not accept a value less than  0.
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
# creates a matrix table from a vector
table1<- tablex(23,45,60,789)

# using the table created above, calculates the Odds Ratio
OR(table1, ci= 75)}
