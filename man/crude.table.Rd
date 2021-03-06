\name{crude.table}
\alias{crude.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Contingency Table for Rate Data
}
\description{
This function takes raw numbers and makes a 2 by 2 table for further analysis, this function is designed for crude rate data analysis
}
\usage{
crude.table(a,b,c,d)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
number of exposed and diseased people
}
\item{b}{
number of unexposed and diseased people
}
\item{c}{
exposed person-time
}
\item{d}{
unexposed person-time
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
crude.table(41,15,28010,19017)}
