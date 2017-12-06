\name{my.table}
\alias{my.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creating Contigency Table
}
\description{
This function takes a vector of data and creates one to five 2 X 2 contingency tables of exposure and disease depending on the length of the vector
}
\usage{
my.table (data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a vector of data that can be used to create 2 X 2 tables
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
crude.data <- c (1,2,3,4)
my.table (crude.data)
strat.data <- c (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
my.table (strat.data)


