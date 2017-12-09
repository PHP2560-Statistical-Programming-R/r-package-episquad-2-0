\name{my.table}
\alias{my.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Contingency Table
}
\description{
This function takes a vector of data and creates one to five 2 X 2 tables of exposure and disease depending on the length of the vector.(this function is designed for risk() function in the package)
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
my.table (crude.data)
strat.data <- c (1,17,7,257,3,7,14,52,9,15,30,107,14,5,44,27)
my.table (strat.data)
}


