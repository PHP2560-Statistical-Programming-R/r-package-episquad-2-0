\name{stratified.table}
\alias{stratified.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Table for Stratified Rate Data
}
\description{
This function takes raw numbers and makes 2 by 2 tables for further analysis, this function is designed for stratified rate data analysis, the number of stratified tables is limited to 2 to 5.
}
\usage{
stratified.table(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data should be a set of numbers, the amount of numbers in the set is limited to 8 to 20 as the number of stratified tables is limited to 2 to 5.
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
data1<-c(41,15,28010,19017,23,42,14679,45363)
stratified.table(data1)}
