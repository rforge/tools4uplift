\name{SplitUplift}
\alias{SplitUplift}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split dataset with respect to Uplift distribution
}
\description{
Split a dataset into training and validation subsets with respect to the uplift sample distribution.
}
\usage{
SplitUplift(data, p, group)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a data frame of interest that contains at least the response and the treatment variables.
}
  \item{p}{
The desired sample size. p is a value between 0 and 1 expressed as a decimal, it is set to be proportional to the number of observations per group.
}
  \item{group}{
Your grouping variables. Generally, for uplift modelling, this should be a vector of treatment and response variables names, e.g. c("treat", "y").
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
Mouloud Belbahri
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

library(tools4uplift)
scene <- GenerateScene(plotit = FALSE)
train <- GenerateUplift(scene, nbobs = 1000, dimension = 10)

split <- SplitUplift(train, 0.8, c("treat", "y"))
train <- split[[1]]
valid <- split[[2]]

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line