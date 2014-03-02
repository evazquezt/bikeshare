\name{readTripData}
\alias{readTripData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Read trip data from a downloaded trip history data file
  %%  ~~function to do ... ~~
}
\description{
  After you download a bikeshare trip data file, use this function
  to read in the data into a BikeshareData object.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readTripData(path,city)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
    Path to the data file
  }
  \item{city}{
    City for which the data comes from
    }
}
\details{

  Implementation uses the city argument to automatically retrieve data
  file conventions
}
\value{
  BikeshareData object containing all of the standard information from the data file.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  %%  ~~who you are~~
  John Sears
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#bd = readTripData(path="data/2010-4th-quarter.csv",city="WAS")
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
