\name{readTripData}
\alias{readTripData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Read trip data from a downloaded trip history data file

}
\description{
  After you download a bikeshare trip data file, use this function
  to read in the data into a BikeshareData object.
}
\usage{
readTripData(path,city)
}
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
\author{
  John Sears
}

\examples{
#bd = readTripData(path="data/2010-4th-quarter.csv",city="WAS")
}