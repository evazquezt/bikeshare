\name{readStationData}
\alias{readStationData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read station data
}
\description{
Read format-specific bikeshare station data files
}
\usage{
readStationData(stationPath, city)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stationPath}{
    Path to stations data file
}
  \item{city}{
    City object identifying bikeshare city
}
}
\value{
 List of BikeshareStation objects
}

\author{
  John Sears
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
BikeshareStation
}
\examples{
#readStationData("data/was/bikeStations.xml", WAS)
}