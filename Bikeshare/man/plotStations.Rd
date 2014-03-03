\name{plotStations}
\alias{plotStations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot station locations
}
\description{
Plot station locations on a hybrid map from bikeshare station data files.
}
\usage{
plotStations(data, city, zoom=13)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Path to stations data file
}
  \item{city}{
City object identifying bikeshare city
}
  \item{zoom}{
Map zoom, integer ranging from 3 to 21.  3 is a continent; 21 is a building; default value is 13 = roughly a city center. ** note: need to integrate "auto" still...
}
}

\value{
Map of city with red dots showing station locations.
}

\author{
Sara Kwasnick
}
\note{
Require ggmap and ggplot2
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
BikeshareStation, readStationData
}
\examples{
# plotStations("Bikeshare/inst/extData/bikeStations.xml",WAS)
}
