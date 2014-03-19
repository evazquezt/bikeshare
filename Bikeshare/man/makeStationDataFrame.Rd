\name{makeStationDataFrame}
\alias{makeStationDataFrame}
\title{
  Generate a data frame of BikeshareStation data from a list of station objects
}
\description{
  Extract all information from a list of BikeshareStation objects into a
  data frame.
}
\usage{
makeStationDataFrame(stations)
}

\arguments{
  \item{stations}{
    List of BikeshareStation objects.
}
}
\author{
John Sears
}

\seealso{
  readStationData
}
\examples{
\dontrun{
# Load data
stations = readStationData(system.file("extData/bikeStations.xml",
  package="Bikeshare"),.cities()$WAS)
# Convert to a data.frame for analysis
stations.df = makeStationDataFrame(stations)
}
}
