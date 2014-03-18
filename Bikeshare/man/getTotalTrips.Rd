\name{getTotalTrips}
\alias{getTotalTrips}

\title{
Total number of trips between pairs of stations
}
\description{
Given a set of bike trips and a set of stations, this function returns a dataframe reporting the number of trips between each pair of the stations.
}
\usage{
getTotalTrips(bikeshareData, tripSubset = TRUE, stationSubset = TRUE)
}
\arguments{
  \item{bikeshareData}{
A bikeshareData object, obtained via \code{readTripData}.
}
  \item{tripSubset}{
String describing which subset of trips to include in the plot.  Valid column names for subsetting are the trip start time \code{startTime}, trip end time \code{endTime}, trip start location \code{startLoc}, trip end locations \code{endLoc}, bike number \code{bike}, and member type \code{memberType}.
}
  \item{stationSubset}{
String describing which subset of stations in \code{bikeshareData} to plot.  Valid column names for subsetting are the station full name \code{name}, station latitude \code{lat}, station longitude \code{long}, unique station ID number \code{stationId}, number of bikes at a station \code{numBikes}, station installation date \code{installDate}, and station removal date \code{removalDate}.
}
}
\value{
Returns a data frame with three columns: X1 and X2 are character columns containing station ID numbers, and trips is a numeric column containing the number of trips either form X1 to X2 or X2 to X1. 

For example, the trips entry of row with X1="31000" and X2="31001" will contain the number of trips from station 31000 to 31001 plus the number of trips from 31001 to 31000.  A row with X1=X2="31000" will contain all trips that both started and ended from station 31000.
}
\author{
Sara Kwasnick
}
\seealso{
\code{getTotalVisits}
}
\examples{
\dontrun{
bd <- readTripData(system.file("extData/2010-4th-quarter.csv", package="Bikeshare"), .cities()$WAS, stations)
stationSubset <- "stationId <= 31020"
getTotalTrips(bd,stationSubset=stationSubset) 
}
}