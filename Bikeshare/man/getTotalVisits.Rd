\name{getTotalVisits}
\alias{getTotalVisits}
\title{
Compute total number of visits to each station
}
\description{
Given a set of bike trips and a set of stations, this function returns a dataframe reporting the number of visits to each station.
}
\usage{
getTotalVisits(bikeshareData, tripSubset = TRUE, stationSubset = TRUE, includeSelf = TRUE)
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
  \item{includeSelf}{
Takes value TRUE or FALSE.  If TRUE, counts trips from a station to itself in the total tally of visits.  If FALSE, it excludes trips from the station to itself.
}
}
\details{
The total number of visits is determined by how many times that station was visited in the trips dataset, either to pick up or drop off a bike.  When a bike was picked up AND dropped off at the same station (and \code{includeSelf==TRUE}), that station earns +2 visits rather than +1.
}
\value{
Returns a data frame with two columns: \code{station} contains station ID number, and \code{visits} contains the total number of visits to that station.
}
\author{
Sara Kwasnick
}
\seealso{
\code{getTotalTrips}, \code{plotBubbles}
}
\examples{
\dontrun{
bd <- readTripData(system.file("extData/2010-4th-quarter.csv", package="Bikeshare"), .cities()$WAS, stations)
stationSubset <- "stationId <= 31020"
getTotalVists(bd,stationSubset=stationSubset) 
}
}