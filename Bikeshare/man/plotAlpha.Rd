\name{plotAlpha}
\alias{plotAlpha}

\title{
Plot alpha-blended routes taken between stations
}
\description{
Given a bikeshareData object and subset string describing which trips and stations to include, return a plot showing all departure-arrival station pairs connected by alpha-blended lines.  Background map is taken from Google Maps. By using this function you are agreeing to the Google Maps API Terms of Service at \url{https://developers.google.com/maps/terms}.  
}
\usage{
plotAlpha(bikeshareData, trip.subset = TRUE, station.subset = TRUE, zoom = 13, col = "red", alpha = 0.2, api_key = NULL)
}

\arguments{
  \item{bikeshareData}{
	A bikeshareData object, obtained via \code{readTripData}.
}
  \item{trip.subset}{
	String describing which subset of trips to include in the plot.  Valid column names for subsetting are the trip start time \code{startTime}, trip end time \code{endTime}, trip start location \code{startLoc}, trip end locations \code{endLoc}, bike number \code{bike}, and member type \code{memberType}.
}
  \item{station.subset}{
String describing which subset of stations in \code{bikeshareData} to plot.  Valid column names for subsetting are the station full name \code{name}, station latitude \code{lat}, station longitude \code{long}, unique station ID number \code{stationId}, number of bikes at a station \code{numBikes}, station installation date \code{installDate}, and station removal date \code{removalDate}.
}
  \item{zoom}{
	Map zoom, integer ranging from 3 to 21 or equal to "auto".  3 is a continent; 21 is a building; default value is 13 = roughly a city center.
}
  \item{col}{
	Color to be used for plotting station connections on the map.  Default color is red.
}
  \item{alpha}{
 Alpha blending parameter in [0,1].  Default is 0.2.
}
  \item{api_key}{
	A developer API key for cloudmade maps, if applicable.
}
}

\details{
Ignores trips that departed from and returned to the same station, as these would not show up point-to-point on a map.

Any stations falling outside of the zoom window will be dropped from the map (generates a warning).
}

\value{
A hybrid map overlaid by a network of lines connecting all station locations for which at least one trip occured.
}

\author{
Sara Kwasnick
}
\note{
Requires \code{ggmap} and \code{ggplot2}. 
}

\seealso{
\code{ggmap}, \code{plotStations}, \code{plotTrips}, \code{plotBubbles}, \code{buildDestinationMatrix}
}
\examples{
\dontrun{
stations <- readStationData(system.file("extData/bikeStations.xml", package="Bikeshare"),.cities()$WAS)
plotStations(stations)
bd <- readTripData(system.file("extData/2010-4th-quarter.csv", package="Bikeshare"), .cities()$WAS, stations)
station.subset <- "stationId <= 31010"
plotAlpha(bd,station.subset=station.subset)
}
}