\name{plotTrips}
\alias{plotTrips}

\title{
Plot routes taken between stations, color-coded by frequency
}
\description{
Given a bikeshareData object and subset string describing which trips and stations to include, return a plot showing all departure-arrival station pairs color-coded and alpha-blended by frequency.  Background map is taken from Google Maps. By using this function you are agreeing to the Google Maps API Terms of Service at \url{https://developers.google.com/maps/terms}.  
}
\usage{
plotTrips(bikeshareData, trip.subset = TRUE, station.subset = TRUE, zoom = 13, col = "red", alpha = 0.2, api_key = NULL)
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
	Base color for a palette to categorize routes by frequnency.  Default is red.
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
A hybrid map overlaid by a network of lines connecting all station pairs between which at least one trip occured.  Lines are color-coded and overlaid by frequency, with less popular routes closer to white and more popular routes closer to user-supplied \code{col}. 
}
\author{
Sara Kwasnick
}
\note{
Requires \code{ggmap} and \code{ggplot2}.  Avoid setting \code{col="white"}; infrequent routes are already designated as white, so an input of this nature will result in all routes being white.
}

\seealso{
\code{ggmap}, \code{plotStations}, \code{plotAlpha}, \code{plotBubbles}, \code{buildDestinationMatrix}
}
\examples{
\dontrun{
stations <- readStationData(system.file("extData/bikeStations.xml", package="Bikeshare"),.cities()$WAS)
bd <- readTripData(system.file("extData/2010-4th-quarter.csv", package="Bikeshare"), .cities()$WAS, stations)
station.subset <- "stationId <= 31020"
plotAlpha(bd,station.subset=station.subset)	
}
}

