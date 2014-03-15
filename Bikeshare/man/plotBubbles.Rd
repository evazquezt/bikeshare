\name{plotBubbles}
\alias{plotBubbles}
\title{
Plot stations as bubbles
}
\description{
Given a bikeshareData object and subset string describing which trips and stations to include, return a bubble plot of stations with bubble size corresponding to usage of that station.  Background map is taken from Google Maps. By using this function you are agreeing to the Google Maps API Terms of Service at \url{https://developers.google.com/maps/terms}.  
}
\usage{
plotBubbles(bikeshareData, trip.subset = TRUE, station.subset = TRUE, zoom = 13, col = "red", alpha = 0.2, api_key = NULL)
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
	Bubble color.  Default is red.
}
  \item{alpha}{
	Alpha blending parameter in [0,1].  Default is 0.2.
}
  \item{api_key}{
	A developer API key for cloudmade maps, if applicable.
}
}
\details{
Station usage is determined by how many times that station was visited in the dataset, either to pick up or drop off a bike.  Thus, when a bike was picked up AND dropped off at the same station, that station earns +2 visits rather than +1.

Any stations falling outside of the zoom window will be dropped from the map (generates a warning).
}
\value{
A hybrid map overlaid by bubbles at station locations, with bubble size corresponding to station usage.  Map includes a bubble-size legend.
}
\author{
Sara Kwasnick
}
\note{
Requires \code{ggmap} and \code{ggplot2}. 
}

\seealso{
\code{ggmap}, \code{plotStations}, \code{plotTrips}, \code{plotAlpha}, \code{buildDestinationMatrix}
}
\examples{
\dontrun{
stations <- readStationData(system.file("extData/bikeStations.xml", package="Bikeshare"),.cities()$WAS)
bd <- readTripData(system.file("extData/2010-4th-quarter.csv", package="Bikeshare"), .cities()$WAS, stations)

## small example
station.subset <- "stationId <= 31020"
plotBubbles(bd,station.subset=station.subset,zoom=14,alpha=.5)

## longer example, just looking at trips that started and ended at the same station
trip.subset <- "startLoc==endLoc" 
plotBubbles(bd,trip.subset=trip.subset)		
}	
}
