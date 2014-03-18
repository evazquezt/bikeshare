\name{plotStations}
\alias{plotStations}

\title{
Plot station locations
}
\description{
Plot station locations on a hybrid map from bikeshare station data files.  Background map is taken from Google Maps. By using this function you are agreeing to the Google Maps API Terms of Service at \url{https://developers.google.com/maps/terms}.  
}
\usage{
plotStations(stationData,dataSubset=TRUE,zoom=13,col="red",apiKey=NULL)
}

\arguments{
  \item{stationData}{
	A list containing objects of class "BikeshareStation".  Obtained via \code{readStationData}.
}
  \item{dataSubset}{
	String describing which subset of stations in \code{stationData} to plot.  Valid column names for subsetting are the station full name \code{name}, station latitude \code{lat}, station longitude \code{long}, unique station ID number \code{stationId}, number of bikes at a station \code{numBikes}, station installation date \code{installDate}, and station removal date \code{removalDate}.
}
   \item{zoom}{
	Map zoom, integer ranging from 3 to 21 or equal to "auto".  3 is a continent; 21 is a building; default value is 13 = roughly a city center.
}
	\item{col}{
	Color of to be used for plotting station points on the map.	Default color is red.
}
	\item{apiKey}{ A developer API key for cloudmade maps, if applicable.
}
}

\details{
Any stations falling outside of the zoom window will be dropped from the map (generates a warning).
}
\value{
"Hybrid" map of city with dots showing station locations.
}

\author{
Sara Kwasnick
}
\note{
Requires \code{ggmap} and \code{ggplot2}.  Wraps ggmap for efficient bikeshare station plotting.
}

\seealso{
\code{BikeshareStation}, \code{readStationData}, \code{ggmap}, \code{plotBubbles}, \code{plotTrips}, \code{plotAlpha}
}
\examples{
\dontrun{
stations <- readStationData(system.file("extData/bikeStations.xml", package="Bikeshare"),.cities()$WAS)
plotStations(stations)
}
}
