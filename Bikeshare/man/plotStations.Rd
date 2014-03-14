\name{plotStations}
\alias{plotStations}

\title{
Plot station locations
}
\description{
Plot station locations on a hybrid map from bikeshare station data files.
}
\usage{
plotStations(station.data.object,sbst=TRUE,zoom=13,col="red",api_key)
}

\arguments{
  \item{station.data.object}{
	A list containing objects of class "BikeshareStation".  Obtained via \code{readStationData}.
}
  \item{sbst}{
	String describing which subset of station in \code{station.data.object} to plot.  Valid column names for subsetting are the station full name \code{name}, station latitude \code{lat}, station longitude \code{long}, unique station ID number \code{stationId}, number of bikes at a station \code{numBikes}, station installation date \code{installDate}, and station removal date \code{removalDate}.
}
   \item{zoom}{
	Map zoom, integer ranging from 3 to 21.  3 is a continent; 21 is a building; default value is 13 = roughly a city center.
}
	\item{col}{
	Color of to be used for plotting station points on the map.	Default color is red.
}
	\item{api_key}{ A developer API key for cloudmade maps, if applicable.
}
}

\value{
Map of city with dots showing station locations.
}

\author{
Sara Kwasnick
}
\note{
Requires \code{ggmap} and \code{ggplot2}.  Wraps ggmap for efficient bikeshare station plotting.
}

\seealso{
\code{BikeshareStation}, \code{readStationData}, \code{ggmap}
}
\examples{
\dontrun{
## not run
stations <- readStationData(system.file("extData/bikeStations.xml", package="Bikeshare"),.cities()$WAS)
plotStations(stations)
}
}
