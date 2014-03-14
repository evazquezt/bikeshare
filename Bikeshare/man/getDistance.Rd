\name{getDistance}
\alias{getDistance}

\title{
A matrix of distances between stations
}
\description{
Given a station data object and subset strings describing starting and ending stations, return a matrix containing the either biking or driving distance in kilometers between each pair of stations.  Distances are taken from Google Maps. By using this function you are agreeing to the Google Maps API Terms of Service at \url{https://developers.google.com/maps/terms}. 
}
\usage{
getDistance(station.data.object, from.subset = TRUE,to.subset = TRUE,mode=c("bicycling","driving"), return.limit = FALSE,api_key)
}

\arguments{
  \item{station.data.object}{
	A list containing objects of class "BikeshareStation".  Obtained via \code{readStationData}.
}
  \item{from.subset}{
	String describing a subset of \code{station.data.object} to be used as starting stations.  Valid column names for subsetting are the station full name \code{name}, station latitude \code{lat}, station longitude \code{long}, unique station ID number \code{stationId}, number of bikes at a station \code{numBikes}, station installation date \code{installDate}, and station removal date \code{removalDate}.
}
  \item{to.subset}{
	String describing a subset of \code{station.data.object} to be used as ending stations.  Possible subset names are same as in \code{from.subset}.
}
  \item{mode}{
	Mode of transit between stations.  Equal to either "bicycling" or "driving".	
}
  \item{return.limit}{
	If TRUE, returns remaining available Google Maps API queries via \code{distQueryCheck()}.
}
	\item{api_key}{ A developer API key for cloudmade maps, if applicable.
}
}
\details{
Note that, without a developers' license, the Google Maps API limits queries to 2500 per day.
}
\value{
Returns a matrix.  Row names are the station IDs for all stations subsetted by from.subset, and column names are the station IDs for all stations subsetted by to.subset.  If the subsets overlaps, entries may be redundant.  Element \code{[i,j]} of the matrix gives the biking distance between the row station \code{i} and column station \code{j}.
}

\author{
Sara Kwasnick
}

\seealso{
\code{mapdist} from package \code{ggmap}.

The Google Maps Distance Matrix API, \url{http://code.google.com/apis/maps/documentation/distancematrix/}.
}
\examples{
\dontrun{	
## Not run
from.subset <- "stationId <= 31001"
to.subset <- "numBikes <= 10"
stations <- readStationData(system.file("extData/bikeStations.xml", package="Bikeshare"),.cities()$WAS)
getBikingDistance(stations,from.subset,to.subset)
}
}
