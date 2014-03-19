\name{BikeshareStation-class}
\docType{class}
\alias{BikeshareStation-class}
\alias{BikeshareStation}
\alias{as.data.frame.BikeshareStation}
\title{Class "BikeshareStation"}
\description{Object representing city-specific bikeshare data}

\section{Slots}{
	 \describe{
	   \item{\code{name}}{Name of station}
	   \item{\code{lat}}{Latitude of station}
	   \item{\code{long}}{Longitude of station}
	   \item{\code{installDate}}{Date when station was installed}
	   \item{\code{removalDate}}{Date when station was removed (NA otherwise)}
	   \item{\code{stationId}}{Station id number}
	   \item{\code{numBikes}}{Max number of bikes in station}
  }
}
\author{
  John Sears
}


\seealso{
	 \code{\link{readStationData}} for reading in this data   
}
\keyword{classes}
