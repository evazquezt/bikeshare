\name{BikeshareStation-class}
\docType{class}
\alias{BikeshareStation-class}
\alias{BikeshareStation}
\title{Class "BikeshareStation"}
\description{Object representing city-specific bikeshare data}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BikeshareStation", ...)}, or read from a typical file by  calling \code{scanGPSTRACK()}.
}
\section{Slots}{
	 \describe{
	   \item{\code{name}}{Name of station}
	   \item{\code{lat}}{Latitude of station}
	   \item{\code{long}}{Longitude of station}
	   \item{\code{installDate}}{Date when station was installed}
	   \item{\code{removalDate}}{Date when station was removed (NA otherwise)}
	   \item{\code{numBikes}}{Max number of bikes in station}
  }
}


\seealso{
	 \code{\link{readStationData}} for reading in this data   
}
\keyword{classes}
