\name{BikeshareCity-class}
\docType{class}
\alias{BikeshareCity-class}
\alias{BikeshareCity}
\alias{.cities}

\title{Class "BikeshareCity"  }
\description{Object representing city-specific bikeshare data}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BikeshareCity", ...)}, or read from a typical file by  calling \code{scanGPSTRACK()}.
}
\section{Slots}{
	 \describe{
	   \item{\code{name}}{Name of city}
	   \item{\code{delim}}{File delimiter of city's bikeshare trip
	     data files}
	   \item{\code{header}}{Logical whether city trip file includes
	     a header}
	   \item{\code{keepCols}}{Columns from which to extract standard
	     data}
	   \item{\code{memberTypes}}{The valid types of membership
	     available}
	   \item{\code{timeFormat}}{Format string to pull date/time from}
  }
}


\seealso{
	 \code{\link{readTripData}} for reading in this data   
}
\keyword{classes}
