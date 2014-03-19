\name{BikeshareCity-class}
\docType{class}
\alias{BikeshareCity-class}
\alias{BikeshareCity}
\alias{.cities}

\title{Class "BikeshareCity"  }
\description{Object representing city-specific bikeshare data information}

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
\author{
  John Sears
}


\seealso{
	 \code{\link{readTripData}} for reading in this data   
}
\keyword{classes}
