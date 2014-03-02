\name{BikeshareData-class}
\Rdversion{1.1}
\docType{class}
\alias{BikeshareData-class}
\alias{BikeshareData}
\alias{as.data.frame.BikeshareData}
\title{Class \code{"BikeshareData"}}
\description{
Object for storing bikeshare data
}
\section{Objects from the Class}{
Objects can be created by calls of the form
\code{BikeshareData(location, startTime, endTime, startLoc, endLoc,
  bike, memberType, stationNames)}.
}
\section{Slots}{
  \describe{
    \item{\code{location}:}{Name of city }
    \item{\code{startTime}:}{Start time of trip }
    \item{\code{endTime}:}{End time of trip}
    \item{\code{startLoc}:}{Start location of trip}
    \item{\code{endLoc}:}{End location of trip}
    \item{\code{bike}:}{Bike used}
    \item{\code{memberType}:}{Type of membership (member, guest)}
    \item{\code{stationNames}:}{Names of stations in data file (to be
      matched with BikeshareStation objects)}
  }
}
\section{Methods}{
as.data.frame
}
\author{
John Sears
}

\seealso{
  readTripData
}
\examples{
showClass("BikeshareData")
}
\keyword{classes}
