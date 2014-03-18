\name{buildDestinationMatrix}
\alias{buildDestinationMatrix}
\title{
  Function to build an matrix counting trips from rows to columns.
}
\description{
  Matrix returned has n (number of stations) rows and columns.  Element
  in (i,j) is the total number of trips in the dataset originating at
  station i and ending at station j.
}
\usage{
buildDestinationMatrix(bikeshareData,tripSubset,stationSubset)
}

\arguments{
  \item{bikeshareData}{
    A BikeshareData object, obtained via \code{readTripData}.
}
	\item{tripSubset}{
	String describing which subset of trips to include in the tally.  Valid column names for subsetting are the trip start time \code{startTime}, trip end time \code{endTime}, trip start location \code{startLoc}, trip end locations \code{endLoc}, bike number \code{bike}, and member type \code{memberType}.
}
	\item{stationSubset}{
	String describing which subset of station to include in the matrix.  Valid column names for subsetting are the station full name \code{name}, station latitude \code{lat}, station longitude \code{long}, unique station ID number \code{stationId}, number of bikes at a station \code{numBikes}, station installation date \code{installDate}, and station removal date \code{removalDate}.	
}
}

\author{
  John Sears & Sara Kwasnick
}

