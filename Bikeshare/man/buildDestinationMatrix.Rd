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
buildDestinationMatrix(bikeshareData)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bikeshareData}{
    A BikeshareData object
}
}

\author{
  John Sears
}

