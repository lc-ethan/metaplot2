\name{makeColDesc}
\alias{makeColDesc}

\title{
Make a column description in specified format 
}
\description{  
The function produces descriptions that are used for making a new column
from using existing columns. It is used inside the \code{newCols} argument in
the \code{\link{metaDF2Matrix}} function.
}
\usage{
makeColDesc(format, colNames)
}
\arguments{
  \item{format}{Format string for new column.}
  \item{colNames}{Names of existing columns.}
}
\details{
The column names are taken from the data frame of the \code{"metaDF"} object.
It is a convenience function for specifying the new column format within the
\code{metaDF2Matrix()} function.
}
\value{
An object of class \code{'coldesc'} with a list of following components:
  \item{format}{A format string}
  \item{colNames}{A character vector of column names}
}
\seealso{
\code{\link{makeCIDesc}},
\code{\link{makeMSDDesc}}
}
\examples{
makeColDesc("Study\%s (\%.0f,\%.0f)", c("study", "n.e", "n.c"))
}