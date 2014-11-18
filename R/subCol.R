subCol <- function(colDF, colSum) {
  list(DF = colDF, sum = if (missing(colSum)) "" else colSum)
}