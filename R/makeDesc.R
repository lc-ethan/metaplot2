## functions for set up special describtion format 
makeColDesc <- function(format, colnames){
  x <- list(format = format, colnames = colnames)
  class(x) <- "coldesc"
  x
}


makeCol <- function(coldesc, df, isSummary = FALSE) {
  args <- lapply(coldesc$colnames, get, df)
  ## if both columns have NA and it is a summary information 
  ## set the cell as blank
  # step performed for df with only one row
  check.NA <- any(ifelse(is.null(nrow(sapply(args, is.na))),
                         all(sapply(args, is.na)),
                         apply(sapply(args, is.na), 1, all)))
  if (isSummary && check.NA) {
    return("")
  }
  else {
    args <- c(list(fmt = coldesc$format), args)
    do.call("sprintf", args)
  }
}


makeCIDesc <- function(col1 = "lower", col2 = "upper", round = 2,
                       brackets = c("[", "]"), sep = ", ") {
  makeColDesc(format = paste(brackets[1], "% .", round, "f", sep,
                             "% .", round, "f",  brackets[2], sep = ""),
              colnames = c(col1, col2))
}

makeMSDDesc <- function(col1 = "mean", col2 = "sd", round = c(1, 2),
                        brackets = c("(", ")")) {
  makeColDesc(format = paste("% .", round[1], "f", brackets[1],
                             "%.", round[2], "f",  brackets[2], sep = ""),
              colnames = c(col1, col2))
}

## functions for heterogeneity information
# function for generating customized format for parts
makeStatsDesc <- function(format, statsNames) {
  x <- list(format = format, statsNames = statsNames)
  class(x) <- "statsDesc"
  x  
}

makeStats <- function(statsDesc, hetero) {
  x <- as.list(hetero)
  args <- lapply(statsDesc$statsNames, get, x)
  args <- c(list(fmt = statsDesc$format), args)
  do.call("sprintf", args)
}


