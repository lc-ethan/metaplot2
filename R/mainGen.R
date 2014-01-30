##=============main DF matrix generating function====================##
mainGen <- function(df, order, newCols, roundCols, isSummary) {
  # step 1: add columns with specific format into main DF
  if (any(order %in% c("ci", "msd.e", "msd.c", names(newCols)))) {
    df <- addCols(df = df, order = order, newCols = newCols, 
                  isSummary = isSummary)
  }
 
  # step 2: extract the required columns to form a new data frame
  df <- df[order]
  
  # step 3: round up the main DF
  df <- roundUpCols(df = df, newCols = newCols, roundCols = roundCols,
                    isSummary = isSummary)
  
  df  
}
##==============================makeCol===============================##

## set up describtion format for column
makeColDesc <- function(format, colNames){
  x <- list(format = format, colNames = colNames)
  class(x) <- "colDesc"
  x
}

## generate column with specified format
makeCol <- function(colDesc, df, isSummary) {
  if (!inherits(colDesc, "colDesc")) {
    stop("unexpected format listed in the argument 'newCols'")
  }
  col.names <- names(df)
  if (!all(colDesc$colNames %in% col.names)) {
    stop("unexpected column names in makeColDesc()")
  }
  args <- lapply(colDesc$colNames, get, df)
  # check if the column cells are NA
  check.NA <- any(ifelse(is.null(nrow(sapply(args, is.na))),
                         all(sapply(args, is.na)), # with 1 row
                         apply(sapply(args, is.na), 1, all))) # with multiple rows
  if (isSummary && check.NA) {
    return("")
  }
  else {
    args <- c(list(fmt = colDesc$format), args)
    do.call("sprintf", args)
  }
}

## generate a set of columns to be used in mainGen()
addCols <- function(df, order, newCols, isSummary) {
  default.format <- list()
  default.format$ci <- list(format = paste("[", "%.", 2, "f", ", ", 
                                           "%.", 2, "f", "]", sep =""),
                            colNames = c("lower", "upper"))
  default.format$msd.e <- list(format = paste("% .", 1, "f", "(", "%.", 
                                              2, "f", ")", sep = ""),
                               colNames = c("mean.e", "sd.e"))
  default.format$msd.c <- list(format = paste("% .", 1, "f", "(", "%.", 
                                              2, "f", ")", sep = ""),
                               colNames = c("mean.c", "sd.c"))
  if (is.null(newCols)) {
    col.format <- default.format    
  }
  else {
    col.format <- list()
    if (!any(names(newCols) %in% "ci")) {
      col.format$ci <- default.format$ci
    }
    if (!any(names(newCols) %in% "msd.e")) {
      col.format$msd.e <- default.format$msd.e
    }
    if (!any(names(newCols) %in% "msd.c")) {
      col.format$msd.c <- default.format$msd.c
    }    
  }
  col.desc <- lapply(col.format, do.call, what = "makeColDesc")
  col.desc <- c(col.desc, newCols)
  req.col <- order[order %in% c("ci", "msd.e", "msd.c", names(newCols))]
  col.desc <- col.desc[req.col]
  new.col <- lapply(col.desc, makeCol, df = df, isSummary = isSummary)
  new.col <- as.data.frame(new.col)                   
  cbind(df, new.col)  
}

##==============================roundUpCol===================================##

## set up round for the column
setUpRound <- function(var, roundCols, isSummary) {
  defaultRoundCols <- c(n.e = 0, mean.e = 1, n.c = 0, mean.c = 1, sd.e = 2, 
                        sd.c = 2, effect = 2, w.fixed = ifelse(isSummary, 0, 1), 
                        w.random = ifelse(isSummary, 0, 1), mean = 2, lower = 2,
                        upper = 2, other = 2) 
  if (is.null(roundCols)) {
    if (var %in% names(defaultRoundCols)) {
      rounding <- defaultRoundCols[var]
      as.numeric(rounding)
    }
    else {
      rounding <- defaultRoundCols["other"]
      as.numeric(rounding)
    }
  }
  else {
    if (var %in% names(defaultRoundCols)) {
      rounding <- switch(as.character(roundCols[var]), 
                         "NA" = defaultRoundCols[var],
                         roundCols[var]) 
      as.numeric(rounding)
    }
    else {
      rounding <- switch(as.character(roundCols[var]), 
                         "NA" = defaultRoundCols["other"],
                         roundCols[var])
      as.numeric(rounding)
    }  
  } 
}

# round up the column with specified requirement
roundUpCols <- function(df, newCols, roundCols, isSummary) {
  if (all(names(df) %in% c("study", "ci", "msd.e", 
                           "msd.c", names(newCols)))) {
    df
  }
  else {
    temp.DF <- df[!(names(df) %in% c("study", "ci", "msd.e", 
                                     "msd.c", names(newCols)))]
    
    if (ncol(temp.DF) == 1 && !sapply(temp.DF, is.numeric)) {
      df
    }
    else { 
      var <- names(temp.DF)
      var <- var[sapply(temp.DF, function(df) is.numeric(df) || is.na(df))]
    
      round.cols <- sapply(var, setUpRound, roundCols = roundCols, 
                           isSummary = isSummary) 
      round.names <- names(round.cols)
      for (i in round.names) {
        df[, i] <- sprintf(paste("%0.", round.cols[i], "f", sep = ""),
                           df[, i]) 
      }
      df
    }
  }
}
     
       

