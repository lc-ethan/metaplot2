##============metaDF2Matrix.metacont==================##
## step 1 function for checking order argument
checkOrder <- function(df, order, newCols) {
  if (!missing(order)){
    if (!all(order %in% c(colnames(df$DF), names(newCols), "ci", "msd.e", "msd.c")))
      stop("unexpected name listed under argument 'order'")
  }  
}

## step 3 function for adding CI and MSD
addCIMSD <- function(df, order, newCols, isSummary) {
  # add CI
  if (any(order %in% "ci") && !any(names(newCols) %in% "ci")) {
    # set up format
    ci <- makeCIDesc(col1 = "lower", col2 = "upper", round = 2,
                     brackets = c("[", "]"), sep = ", ")
    # insert the new column
    df[, "ci"] <- makeCol(coldesc = ci, df = df, isSummary = isSummary)      
  }
  
  # add msd.e
  if (any(order %in% "msd.e") && !any(names(newCols) %in% "msd.e")) {
    # set up format
    MSD <- makeMSDDesc(col1 = "mean.e", col2 = "sd.e", round = c(1, 2),
                       brackets = c("(", ")"))
    # insert the new column
    df[, "msd.e"] <- makeCol(coldesc = MSD, df = df, isSummary = isSummary)
  }
  
  # add msd.c
  if (any(order %in% "msd.c") && !any(names(newCols) %in% "msd.c")) {
    # set up format
    MSD <- makeMSDDesc(col1 = "mean.c", col2 = "sd.c", round = c(1, 2),
                       brackets = c("(", ")"))
    # insert the new column
    df[, "msd.c"] <- makeCol(coldesc = MSD, df = df, isSummary = isSummary)
  }
  df
}

## step 4 add columns with new format
addCols <- function(df, newCols, isSummary) {
  if (!is.null(newCols)) {
    if (!is.list(newCols)) {
      stop("unexpected format listed under argument newCols")  
    }
    else {
      newcolnames <- names(newCols)
      for (i in newcolnames) {
        if(inherits(newCols[[i]], "coldesc")) {
          df[, i] <- makeCol(coldesc = newCols[[i]], df = df, isSummary)
        }
        else {
          stop("unexpected format listed under argument newCols")
        }
      }  
    } 
  }
  df
}

## step 5 rounding columns
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

roundingCols <- function(df, newCols, roundCols, isSummary) {
  temp.DF <- df[!(names(df) %in% c("study", "ci", "msd.e", "msd.c", names(newCols)))]
  var <- names(temp.DF[, sapply(temp.DF, is.numeric)])
  round.cols <- sapply(var, setUpRound, roundCols = roundCols, 
                       isSummary = isSummary) 
  roundnames <- names(round.cols)
  for (i in roundnames) {
    df[, i] <- sprintf(paste("%0.", round.cols[i], "f", sep = ""),
                       df[, i])    
  }
  list(df = df, roundCols = round.cols)
}

