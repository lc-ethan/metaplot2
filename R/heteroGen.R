##=============heterogeneity information generating function====================##

## heteroGen() used in metaDF2Matrix() to generate hetero info
heteroGen <- function(hetero, df, stats, newLabel, metaClass, overallSum) {  
  # step 1: check argument
  if (!is.null(newLabel)){
    if (!all(sapply(newLabel, inherits, what = "labelDesc"))) {
      stop("unexpected class listed under 'newLabel'")
    }   
  }
  
  # step 2: creat hetero labels for the stats information
  hetero.label <- addLabel(hetero = hetero, newLabel = newLabel, metaClass = metaClass, 
                           overallSum = overallSum) 
  
  # step 3: create the stats information as the list
  hetero.stats <- lapply(stats, FUN = addStats, 
                         label = hetero.label, hetero = hetero)
  
  # step 4: transform it into a matrix
  t(sapply(hetero.stats, function(stats, df) c(stats, rep("", ncol(df) - 1)), 
           df = df))
}

##==================================makeLabel====================================##

## set up describtion format for label
makeLabelDesc <- function(format, heteroNames) {
  x <- list(format = format, heteroNames = heteroNames)
  class(x) <- "labelDesc"
  x  
}

## generate label with specified format
makeLabel <- function(labelDesc, hetero) {
  hetero.names <- names(hetero)
  if (!all(labelDesc$heteroNames %in% hetero.names)) {
    stop("unexpected hetero names in makeLabelDesc()")
  }
  hetero <- as.list(hetero)
  args <- lapply(labelDesc$heteroNames, get, hetero)
  args <- c(list(fmt = labelDesc$format), args)
  do.call("sprintf", args)
}

## generate a set of labels to be used in heteroGen()
addLabel <- function(hetero, newLabel, metaClass, overallSum) { 
  default.format <- list()
  default.format$Q <- list(format = paste("Chi-square =", "% .", 2, "f", sep = ""), 
                           heteroNames = "Q")
  
  default.format$p <- list(format = paste("(p =", "% .", 3, "f", ")", sep = ""), 
                           heteroNames = "p")
  
  default.format$df <- list(format = paste("df =", "% .", 0, "f", sep = ""), 
                            heteroNames = "df") 
  
  default.format$tau2 <- list(format = paste("tau-squared =", "% .", 4, "f", sep = ""), 
                              heteroNames = "tau2")
  
  default.format$H <- list(format = paste("H =", "% .", 2, "f", sep = ""), heteroNames = "H")
  
  default.format$H.ci <- list(format = paste("[", "%.", 2,  "f",  ",", 
                                             "% .", 2, "f", "]", sep = ""), 
                              heteroNames = c("H.lower", "H.upper"))
  
  default.format$I2 <- list(format = paste("I-Squared =", "% .", 2, "f", "%%", sep = ""), 
                            heteroNames = "I2.per")
  
  default.format$I2.ci <- list(format = paste("[", "%.", 2,  "f", "%%",  ",", "% .", 2,
                                              "f", "%%", "]", sep = ""), 
                               heteroNames = c("I2.lower.per", "I2.upper.per"))
  
  
  if (any(metaClass == "metabinDF")) {
    if (any(metaClass == "groupedMetaDF")) {
      if (overallSum) {
        default.format$Q.CMH <- list(format = paste("Test for overall effect: ", "Q = ",
                                                    "%.", 2, "f", sep = ""),
                                     heteroNames = "Q.CMH")
      }  
    }
    else {
      default.format$Q.CMH <- list(format = paste("Test for overall effect: ", "Q = ",
                                                  "%.", 2, "f", sep = ""),
                                   heteroNames = "Q.CMH")
    }
  }
  
  
  if (is.null(newLabel)) {
    label.format <- default.format
    hetero["I2.per"] <- hetero["I2"] * 100
    hetero["I2.lower.per"] <- hetero["I2.lower"] * 100 
    hetero["I2.upper.per"] <- hetero["I2.upper"] * 100
  }
  else {
    label.format <- list()
    if (!any(names(newLabel) %in% "Q")) {
      label.format$Q <- default.format$Q
    }
    if (!any(names(newLabel) %in% "p")) {
      label.format$p <- default.format$p
    }
    if (!any(names(newLabel) %in% "df")) {
      label.format$df <- default.format$df
    }
    if (!any(names(newLabel) %in% "tau2")) {
      label.format$tau2 <- default.format$tau2
    }
    if (!any(names(newLabel) %in% "H")) {
      label.format$H <- default.format$H
    }
    if (!any(names(newLabel) %in% "H.ci")) {
      label.format$H.ci <- default.format$H.ci
    }
    if (!any(names(newLabel) %in% "I2")) {
      label.format$I2 <- default.format$I2
      hetero["I2.per"] <- hetero["I2"] * 100
    }
    if (!any(names(newLabel) %in% "I2.ci")) {
      label.format$I2.ci <- default.format$I2.ci
      hetero["I2.lower.per"] <- hetero["I2.lower"] * 100 
      hetero["I2.upper.per"] <- hetero["I2.upper"] * 100
    }
    if (any(metaClass == "metabinDF")) {
      if (!any(names(newLabel) %in% "Q.CMH")) {
        label.format$Q.CMH <- default.format$Q.CMH
      }  
    }
  }
  label.desc <- lapply(label.format, do.call, what = "makeLabelDesc")
  label.desc <- c(label.desc, newLabel)
  lapply(label.desc, makeLabel, hetero = hetero)
}

##==================================makeStats====================================##

## set up describtion format for hetero information
makeStatsDesc <- function(labelNames, heading, newStatsFormat, 
                          emptyHeading = FALSE) {
  if (!emptyHeading) {
    if (missing(heading) && missing(newStatsFormat)) {
      format <- paste("Heterogeneity:", paste(rep("%s", length(labelNames)),
                                              collapse = " "))
      x <- list(format = format, statsNames = labelNames)
      class(x) <- "statsDesc"
    }
    
    if (!missing(heading) && missing(newStatsFormat)) {
      format <- paste(heading, paste(rep("%s", length(labelNames)), 
                                     collapse = " "))
      x <- list(format = format, statsNames = labelNames)
      class(x) <- "statsDesc"
    }
    
    if (!missing(newStatsFormat)) {
      format <- newStatsFormat
      x <- list(format = format, statsNames = labelNames)
      class(x) <- "statsDesc"
    }  
    x   
  }
  else {
    format <- paste(paste(rep(" ", 23), collapse = ""), 
                    paste(rep("%s", length(labelNames)), collapse = " "))
    x <- list(format = format, statsNames = labelNames)
    class(x) <- "statsDesc" 
    x
  }
}

## generate hetero info with specified format
makeStats <- function(statsDesc, label) {
  label.names <- names(label)
  if (!all(statsDesc$statsNames %in% label.names)) {
    stop("unexpected label names in makeStatsDesc()")
  }
  args <- lapply(statsDesc$statsNames, get, label)
  args <- c(list(fmt = statsDesc$format), args)
  do.call("sprintf", args)  
}

## generate a set of hetero info to be used in heteroGen()
addStats <- function(stats, label, hetero) {
  if (inherits(stats, c("statsDesc", "labelDesc"))) {
    if (inherits(stats, "statsDesc")) {
      hetero.stats <- makeStats(statsDesc = stats, label = label)
    }
    else {
      hetero.stats <- makeLabel(labelDesc = stats, hetero = hetero)
    }  
  }
  else {
    hetero.stats <- sprintf(paste(as.character(stats), collapse = ""))
  }  
  hetero.stats
}


