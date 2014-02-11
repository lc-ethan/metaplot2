forestDF <- function(object, ...) UseMethod("forestDF")

forestDF.meta.MH <- function(object, study, rate, logRate, 
                             selogRate, lower, upper) {
  if (inherits(object, "meta.MH.OR")) {
    DF <- data.frame(study = study, OR = rate, logOR = logRate, selogOR = selogRate,
                     lower = lower, upper = upper)
  }
  else {
    DF <- data.frame(study = study, RR = rate, logRR = logRate, selogRR = selogRate,
                     lower = lower, upper = upper)
  }
  rownames(DF) <- 1:nrow(DF)
  DF
}

forestDF.meta.DSL <- function(object, study, rate, logRate, 
                             selogRate, lower, upper) {
  if (inherits(object, "meta.MH.OR")) {
    DF <- data.frame(study = study, OR = rate, logOR = logRate, selogOR = selogRate,
                     lower = lower, upper = upper)
  }
  else {
    DF <- data.frame(study = study, RR = rate, logRR = logRate, selogRR = selogRate,
                     lower = lower, upper = upper)
  }
  rownames(DF) <- 1:nrow(DF)
  DF
}

rmeta2DF.meta.MH <- function(meta, add = NULL, rowOrder = NULL,
                            title = NULL, subtitle = NULL, ...) {

  sum.meta <- summary(meta)
  ## step 1: set up main data frame
  DF <- forestDF(object = meta, study = meta$names, rate = sum.meta$stats[, meta$statistic], 
                 logRate = if (meta$statistic == "OR") meta$logOR else meta$logRR, 
                 selogRate = if (meta$statistic == "OR") meta$selogOR else meta$selogRR,
                 lower = sum.meta$stats[, "(lower "], 
                 upper = sum.meta$stats[, paste(100 * meta$conf.level, "% upper)", sep = "")])
  
  ## step 2: set up fixed effect
  summary.fixed <- forestDF(object = meta, study = "summary.fixed", rate = sum.meta$MHci[2],
                            logRate = meta$logMH, selogRate = meta$selogMH,
                            lower = sum.meta$MHci[1], upper = sum.meta$MHci[3])
  
  ## step 3: customization on the main data frame
  # attach additional columns to the meta object
  if (!is.null(add)) {
    # attach the additional column to the main data frame
    DF <- cbind(DF, add)
    # attach the corresponding space to the summary data frame
    addspace <- lapply(add, function(x){x <- ""})
    summary.fixed <- cbind(summary.fixed, addspace)   
  }
  
  # specify row orders
  if (!is.null(rowOrder)) {
    Order <- order(DF[, rowOrder], ...)
    DF <- DF[Order, ]
  }
  
  ## step 4: heterogeneity information
  hetero <- c(Q = meta$het[1], df = meta$het[2], p = meta$het[3], 
              con.level = meta$conf.level)
  
  ## step 5: set up the titles
  Title <- title
  Subtitle <- subtitle
  
  ## step 6: the wrap up
  output <- list(DF = DF, summary.fixed = summary.fixed, hetero = hetero, 
                 title = Title, subtitle = Subtitle)

  class(output) <- c("meta.MH.DF", "metaDF")
  
  output 
}
