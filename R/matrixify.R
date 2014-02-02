matrixify <- function(df, order, newCols, roundCols, stats, 
                      newLabel, hgap, overallSum, ...) {
  ## matrixify for normal DF
  if (!overallSum) {
    ## default setting for order
    if (missing(order)) {
      order <- c("study", "effect")
    }
    
    ## main DF
    DF <- df$DF
    
    # generate main DF
    main.DF <- mainGen(df = DF, order = order, newCols = newCols, 
                       roundCols = roundCols, isSummary = FALSE)
    
    # transform main DF into matrix
    matrix.DF <- as.matrix(main.DF)
    
    ## set up gap between main DF and summary by default
    if (missing(hgap)) {
      matrix.DF <- rbind(matrix.DF, gap = "")
    } 
    
    ## summary
    # extract the fixed and the random summary
    summary <- list(fixed = df$summary.fixed, random = df$summary.random)
  }
  
  ## matrixify for overall summary
  else {
    ## summary
    # extract the overall summary
    summary <- list(fixed = df$overall.fixed, random = df$overall.random)
  }
 
  summary <- lapply(summary, mainGen, order = order, newCols = newCols,
                    roundCols = roundCols, isSummary = TRUE)
  
  round.sum <- rbind(summary$fixed, summary$random)
  matrix.sum <- as.matrix(round.sum)
  
  ## set up gap between heterogeneity information and summary by default
  if (missing(hgap)) {
    matrix.sum <- rbind(matrix.sum, gap = "")
  }
  
  ## hetero information
  hetero <- df$hetero
  
  matrix.hetero <- heteroGen(hetero = hetero, df = matrix.sum, 
                             stats = stats, newLabel = newLabel)
  
  ## set up the main matrix
  if (!overallSum) {
    matrix.full <- rbind(matrix.DF, matrix.sum, hetero = matrix.hetero)
  }
  else {
    matrix.full <- rbind(matrix.sum, hetero = matrix.hetero)
  }
  
  ## plotDF
  plot.list <- plotDF(df = df, hetero = matrix.hetero, hgap = hgap, overallSum = overallSum)
  
  list(matrix = matrix.full, plot.DF = plot.list$plot.DF,
       is.summary = plot.list$is.summary)
}