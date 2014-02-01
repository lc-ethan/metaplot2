matrixify <- function(df, order, newCols, roundCols, stats, 
                      newLabel, colNames, groupLab, hgap, vgap) {
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
  
  matrix.hetero <- heteroGen(hetero = hetero, df = matrix.DF, 
                             stats = stats, newLabel = newLabel)
  
  ## set up the main matrix
  matrix.full <- rbind(matrix.DF, matrix.sum, hetero = matrix.hetero)
  
  ## plotDF
  plot.list <- plotDF(df = df, hetero = matrix.hetero, hgap = hgap )
  plot.DF <- plot.list$plot.DF
  is.summary <- plot.list$is.summary
  
  ## set up column names
  # matrix
  if (missing(colNames)) {
    matrix.full <- rbind(colnames = colnames(matrix.full), matrix.full)
  }
  else {
    if (length(colNames) != ncol(matrix.full))
      stop("the number of column names does not match the number of columns")
    matrix.full <- rbind(colnames = colNames, matrix.full)
  }
  
  # plot.DF
  plot.DF <- rbind(NA, plot.DF)
  is.summary <- c(TRUE, is.summary)
  
  ## set up title and subtitle
  if (!is.null(df$subtitle)) {
    subtitle <- c(df$subtitle, rep(NA, ncol(matrix.full) - 1))
    if (missing(hgap)) {
      matrix.full <- rbind(subtitle = subtitle, gap = "",  matrix.full)
      plot.DF <- rbind(NA, NA, plot.DF)
      is.summary <- c(TRUE, FALSE, is.summary)
    }
    else {
      matrix.full <- rbind(subtitle = subtitle, matrix.full)
      plot.DF <- rbind(NA, plot.DF)
      is.summary <- c(TRUE, is.summary)  
    }    
  }
  
  if (!is.null(df$title)) {
    title <- c(df$title, rep(NA, ncol(matrix.full) - 1))
    if (missing(hgap) && is.null(df$subtitle)) {
      matrix.full <- rbind(title = title, gap = "",  matrix.full)
      plot.DF <- rbind(NA, NA, plot.DF)
      is.summary <- c(TRUE, FALSE, is.summary)  
    }
    else {
      matrix.full <- rbind(title = title,  matrix.full)
      plot.DF <- rbind(NA, plot.DF)
      is.summary <- c(TRUE, is.summary)  
    }
  }
  
  ## insert gaps by users specification
  if (!missing(hgap)) {
    for (i in 1:length(hgap)) {
      matrix.full <- rbind(matrix.full[1:(hgap[i] - 1), , drop = FALSE], gap = "", 
                           matrix.full[hgap[i]:nrow(matrix.full), , drop = FALSE])
      plot.DF <- rbind(plot.DF[1:(hgap[i] - 1), , drop = FALSE], NA,
                       plot.DF[hgap[i]:nrow(plot.DF), , drop = FALSE])
      is.summary <- c(is.summary[1:(hgap[i] - 1)], FALSE, 
                      is.summary[hgap[i]:length(is.summary)])
    }
  }
  if (!missing(vgap)) {
    for (i in 1:length(vgap)) {
      matrix.full <- cbind(matrix.full[, 1:(vgap[i] - 1), drop = FALSE], gap = "",
                           matrix.full[, vgap[i]:ncol(matrix.full), drop = FALSE])
    }
  }
  
  ## combine plotting information
  plot.DF <- cbind(plot.DF, is.summary)
  
  list(matrix = matrix.full, plotDF = plot.DF)
}