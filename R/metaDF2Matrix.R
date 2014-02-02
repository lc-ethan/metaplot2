##=========================metaDF2Matrix=========================##
metaDF2Matrix <- function(object, ...) UseMethod("metaDF2Matrix")

##==========================metacont=============================##
metaDF2Matrix.metacontDF <- function(df, order, newCols = NULL, roundCols = NULL,
                                     stats = list(hetero = makeStatsDesc(labelNames = c("Q", "p", "df"))), 
                                     newLabel = NULL, colNames, groupLab, hgap, vgap, ...) {
  if (!inherits(df, "groupedMetaDF")){
    ## generate matrix for plotting
    metaMatrix <- matrixify(df = df, order = order, newCols = newCols, 
                            roundCols = roundCols, stats = stats, 
                            newLabel = newLabel, hgap = hgap, overallSum = FALSE, ...) 
    
    plot.matrix <- metaMatrix$matrix
    plot.DF <- metaMatrix$plot.DF
    is.summary <- metaMatrix$is.summary
    
    ## set up column names for single group
    if (missing(colNames)) {
      plot.matrix <- rbind(colnames = colnames(plot.matrix), plot.matrix)
    }
    else {
      if (length(colNames) != ncol(plot.matrix))
        stop("the number of column names does not match the number of columns")
      plot.matrix <- rbind(colnames = colNames, plot.matrix)
    } 
    # plot.DF
    plot.DF <- rbind(NA, plot.DF)
    is.summary <- c(TRUE, is.summary)
    
    ## set up title and subtitle
    if (!is.null(df$subtitle)) {
      subtitle <- c(df$subtitle, rep(NA, ncol(plot.matrix) - 1))
      if (missing(hgap)) {
        plot.matrix <- rbind(subtitle = subtitle, gap = "",  plot.matrix)
        plot.DF <- rbind(NA, NA, plot.DF)
        is.summary <- c(TRUE, FALSE, is.summary)
      }
      else {
        plot.matrix <- rbind(subtitle = subtitle, plot.matrix)
        plot.DF <- rbind(NA, plot.DF)
        is.summary <- c(TRUE, is.summary)  
      }    
    }
    
    if (!is.null(df$title)) {
      title <- c(df$title, rep(NA, ncol(plot.matrix) - 1))
      if (missing(hgap) && is.null(df$subtitle)) {
        plot.matrix <- rbind(title = title, gap = "",  plot.matrix)
        plot.DF <- rbind(NA, NA, plot.DF)
        is.summary <- c(TRUE, FALSE, is.summary)  
      }
      else {
        plot.matrix <- rbind(title = title,  plot.matrix)
        plot.DF <- rbind(NA, plot.DF)
        is.summary <- c(TRUE, is.summary)  
      }
    }   
    
    ## insert gaps by users specification
    if (!missing(hgap)) {
      for (i in 1:length(hgap)) {
        plot.matrix <- rbind(plot.matrix[1:(hgap[i] - 1), , drop = FALSE], gap = "", 
                             plot.matrix[hgap[i]:nrow(plot.matrix), , drop = FALSE])
        plot.DF <- rbind(plot.DF[1:(hgap[i] - 1), , drop = FALSE], NA,
                         plot.DF[hgap[i]:nrow(plot.DF), , drop = FALSE])
        is.summary <- c(is.summary[1:(hgap[i] - 1)], FALSE, 
                        is.summary[hgap[i]:length(is.summary)])
      }
    }
    if (!missing(vgap)) {
      for (i in 1:length(vgap)) {
        plot.matrix <- cbind(plot.matrix[, 1:(vgap[i] - 1), drop = FALSE], gap = "",
                             plot.matrix[, vgap[i]:ncol(plot.matrix), drop = FALSE])
      }
    }
    plot.DF <- cbind(plot.DF, is.summary)
    output <- list(matrix = plot.matrix, plotDF = plot.DF)
    class(output) <- c("metacontM", "metaM")
    output
  }
  else {
    ## generate main DF
    groupDF <- df$Group
    groupMetaMatrix <- lapply(groupDF, matrixify, order = order, newCols = newCols, 
                              roundCols = roundCols, stats = stats, newLabel = newLabel, 
                              hgap = hgap, overallSum = FALSE)
    
    overall.sum <- matrixify(df = df, order = order, newCols = newCols,
                             roundCols = roundCols, stats = stats, newLabel = newLabel, 
                             hgap = hgap, overallSum = TRUE) 
   
    groupMetaMatrix[[length(groupMetaMatrix) + 1]] <- overall.sum
    
    
    if (!missing(groupLab)) {
      if (!(length(groupLab) == length(groupDF)))
        stop("number of groups does not match the number of group labels")
      groupMetaMatrix <-  mapply(setGroupLabel, groupMetaMatrix, groupLab)
      group.plot.matrix <- groupMetaMatrix[1, ]
      group.plot.DF <- groupMetaMatrix[2, ]
      group.is.summary <- groupMetaMatrix[3, ]
    }
    else {
      group.plot.matrix <- lapply(groupMetaMatrix, get, x = "matrix")
      group.plot.DF <- lapply(groupMetaMatrix, get, x = "plot.DF")
      group.is.summary <- lapply(groupMetaMatrix, get, x = "is.summary")
    }
    
    plot.matrix <- NULL
    plot.DF <- NULL
    is.summary <- NULL
    for (i in 1:length(groupMetaMatrix)) {
      plot.matrix <- rbind(plot.matrix, group.plot.matrix[[i]])
      plot.DF <- rbind(plot.DF, group.plot.DF[[i]])
      is.summary <- c(is.summary, group.is.summary[[i]])
    }
    
    ## set up title and subtitle
    if (!is.null(df$subtitle)) {
      subtitle <- c(df$subtitle, rep(NA, ncol(plot.matrix) - 1))
      if (missing(hgap)) {
        plot.matrix <- rbind(subtitle = subtitle, gap = "",  plot.matrix)
        plot.DF <- rbind(NA, NA, plot.DF)
        is.summary <- c(TRUE, FALSE, is.summary)
      }
      else {
        plot.matrix <- rbind(subtitle = subtitle, plot.matrix)
        plot.DF <- rbind(NA, plot.DF)
        is.summary <- c(TRUE, is.summary)  
      }    
    }
    
    if (!is.null(df$title)) {
      title <- c(df$title, rep(NA, ncol(plot.matrix) - 1))
      if (missing(hgap) && is.null(df$subtitle)) {
        plot.matrix <- rbind(title = title, gap = "",  plot.matrix)
        plot.DF <- rbind(NA, NA, plot.DF)
        is.summary <- c(TRUE, FALSE, is.summary)  
      }
      else {
        plot.matrix <- rbind(title = title,  plot.matrix)
        plot.DF <- rbind(NA, plot.DF)
        is.summary <- c(TRUE, is.summary)  
      }
    }   
    
    ## insert gaps by users specification
    if (!missing(hgap)) {
      for (i in 1:length(hgap)) {
        plot.matrix <- rbind(plot.matrix[1:(hgap[i] - 1), , drop = FALSE], gap = "", 
                             plot.matrix[hgap[i]:nrow(plot.matrix), , drop = FALSE])
        plot.DF <- rbind(plot.DF[1:(hgap[i] - 1), , drop = FALSE], NA,
                         plot.DF[hgap[i]:nrow(plot.DF), , drop = FALSE])
        is.summary <- c(is.summary[1:(hgap[i] - 1)], FALSE, 
                        is.summary[hgap[i]:length(is.summary)])
      }
    }
    if (!missing(vgap)) {
      for (i in 1:length(vgap)) {
        plot.matrix <- cbind(plot.matrix[, 1:(vgap[i] - 1), drop = FALSE], gap = "",
                             plot.matrix[, vgap[i]:ncol(plot.matrix), drop = FALSE])
      }
    }
    plot.DF <- cbind(plot.DF, is.summary)
    output <- list(matrix = plot.matrix, plotDF = plot.DF)
    class(output) <- c("metacontM", "metaM")
    output    
  }
}