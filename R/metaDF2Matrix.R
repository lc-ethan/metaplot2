#### set up the generic function
metaDF2Matrix <- function(object, ...) UseMethod("metaDF2Matrix")

##===================metacont=========================##
metaDF2Matrix.metacontDF <- function(df, order, roundCols = NULL, 
                                     colNames, groupLab, hgap, newCols = NULL, ...) {
  
  if (!inherits(df, "groupedMetaDF")){
    
    ### check order argument
    checkOrder(df = df, order = order, newCols = newCols)
    
    ### main DF
    ## step 1: extract the main data frame
    DF <- df$DF
    
    ## step 2: add CI or MSD into main DF if required
    DF <- addCIMSD(df = DF, order = order, newCols = newCols,
                   isSummary = FALSE)
    
    ## step 3: add columns with specific format into main DF
    DF <- addCols(df = DF, newCols = newCols, isSummary = FALSE)

    ## step 4: rounding columns in main DF
    rounding.DF <- roundingCols(df = DF, newCols = newCols,
                                roundCols = roundCols, isSummary = FALSE)
    
    round.DF <- rounding.DF[[1]]
    round.cols <- rounding.DF[[2]]
    
    ## step 5: convert the adjusted DF to matrix
    matrix.DF <- as.matrix(round.DF)
    
    ## step 6: set up gap between main DF and summary
    if (missing(hgap)) {
      matrix.DF <- rbind(matrix.DF, gap = rep("", ncol(matrix.DF)))
    }
  
    ### summary
    ## step 1: extract the fixed and the random summary
    summary <- list(fixed = df$summary.fixed, random = df$summary.random)
    
    ## step 2: add CI or MSD into summary if required
    summary <- lapply(summary, addCIMSD, order = order, newCols = newCols,
                      isSummary = TRUE)
    
    ## step 3: add columns with specific format into summary
    summary <- lapply(summary, addCols, newCols = newCols, isSummary = TRUE)
    
    ## step 4: rounding columns in summary
    summary <- lapply(summary, roundingCols, newCols = newCols,
                      roundCols = roundCols, isSummary = TRUE)
    
    round.fixed <- summary$fixed$df
    round.random <- summary$random$df
    
    ## step 5: combine the summary effects
    round.sum <- rbind(round.fixed, round.random)
    
    ## step 6: convert the adjusted summary into matrix
    matrix.sum <- as.matrix(round.sum)
    
    ### set up the main matrix
    matrix.total <- rbind(matrix.DF, matrix.sum)
    
    ### plotDF
    ## step 1: generate the plotting parameters for the main DF
    plot.DF <- data.frame(mean = round.DF["mean"],
                          lower = round.DF["lower"],
                          upper = round.DF["upper"])
    
    ## step 2: gap
    if (missing(hgap)) {
      plot.DF[nrow(plot.DF) + 1, ] <- rep(NA, ncol(plot.DF))
    }
    
    ## step 3: generate the plotting parameters for the summary
    plot.sum <- data.frame(mean = round.sum["mean"],
                           lower = round.sum["lower"],
                           upper = round.sum["upper"])
    
    ## step 4: combine two sets of plotting parameters
    PlotDF <- rbind(plot.DF, plot.sum)
    
    ## step 5: set up is.summary for formatting
    is.summary <- c(rep(FALSE, nrow(round.DF) + ifelse(missing(hgap), 1, 0)), 
                    rep(TRUE, nrow(round.sum)))
    
    ### hetero information
  
    
    
   
    
    
                          
                          
                      
    
    
  }  
}