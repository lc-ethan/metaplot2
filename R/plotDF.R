##=============plotDF generating function================##
plotDF <- function(df, hetero, hgap) {
  # step 1: generate the plotting parameters for the main DF
  mainDF <- df$DF
  plot.main <- data.frame(mean = mainDF["mean"],
                          lower = mainDF["lower"],
                          upper = mainDF["upper"])
  
  # step 2: gap by default
  if (missing(hgap)) {
    plot.main[nrow(plot.main) + 1, ] <- rep(NA, ncol(plot.main)) 
  }
  
  # step 3: generating the plotting parameters for the summary
  summary <- rbind(df$summary.fixed, df$summary.random)
  plot.sum <- data.frame(mean = summary["mean"],
                         lower = summary["lower"],
                         upper = summary["upper"])
  
  # step 4: gap by default
  if (missing(hgap)) {
    plot.sum[nrow(plot.sum) + 1, ] <- rep(NA, ncol(plot.sum)) 
  }
  
  # step 5: generating the plotting parameters for the hetero
  plot.hetero <- data.frame(mean = rep(NA, nrow(hetero)),
                            lower = rep(NA, nrow(hetero)),
                            upper = rep(NA, nrow(hetero)))
  
  # step 6: combination
  plot.DF <- rbind(plot.main, plot.sum, plot.hetero)
  
  # step 7: set up is.summary for formatting
  is.summary <- c(rep(FALSE, nrow(plot.main)), 
                  rep(TRUE, nrow(summary)), 
                  rep(FALSE, ifelse(missing(hgap), 1, 0) + nrow(plot.hetero)))
  
  list(plot.DF = plot.DF, is.summary = is.summary)
} 