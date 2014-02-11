## set up generic function for forestDF
forestDF <- function(object, ...) UseMethod("forestDF")

##===================meta=======================##
# metabin
forestDF.metabin <- function(meta, study, n.e, event.e, 
                             n.c, event.c, effect, se, 
                             w.fixed, w.random, mean, lower, upper,
                             e.lower, e.upper, summary = FALSE){
  
  DF <- data.frame(study = study, n.e = n.e, event.e = event.e,
                   n.c = n.c, event.c = event.c, effect = effect,
                   se = se, w.fixed = w.fixed, w.random = w.random,
                   mean = mean, lower = lower, upper = upper,
                   e.lower = e.lower, e.upper = e.upper)
  
  if (!is.null(meta$byvar)) {
    if (summary == FALSE) DF <- cbind(DF, group = meta$byvar)
    else DF <- cbind(DF, group = "")     
  } 
  DF
}

# metacont
forestDF.metacont <- function(meta, study, n.e, mean.e, sd.e, n.c, sd.c, 
                              effect, se, w.fixed, w.random, mean, 
                              lower, upper, summary = FALSE){  
  
  DF <-  data.frame(study = study, n.e = n.e, mean.e = mean.e, sd.e = sd.e,
                    n.c = n.c, mean.c = mean.c, sd.c = sd.c, effect = effect,
                    se = se, w.fixed = w.fixed, w.random = w.random,
                    mean = mean, lower = lower, upper = upper)
  
  if (!is.null(meta$byvar)) {
    if (summary == FALSE) DF <- cbind(DF, group = meta$byvar)
    else DF <- cbind(DF, group = "")   
  }
  DF
}

##=========================rmeta========================##
# meta.MH
forestDF.meta.MH <- function(object, study, rate, lower, upper) {
    DF <- data.frame(study = study, n.e = NA, event.e = NA, n.c = NA,
                     event.c = NA, effect = NA, se = NA, w.fixed = NA,
                     w.random = NA, mean = rate, lower = lower, 
                     upper = upper, e.lower = NA, e.upper = NA)
  rownames(DF) <- 1:nrow(DF)
  DF
}

# meta.DSL
forestDF.meta.DSL <- function(object, study, rate, lower, upper) {
    DF <- data.frame(study = study, n.e = NA, event.e = NA, n.c = NA,
                     event.c = NA, effect = NA, se = NA, w.fixed = NA,
                     w.random = NA, mean.OR = rate, lower = lower, 
                     upper = upper, e.lower = NA, e.upper = NA)

  rownames(DF) <- 1:nrow(DF)
  DF
}
