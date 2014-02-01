##=========================metaDF2Matrix=========================##
metaDF2Matrix <- function(object, ...) UseMethod("metaDF2Matrix")

##==========================metacont=============================##
metaDF2Matrix.metacontDF <- function(df, order, newCols = NULL, roundCols = NULL,
                                     stats = list(hetero = makeStatsDesc(labelNames = c("Q", "p", "df"))), 
                                     newLabel = NULL, colNames, groupLab, hgap, vgap, ...) {
  if (!inherits(df, "groupedMetaDF")){
    matrixify(df = df, order = order, newCols = newCols, roundCols = roundCols,
              stats = stats, newLabel = newLabel, colNames = colNames, hgap = hgap, vgap = vgap, ...)   
  }

}