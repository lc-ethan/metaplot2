path <- "/Users/cenanningli/GoogleDrive/Stats Summer Scholarship/gitHub/metaplot2/R"
metaplotfunction(path)
##==================metabin=================##
library(meta)
data(Olkin95)
## simple example
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95, 
                 subset=c(41,47,51,59), sm="RR", method="I")                                

Data1 <- meta2DF(meta1, title = "Thrombolytic Therapy" , subtitle = "Olkin I (1995)",
                 rowOrder = "effect", decreasing = TRUE)

matrix1 <- metaDF2Matrix(Data1)

drawMeta(matrix1)

## remove stats
matrix1 <- metaDF2Matrix(Data1, stats = NULL)

drawMeta(matrix1)

## customised columns
matrix1 <- metaDF2Matrix(Data1, order = c("study", "effect", "ci", "weight"), 
                         newCols = list(weight = makeColDesc(format = "%.0f%%", 
                                                             colNames = "w.fixed")))

drawMeta(matrix1, plotCol = 3)

## customised stats
matrix1 <- metaDF2Matrix(Data1,
                         newLabel = list(H.ci = makeLabelDesc(format = "H [95%% CI] = %.2f[%.2f, %.2f]", 
                                                              heteroNames = c("H", "H.lower", "H.upper"))),
                         stats = list(makeStatsDesc(labelNames = c("I2", "I2.ci", "tau2")),
                                      makeStatsDesc(labelNames = c("Q", "p", "df"), emptyHeading = TRUE),
                                      makeStatsDesc(labelNames = c("H.ci"), heading = "Hetero: "),
                                      makeStatsDesc(labelNames = c("I2.ci"), newStatsFormat = "%s is CI for I-Squared"),
                                      makeLabelDesc(format = "Test for Overall Effect: Q = %.f", heteroNames = "Q.CMH"),
                                      "Text can also be included in this part.", 1:10))
drawMeta(matrix1)

## rounding
matrix1 <- metaDF2Matrix(Data1, order = c("study", "effect", "w.fixed", "w.random"), 
                         roundCols = c("effect" = 8, "w.fixed" = 4))

drawMeta(matrix1, plotCol = 3)

## drop rows
# by row names
matrix1 <- metaDF2Matrix(Data1, drop = "fixed", stats = NULL)

drawMeta(matrix1)

# by row
matrix1 <- metaDF2Matrix(Data1, drop = 5:6, stats = NULL)

drawMeta(matrix1)

## gap
matrix1 <- metaDF2Matrix(Data1, order = c("study", "effect"), hgap = c(5, 7), stats = NULL)

drawMeta(matrix1)

### group studies
## simple example
Olkin95.2 <- Olkin95[1:10, ]
meta2 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95.2, studlab = author,
                 sm = "RR", method = "I", byvar = c(1, 2, 1, 1, 2, 2, 2, 3, 3, 3),
                 bylab = "group")

Data2 <- meta2DF(meta2, title = "Thrombolytic Therapy", subtitle = "Group Studies for Olkin95", 
                 rowOrder = "effect", decreasing = TRUE) 

matrix2 <- metaDF2Matrix(Data2)

drawMeta(matrix2)

## group label
matrix2 <- metaDF2Matrix(Data2, groupLab = c("Group 1", "Group 2", "Group 3", "Overall"))

drawMeta(matrix2)

## group Stats
matrix2 <- metaDF2Matrix(Data2, groupLab = c("Group 1", "Group 2", "Group 3", "Overall"),
                         groupStats = list(hetero = makeStatsDesc(labelNames= "Q.CMH")))

drawMeta(matrix2)

## customised columns
matrix2 <- metaDF2Matrix(Data2, order = c("study", "effect", "ci", "w.fixed", "w.random"),
                         groupLab = c("Group 1", "Group 2", "Group 3", "Overall"),
                         groupStats = list(hetero = makeStatsDesc(labelNames= "Q.CMH")))

drawMeta(matrix2, plotCol = 3)
                 
##===========================metacont=============================##
data(Fleiss93cont)

## simple example
meta3 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, data=Fleiss93cont, 
                  sm="SMD", studlab = study)

Data3 <- meta2DF(meta3, title = "Mental Health Treatment", subtitle = "Fleiss 1993",
                 rowOrder = "effect", decreasing = TRUE) 
Data3

matrix3 <- metaDF2Matrix(Data3)

drawMeta(matrix3)

## grouped studies
meta4 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, data=Fleiss93cont, 
                  sm="SMD", byvar=c(1,2,1,1,2), bylab="group", studlab = study) 

Data4 <- meta2DF(meta4, title = "Mental Health Treatment", subtitle = "Fleiss 1993",
                 rowOrder = "effect", decreasing = TRUE)
Data4

matrix4 <- metaDF2Matrix(Data4)

drawMeta(matrix4)

                        