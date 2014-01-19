require(metaplot)
require(meta)
require(rmeta)
##======meta2DF======##
## metabin
# Example from Edna's version
data(Olkin95)
meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95, 
                 subset=c(41,47,51,59), sm="RR", method="I",
                 studlab = author)
# testing 'add' argument
add <- list(test1 = c(1:4), test2 = c(5:8))          
Data1 <- meta2DF(meta1, title = "Thrombolytic Therapy" , 
                 rowOrder = "effect", decreasing = TRUE,
                 add = add)  
Data1       

## Example made up newly for testing the function of grouped studies
Olkin95.2 <- Olkin95[1:5, ]
meta2 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95.2, studlab = author,
                 sm = "RR", method = "I", byvar = c(1, 2, 1, 1, 2),
                 bylab = "group", studlab = author)

Data2 <- meta2DF(meta2, title = "Thrombolytic Therapy" , 
                 rowOrder = "effect", decreasing = TRUE) 

Data2 

## metacont
# Example from Edna's version
data(Fleiss93cont)
meta3 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, data=Fleiss93cont, 
                  sm="SMD", studlab = study)

Data3 <- meta2DF(meta3, title = "Mental Health Treatment",
                rowOrder = "effect", decreasing = TRUE) 
Data3

## Example for testing the function of grouped studies
meta4 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, data=Fleiss93cont, 
                  sm="SMD", byvar=c(1,2,1,1,2), bylab="group", studlab = study) 
Data4 <- meta2DF(meta4, title = "Mental Health Treatment",
                 rowOrder = "effect", decreasing = TRUE)
Data4

matrix <- metaDF2Matrix(Data4)

drawMeta(matrix)
