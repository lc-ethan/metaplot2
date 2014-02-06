\name{metaDF2Matrix}
\alias{metaDF2Matrix}

\title{
Convert 'metaDF' Objects to Matrices
}
\description{
A generic function that turns \code{'metaDF'} objects into a character matrix 
used for plotting.
}
\usage{
metaDF2Matrix(df, ...)
}
\arguments{
  \item{df}{Either an object of class \code{'metabinDF'}, or an object of class 
           \code{'metacontDF'}. See 'Details'.}
  \item{\dots}{Other arguments.}
}
\details{
x can be any object from the generic function \code{meta2DF()}.
}
\seealso{
\code{\link{metaDF2Matrix.metabinDF}},
\code{\link{metaDF2Matrix.metacontDF}}
}
\examples{
library(meta)
# example for metabin
data(Olkin95)

meta1 <- metabin(event.e, n.e, event.c, n.c, data=Olkin95, 
                 subset = c(41,47,51,59), sm = "RR", method = "I")                                

Data1 <- meta2DF(meta1, title = "Thrombolytic Therapy" , 
                 rowOrder = "effect", decreasing = TRUE)
                
matrix1 <- metaDF2Matrix(df = Data1)

matrix1

# example for metacont
data(Fleiss93cont)

meta2 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, 
                  data = Fleiss93cont, sm = "SMD")

Data2 <- meta2DF(meta2, title = "Mental Health Treatment",
                 rowOrder = "effect", decreasing = TRUE) 

matrix2 <- metaDF2Matrix(Data2)
}
