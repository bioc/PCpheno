\name{deResult-class}
\docType{class}
\alias{deResult-class}

\title{A class for representing the result of a densityEstimate test.}
\description{A class for representing the result of a \code{densityEstimate} test.}

\section{Slots}{
  \describe{
    \item{\code{Size}:}{Object of class \code{"numeric"} representing
      the size of the cellular organizational unit tested}
    \item{\code{Observed}:}{Return a \code{"numeric"}
      vector: the observed  number of interactions between genes inducing a specific phenotype and each
      cellular organizational units}
    \item{\code{Expected}:}{Return a  matrix: the expected
      number of interactions between genes inducing a specific phenotype and each
      cellular organizational units}
  }
}
\section{Extends}{
Class \code{"\linkS4class{testResult}"}, directly.
}
\section{Methods}{
 \describe{
    \item{\code{plot}}{Graphical representation of the test result}
  }
}

\author{N. LeMeur}

\seealso{
  \code{\link[PCpheno:testResult-class]{testResult}},\code{\link[PCpheno:gtResult-class]{gtResult}},\code{\link[PCpheno]{densityEstimate}}, \code{\link[PCpheno]{plot}}
}
\examples{
## apply a densityEstimate test 
data( DudleyPhenoM)
data(ScISIC)

DudleyPhenoL <- apply(DudleyPhenoM,2,function(x) names(which(x==1)))
pH3 <- DudleyPhenoL[["pH3"]]

perm <- 20 
pH3Density <- densityEstimate(genename=pH3, interactome=ScISIC, perm=perm) 

## access results
pH3Density@Observed[1:5]

## use of the plot method
plot(pH3Density)
}
\keyword{classes}
