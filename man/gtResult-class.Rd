\name{gtResult-class}
\docType{class}
\alias{gtResult-class}

\title{A class for representing the result of a graphTheory test.}
\description{A class for representing the result of a \code{graphTheory} test.}

\section{Slots}{
	 \describe{
	   \item{\code{Pvalue}:}{Object of class \code{"numeric"}}
	   \item{\code{Observed}:}{Return a \code{"numeric"}
	     vector: the observed  number of interactions between genes inducing a specific phenotype and each
	     cellular organizational units}
	   \item{\code{Expected}:}{Return a matrix: the expected
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
  \code{\link[PCpheno]{testResult}},\code{\link[PCpheno]{deResult}},\code{\link[PCpheno]{plot}}
}
\examples{
## apply a densityEstimate test 
data(DudleyPhenoM)
data(ScISIC)

DudleyPhenoL <- apply(DudleyPhenoM,2,function(x) names(which(x==1)))
NaCl <- DudleyPhenoL[["NaCl"]]

perm <- 20 
NaClGraph <- graphTheory(genename=NaCl, interactome=ScISIC,
perm=perm)
 

## access results
slotNames(NaClGraph)
NaClGraph@Pvalue[1:5]

## use of the plot method
plot(NaClGraph)
}
\keyword{classes}
