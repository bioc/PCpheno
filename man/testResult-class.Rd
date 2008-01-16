\name{testResult-class}
\docType{class}
\alias{testResult-class}

\title{A virtual class for representing the result of a test.}
\description{
  The \code{testResult} class is the virtual base class for all result
  objects of the densityEstimate and graphTheory tests proposed in \code{PCpheno}.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{Observed}:}{Return a \code{"numeric"}
      vector: the observed number of genes or interactions within each
      cellular organizational units}
    \item{\code{Expected}:}{Return a numeric or a matrix: the expected
      number of genes or interactions within each
      cellular organizational units}
  }
}
\section{Methods}{
No methods defined with class "testResult" in the signature.
}

\author{N. LeMeur}

\seealso{
  \code{\link[PCpheno]{gtResult}},\code{\link[PCpheno]{deResult}}
}
\keyword{classes}
