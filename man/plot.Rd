\name{plot}
\docType{methods}
\alias{plot}
\alias{plot,deResult-method}
\alias{plot,gtResult-method}
\alias{plot,deResult,missing-method}
\alias{plot,gtResult,missing-method}

\title{Graphical method to represent the result of the density or graph test.}
\description{
  a plot method for \code{deResult} and \code{gtResult} objects.
}
\usage{
\S4method{plot}{deResult}(x,...)
\S4method{plot}{gtResult}(x,...)
}

\arguments{
  \item{x}{the \code{deResult} or \code{gtResult} object to plot.}
  \item{...}{general commands to be sent to plot.}
}

\details{
  The plot generated from a \code{deResult} object is a set of density plots.

  The plot generated from a \code{gtResult} object is a histogram.
 
}

\author{N. LeMeur}

\seealso{\emph{ScISI}}
\examples{
data(ScISI)
data(essglist)
essential <- names(essglist)
ScISI <- as.matrix(ScISI)
ratio<- densityEstimate(genename=essential,interactome=ScISI,perm=50)
plot(ratio)
}
\keyword{data}
\keyword{manip}