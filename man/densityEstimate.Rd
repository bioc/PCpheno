\name{densityEstimate}
\alias{densityEstimate}

\title{Observed versus Expected Ratios}
\description{
  Function to calculate the ratio of genes that characterize a phenotype
  (observed) among the genes that characterize a biological complex
  versus the ratio of a set of randomly sampled genes (expected) among
  the genes that characterize a biological complex.
  }
\usage{
densityEstimate(genename,interactome,perm)
}
\arguments{
  \item{genename}{Character vector of the gene names that characterize
    a specific phenotype.}
  \item{interactome}{Contingency matrix of genes (rows) and biological
    complexes (columns) (see package \emph{ScISI}) }
  \item{perm}{Numeric vector indicating the number of simulations to
    run to compute the expected ratios.}
}

\value{
  List of observed and simulated ratios. 
}
\author{N. LeMeur}

\examples{
 data(ScISI)
 data(essglist)
 essential <- names(essglist)
 ScISI <- as.matrix(ScISI)
  ratio<- densityEstimate(genename=essential,interactome=ScISI,perm=50)
}
\keyword{data}
\keyword{manip}
