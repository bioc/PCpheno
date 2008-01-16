\name{SGDphenoL}
\alias{SGDphenoL}
\docType{data}
\title{Saccharomyces Genome Database list of phenotypic data}
\description{
  Saccharomyces Genome Database list of phenotypes
  and associated genes from several published experiments (last update 2006).
}
\usage{
data(SGDphenoL)
}
\format{
  \code{SGDphenoL} is a list of phenotypes. Under each
  phenotype is listed the genes that potentially induce that
  phenotype. A binary matrix can be built from that list using the
  \code{list2Matrix} function from the \code{Rintact} package.    
 }
\source{
  SGD, supplementary information:
  \url{http://www.yeastgenome.org/}
}

\author{N. LeMeur}

\examples{
data(SGDphenoL)
}
\keyword{datasets}

