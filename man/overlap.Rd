\name{overlap}
\alias{overlap}

\title{Count the number of proteins shared by protein complexes}
\description{
  Count the number of proteins shared by protein complexes
}
\usage{
overlap(interactome)
}

\arguments{
  \item{interactome}{Binary matrix composed of genes (rows) and biological
    complexes (columns) (see package \emph{ScISI})}
}

\value{
  The returned value is a data frame with components:
  \item{C1}{Name of the first biological complex}
  \item{C2}{Name of the second biological complex}
  \item{nbSharedProt}{Number of proteins in common}
 
}

\author{N. LeMeur}

\seealso{\emph{ScISI}}
\examples{
xx = cbind("a"=c(0,1,1,1),"b"=c(1,1,0,1))
overlap(xx)
}
\keyword{data}
\keyword{manip}
