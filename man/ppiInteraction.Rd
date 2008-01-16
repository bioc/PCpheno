\name{ppiInteract}
\alias{ppiInteract}

\title{Test the association between AP-MS data and phenotype}
\description{
  Test the association between AP-MS data and phenotype data via a graph
  and permutation model.
}

\usage{
ppiInteract(genename, expGraph, bait, prey, perm=10)
}

\arguments{
  \item{genename}{Genes associated to a phenotype}
  \item{expGraph}{A graphNEL object (a direct graph instance of class\code{graph}).
    The nodes are the union of viable baits (VB) and viable
     prey (VP) of the experiment (see package \emph{ScISI})}
  \item{bait}{Proteins which was sampled as a bait in the binary
    relationship}
  \item{prey}{Proteins which was sampled as a prey in the binary
     relationship}
   \item{perm}{Number of permutation}
 }

\value{
  The returned value is a list:
  \item{Observed}{Observed values}
  \item{Expected}{Expected values after each permutation}
}

\author{R. Gentleman and N. LeMeur}

\seealso{\emph{ScISI}}
\examples{
data(ScISI)
data(essglist)
s1 <- ppiInteract(names(essglist), Gavin2002BPGraph, viableBaits[[8]],
       viablePrey[[8]], perm=10)
}
\keyword{data}
\keyword{manip}
