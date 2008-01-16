\name{KEGG2SCISI}
\alias{KEGG2SCISI}

\title{Mapping between KEGG and ScISI}
\description{
  Count the number of genes shared between a KEGG pathway and a protein
  complex from the ScISI interactome.
}
\usage{
KEGG2SCISI(pw, pc, pcMat, pwMat)
}

\arguments{
  \item{pw}{list of pathway names}
  \item{pc}{list of complex names}
  \item{pwMat}{pathway incidence matrix}
  \item{pcMat}{complex incidence matrix}
}

\value{
  matrix 
}

\author{N. LeMeur}
\seealso{
  \emph{ScISI} \emph{KEGG}
}
\examples{
data(ScISIC)
KeggMat <- PWAmat("YEAST")
KEGG2SCISI(pw = colnames(KeggMat)[1:5], pc = colnames(ScISIC)[1:5], pwMat =
KeggMat, pcMat =ScISIC)
}
\keyword{data}
\keyword{manip}
