\name{getDescr}
\alias{getDescr}

\title{Get formatted annotation data}
\description{
  Function to retrieve the annotation of  multi-protein complexes or
  pathways via GO, MIPS or KEGG.
  }
\usage{
  getDescr(x, database="GO")
}
\arguments{
  \item{x}{Vector of multi-protein complexes or pathways IDs to be described}
  \item{database}{Source of annotation. The database currently available
are MIPS, GO and KEGG} }

\author{N. LeMeur}

\examples{
 xx <- getDescr(c("MIPS-220","MIPS-260.20","04111"),c("MIPS","KEGG"))
}

\keyword{manip}
