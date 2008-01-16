\name{graphTheory}
\alias{graphTheory}

\title{Graph theory to test associations between two or more relationships}
\description{
 Graph theory approach associated with a permutation test to evaluate
 whether the number of associations is unexpectedly large.}
\usage{
graphTheory(genename, interactome, perm)
}

\arguments{
  \item{genename}{A vector a gene names that are associated with a
    particular phenotype}
  \item{interactome}{A binary matrix composed of genes (rows) and biological
    complexes (columns) (see package \emph{ScISI})}
  \item{perm}{Numeric, number of permutation run}
}
\details{
   We form two distinct graphs where the set of nodes are the proteins
(genes) in the organism. In one graph we create edges between genes if the two genes are
members of one, or more, protein complexes. In the second graph we
create an edge between all genes that are associated to a particular phenotype.
 We then construct a third graph on the same node set, but
where there is an edge in this graph only if there is an edge in both
of the first to graphs. We count the number of edges
in the third and test by permutation whether the number of edges is unexpectedly large.
}
\value{
  The returned value is a list with components:
  \item{edgeCount}{Number of associations observed between the genes
    that are linked to a particular phenotype and the given interactome.}
  \item{edgeSimul}{Number of associations if the genes
    that are linked to a particular phenotype are randomly distributed
    across the given interactome}
  \item{p.value}{Returned p.value}
}

\references{Balasubramanian, R., LaFramboise, T., Scholtens, D.,
  Gentleman, R. (2004) A graph-theoretic approach to testing
  associations between disparate sources of functional genomics data.Bioinformatics,20(18),3353-3362.}

\author{R. Gentleman and N. LeMeur}

\examples{
data(ScISI)
data(essglist)
ans <- graphTheory(names(essglist), ScISI, perm=3)
}
\keyword{data}
\keyword{manip}
