\name{complexStatus}
\alias{complexStatus}

\title{Complex Status}
\description{
 Categorize the complex whether or not a complex is composed of a
 significant number of genes involved in a particular
 phenotype than expected by chance.}
\usage{
complexStatus(data, phenotype, interactome, threshold=0.05)
}

\arguments{
  \item{data}{Output from CoHyperG test}
  \item{phenotype}{List of gene names inducing an observed phenotype,
    e.g., list of essential gene names (see package \emph{SLGI})}
  \item{interactome}{A binary matrix composed of genes (rows) and biological
    complexes (columns) (see package \emph{ScISI})}
  \item{threshold}{pvalue threshold (default 0.05)}
}
\details{
   We form four distinct categories from A to D to characterize how a
   complex might be involved in a particular phenotype (according to
   the number of genes it contains and that are involved in a particular
   phenotype - see also \link[PCpheno]{CoHyperG} function)
}
\value{
  The returned value is a list with components:
  \item{A}{"interesting" complexes, complexes with a significant number of interesting
 genes, i.e., genes that participate to a particular phenotype (at a given p-values threshold)}
  \item{B}{complexes with a NON significant number of interesting genes
    BUT that  SHARE genes with complexes from the A status}
  \item{C}{complexes with a NON significant number of interesting genes AND
that DON'T SHARE interesting genes with complexes from cat A}
  \item{D}{complexes WITHOUT interesting genes, i.e. the one involved in
  the studied phenotype}
}

\author{N. LeMeur}

\examples{
data(ScISI)
data(essglist)
essential <- names(essglist)

CoparamsESS <- new("CoHyperGParams",
                   geneIds=essential, 
                   universeGeneIds=rownames(ScISI),
                   annotation="org.Sc.sgd.db",
                   categoryName="ScISI",
                   pvalueCutoff=0.01,
                   testDirection="over")

sign<- hyperGTest(CoparamsESS)
test05 <-complexStatus(data=sign, phenotype=essential,
interactome=ScISI, threshold=0.05)
}
\keyword{data}
\keyword{manip}
