\name{categoryToEntrezBuilder}

\alias{categoryToEntrezBuilder}
\alias{categoryToEntrezBuilder,CoHyperGParams-method}

\title{Return a list mapping multi-protein complexes IDs to YEAST ids}
\description{
  Return a list mapping  multi-protein complexes (category) IDs to the YEAST ids annotated at
  the category id.  
}
\usage{
\S4method{categoryToEntrezBuilder}{CoHyperGParams}(p)
}

\arguments{
  \item{p}{A subclass of \code{HyperGParams-class}}
}

\details{
  End users \bold{should not} call this directly.  This method gets
  called from \code{hyperGTest}.  To add support for a new
  category, a new method for this generic must be defined.  Its
  signature should match a subclass of
  \code{HyperGParams-class} appropriate for the new
  category.
}
\value{
  A list mapping category IDs to YEAST identifiers.
}

\author{S. Falcon and N. LeMeur}
\seealso{
  \code{\link[Category]{hyperGTest}}
  \code{\link{CoHyperGParams-class}}
}
\examples{
data(ScISIC)
data(essglist)
essential <- names(essglist)

params <- new("CoHyperGParams",
              geneIds=essential, 
              universeGeneIds=rownames(ScISIC),
              annotation="YEAST",
              categoryName="ScISIC",
              pvalueCutoff=0.01,
              testDirection="over")

categoryToEntrezBuilder(params)[1:2]
}

\keyword{manip}
