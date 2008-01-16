\name{CoHyperGParams-class}
\docType{class}
\alias{CoHyperGParams-class}
\alias{conditional}
\alias{conditional<-}
\alias{geneIds<-}
\alias{isConditional}
\alias{ontology}
\alias{ontology<-}
\alias{pvalueCutoff<-}
\alias{testDirection<-}
\alias{universeGeneIds}
\alias{HyperGParams-class}
\alias{geneIds,HyperGParams-method}
\alias{geneIds<-,HyperGParams,logical-method}
\alias{geneIds<-,HyperGParams-method}
\alias{annotation,HyperGParams-method}
\alias{conditional,HyperGParams-method}
\alias{ontology,HyperGParams-method}
\alias{pvalueCutoff,HyperGParams-method}
\alias{pvalueCutoff<-,HyperGParams-method}
\alias{testDirection,HyperGParams-method}
\alias{testDirection<-,HyperGParams-method}
\alias{universeGeneIds,HyperGParams-method}
\alias{universeBuilder,CoHyperGParams-method}
\alias{hyperGTest,CoHyperGParams-method}
\alias{isConditional,HyperGParams-method}
\alias{categoryName}
\alias{categoryName,HyperGParams-method}

\title{Class "CoHyperGParams"}

\description{
  A parameter class for representing all parameters needed for running
  the \code{hyperGTest} method with multiprotein complexes.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CoHyperGParams",\dots)}.
}
\section{Slots}{
  \describe{
    \item{\code{geneIds}:}{Object of class \code{"ANY"}: A vector of
      gene identifiers.  Numeric and character vectors are probably the
      only things that make sense.  These are the gene ids for the
      selected gene set.}
    \item{\code{universeGeneIds}:}{Object of class \code{"ANY"}: A
      vector of gene ids in the same format as \code{geneIds} defining a
      subset of the gene ids on the chip that will be used as the
      universe for the hypergeometric calculation.  If this is
      \code{NULL} or has length zero, then all gene ids on the chip will
      be used.}
    \item{\code{annotation}:}{A string giving the name of the
      annotation data package for the chip used to generate the data.}
    \item{\code{cateogrySubsetIds}:}{Object of class \code{"ANY"}:
      If the test method supports it, can be used to specify a subset of
      category ids to include in the test instead of all possible
      category ids.}
    \item{\code{categoryName}:}{A string describing the category.
      Usually set automatically by subclasses.  For example "ScISI".}
    \item{\code{pvalueCutoff}:}{The p-value to use as a cutoff for
        significance for testing methods that require it.  This value
        will also be passed on to the result instance and used for
        display and counting of significant results.  The default is
        0.01.}
      \item{\code{testDirection}:}{A string indicating whether the
          test should be for overrepresentation (\code{"over"}) or
          underrepresentation (\code{"under"}).}
  }
}
\section{Methods}{
  \describe{
  \item{hyperGTest}{\code{signature(p =
        "HyperGParams")}: Perform hypergeometric tests to
      assess over-representation of category ids in the gene set.  See the
      documentation for the generic function for details.  This method
      must be called with a proper subclass of
      \code{HyperGParams}.}
    
  \item{\code{geneIds(r)}, \code{geneIds(r) <- value}}{Accessors for
      the gene identifiers that will be used as the selected gene
      list.}
    
  \item{code{annotation(object)}}{Accessor for annotation}
    
  \item{\code{ontology(r)}}{Accessor for GO ontology.}
    
  \item{\code{pvalueCutoff(r)}, \code{pvalueCutoff(r) <-
        value}}{Accessor for the p-value cutoff.  When setting,
      \code{value} should be a numeric value between zero and one.}

  \item{\code{testDirection}}{Accessor for the test direction.  When setting,
        \code{value} must be either "over" or "under".}

    \item{\code{universeGeneIds(r)}}{accessor for vector of gene
        identifiers.}

    \item{\code{isConditional(r)}}{Returns \code{TRUE} if the instance
      has its conditional flag set}

    
  }
}

\author{S. Falcon and N. LeMeur}

\seealso{
  \code{\link[Category]{HyperGResult-class}}
  \code{\link[PCpheno]{CoHyperGResult-class}}
   \code{\link[Category]{hyperGTest}}
}

\keyword{classes}
