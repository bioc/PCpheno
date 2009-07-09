\name{CoHyperGResult-class}
\docType{class}
\alias{CoHyperGResult-class}
\alias{summary,CoHyperGResult-method}
\alias{geneCounts,CoHyperGResult-method}
\alias{pvalues,CoHyperGResult-method}
\alias{oddsRatios,CoHyperGResult-method}
\alias{universeCounts,CoHyperGResult-method}
\alias{expectedCounts,CoHyperGResult-method}

\title{Class "CoHyperGResult"}

\description{	

  This class represents the results of a test for over-representation
  of genes in a selected gene set based among protein complexes upon the  Hypergeometric distribution.

}

\section{Objects from the Class}{
Objects is created by calls to the funtion hyperGTest.
}

\section{Slots}{
	 \describe{
    \item{\code{pvalues}:}{\code{"numeric"} vector: the ordered
      p-values for each category term tested.}
    \item{\code{oddsRatios}:}{Object of class \code{"numeric"}
          Odds ratio for each category term tested}
     \item{\code{expectedCounts}:}{Object of class \code{"numeric"}
       The expected number of genes for each  gene term tested}
     \item{\code{geneCounts}:}{\code{"integer"} vector: for each
      category term tested, the number of genes from the gene set that
      are annotated at the term.}
    \item{\code{universeCounts}:}{\code{"integer"} vector: for
      each category term tested, the number of genes from the gene
      universe that are annotated at the term.}
    \item{\code{catToGeneId}:}{Object of class \code{"list"}.  The
        names of the list are category IDs.  Each element is a vector
        of gene IDs annotated at the given category ID and in the
        specified gene universe.}

   }
}
\section{Extends}{
Class \code{"HyperGResultBase"}, directly.
}
\section{Methods}{
  \describe{
  \item{geneCounts}{\code{signature(r =
        "CoHyperGResult")}: return an \code{"numeric"}
      vector: for each category term tested, the number of genes from
      the gene set that are annotated at the term.}

    \item{pvalues}{\code{signature(r =
          "HyperGResult")}: return a \code{"numeric"}
        vector: the ordered p-values for each category term tested.}

    \item{oddsRatios}{\code{signature(r =
          "HyperGResult")}: return a \code{"numeric"}
        vector: the odds ratio for each category term tested.}

    \item{expectedCounts}{\code{signature(r =
          "HyperGResult")}: return a \code{"numeric"}
        vector: the expected number of genes for each GO term tested.}

  \item{universeCounts}{\code{signature(r =
        "HyperGResult")}: return an \code{"numeric"}
      vector: for each category term tested, the number of genes from
      the gene universe that are annotated at the term.}
 
  \item{geneIdUniverse}{\code{signature(r =
        "CoHyperGResult")}: return a list named by the
      protein Complexes.  Each element of the list is a vector of gene
      identifiers (from the gene universe) annotated at the
      corresponding protein complex.}
    
  \item{summary}{\code{signature(r = "CoHyperGResult")}:
      Returns a \code{data.frame} summarizing the test result.
      Optional arguments \code{pvalue} and \code{categorySize} allow
      specification of minimum p-value and cateogyrSize, respectively.
      Optional argument \code{htmlLinks} is a logical value indicating
      whether to add HTML links (useful in conjunction with xtables
      print method with \code{type} set to \code{"html"}).}


  }
}

\author{S. Falcon and N. LeMeur}

\seealso{
  \code{\link[category]{HyperGResultBase-class}}
}
\examples{
data(DudleyPhenoM)
data(ScISIC)

## Select genes sensitive to paraquat
DudleyPhenoL <- apply(DudleyPhenoM,2,function(x) names(which(x==1)))
paraquat <- DudleyPhenoL[["Paraq"]]

## Apply a hypergeometric test
params <- new("CoHyperGParams",
              geneIds=paraquat, 
              universeGeneIds=rownames(ScISIC),
              annotation="org.Sc.sgd.db",
              categoryName="ScISIC",
              pvalueCutoff=0.01,
              testDirection="over")

paraquat.complex <- hyperGTest(params)

## access the p-values
pvalues(paraquat.complex)[1:5]

## Display a summary of the results
summary(paraquat.complex)[,1:4]
}
\keyword{classes}
