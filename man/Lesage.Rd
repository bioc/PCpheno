\name{LesageRaw}
\alias{LesageRaw}
\docType{data}
\title{Data from Lesage et al. 2005}

\description{
  Lesage et al. (2005) assembled a network of 316 interactions among 163
genes using deletion mutants in CHS1, CHS3, CHS4, CHS5, CHS6, CHS7 and
BNI4 in a synthetic genetic array analysis.
 }
\usage{
data(LesageRaw)
}
\format{
   \code{LesageRaw} is a 5 column dataframe.
  
  \describe{ 
    \item{SYSTEMATIC}{Systematic gene names. NOTE: All mutants are isogenic to BY4741 (MATa his3\u0394 leu2\u0394 met15\u0394 ura3\u0394) except anp1\u0394 and mnn9\u0394 that are isogenic to BY4742 (MAT\u03b1 his3\u0394 leu2\u0394 lys2\u0394 ura3\u0394).}
    \item{COMMUN}{Commun gene names.}
    \item{CFW}{Mutants showing increased, decreased or wild type sensitivity to Calcofluor white are scored s, r, or wt, respectively.}
    \item{ChitinLevel}{Chitin level (nmole GlcNAc/mg dry weight).
      Values are an average of at least three
      independent determinations. Values
      statistically higher and lower than wild type (p < 0.01) are
      highlighted in red and green, respectively.}
    \item{ChitinLevel.SD}{Standard deviation of the average of at least three
      independent determinations of Chitin level.}
  }
  
  }
\source{
  Lesage et al. (2005), supplementary information:
  \url{http://www.biomedcentral.com/1471-2156/6/8/suppl/S2}
  or  \url{ftp://genome-ftp.stanford.edu/pub/yeast/systematic_results/phenotypes}
}
\references{
Lesage G, Shapiro J, Specht CA, Sdicu AM, Menard P, Hussein S, Tong AH, Boone C,
Bussey H. (2005) An interactional network of genes involved in chitin synthesis in Saccharomyces
cerevisiae.BMC Genet.6(1):8. PMID: 15715908
}
\author{N. LeMeur}

\examples{
data(LesageRaw)
str(LesageRaw)
}
\keyword{datasets}

