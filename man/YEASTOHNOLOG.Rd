\name{YEASTOHNOLOG}
\alias{YEASTOHNOLOG}
\docType{data}
\title{List of ohnolog gene pairs from Byrne, K.P and Wolfe, K.H (2005)}
\description{
  List of 551 paralogous \emph{Saccharomyces cerevisiae} gene pairs formed by Whole
  Genome Duplication (WGD) or ohnolog pairs.
 }
\usage{
data(YEASTOHNOLOG)
}
\format{
  \code{YEASTOHNOLOG} is a dataframe of 551 paired genes. The first two
  columns are the ohnolog gene pairs (systematic gene names). The third
  column is an index (numeric) of the  rate of sequence evolution. The
  last two columns define the chromosome location.
}
\details{
  \describe{
    \item{Gene1 Gene2}{Systematic gene names of the ohnolog pairs}
    \item{Ka}{Coefficient that represents the extent of non-synonymous sequence divergence between each
      ohnolog pairs (Yang and Nielsen, 2000). The highest is the coefficient
      the fastest the 2 elements of a pair have diverged.}
    \item{ChrG1 ChrG2}{Chromosome location of the each element of a
      pair. Note that repeat of the same chromosome locations shared by
      a set of pairs define a block of duplication.}
  }
}
\source{
  Byrne,KP and Wolfe KH (2005), Table2 of supplementary information and
  Scerevisiae\_genome.tab file, chromosome location, from YGOB \url{http://wolfe.gen.tcd.ie/ygob/} (last update 03/20/06)
}
\references{
  Byrne KP, Wolfe KH. (2005) The Yeast Gene Order Browser: combining curated homology and syntenic context
reveals gene fate in polyploid species. Genome Res. 15(10):1456-61. 
PMID: 16169922 
}

\author{N. LeMeur}

\examples{
data(YEASTOHNOLOG)
str(YEASTOHNOLOG)
}
\keyword{datasets}

