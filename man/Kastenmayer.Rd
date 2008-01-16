\name{KastenmayerRaw}
\alias{KastenmayerRaw}
\docType{data}
\title{Data from Kastenmayer et al. 2006}

\description{
  Kastenmayer et al. (2006) undertook the first functional studies of
  small open reading frames (sORFs) in any system, using the model
eukaryote Saccharomyces cerevisiae. Phenotypic analyses of the new gene-deletion strains identified
22 sORFs required for haploid growth, growth at high temperature, growth in the
presence of a non-fermentable carbon source, or growth in the presence of DNA
damage and replication-arrest agents.   
 }
\usage{
data(KastenmayerRaw)
}
\format{
   \code{Kastenmayer} is a 5 columns dataframe.
  
  \describe{ 
    \item{SYSTEMATIC}{Systematic name of the sORF.}
    \item{COMMUN}{Commun name of the sORF.}
    \item{Length}{Length of the small ORF sequence in number of amino acids.}
    \item{Evidence}{Experimental source of the data}
    \item{refHomology}{Bibliographical evidence of reported homology.}
    \item{Kocollection}{Bibliographical evidence of reported homology.}
    \item{ESSENTIAL}{Indicates if the sORF knockout is essential.  A
      blank in this column indicates that the knockout is not-essential, if available.}
    \item{GFPTAP}{"GFP" or "TAP" signifies that
      sORF was detected by the indicated technique. "both" indicates that sORF was detected both as a
      TAP-tagged and GFP-tagged protein.  "None" indicates
      that sORF was not detected by either method. Empty field indicates that sORF was not tested}
    \item{UPTAG}{Sequence of the upstream primer.}
    \item{DOWNTAG}{Sequence of the downstream primer.}
  }
}
\source{
  Kastenmayer et al. (2006), supplementary information:
  \url{http://www.genomeresearch.org}
  or  \url{ftp://genome-ftp.stanford.edu/pub/yeast/systematic_results/phenotypes}
}
\references{
Kastenmayer JP, Ni L, Chu A, Kitchen LE, Au WC, Yang H, Carter CD, Wheeler D,
Davis RW, Boeke JD, Snyder MA, Basrai MA. (2006) Functional genomics of
genes with small open reading frames (sORFs) in S. cerevisiae. Genome
Res. 16(3):365-73. PMID: 16510898 
}
\author{N. LeMeur}

\examples{
data(KastenmayerRaw)
str(KastenmayerRaw)
}
\keyword{datasets}

