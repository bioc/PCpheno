\name{OsterbergRaw}
\alias{OsterbergRaw}
\docType{data}
\title{Data from Osterberg at al. 2006}

\description{
   Osterberg at al. (2006) report growth phenotypes in yeast for a strain
   collection over-expression ~600 C-terminal tagged integral membrane
   proteins growth both under normal and three different stress
   conditions. 
 }
\usage{
data(OsterbergRaw)
}
\format{
   \code{OsterbergRaw} is a 17 columns dataframe.
  
  \describe{ 
    \item{SYSTEMATIC}{Systematic gene names of the studied membrane protein}
    \item{COMMUN}{Commun gene names of the studied membrane protein}
    \item{TMHMM.C}{The topology predicted by TMHMM (TransMembrane
      prediction using Hidden Markov Models)
      using the experimentally assigned C-terminal location for the protein as a
      constraint. The topology is represented in the format Location of
      N-terminus TMhelices Location of C-terminus (i and o stand for in
    and out respectively)}
    \item{WesternBlot}{Protein expression levels (arbitrary units),
      estimated from the band intensity and normalized to the internal
      standard on each Western blot.}
    \item{Bands}{Proteins detected as two distinct bands with different
      molecular mass on the Western blot analysis. category 1 indicates
      that both bands were insensitive to Endo H digestion, 2 indicates
      the higher molecular mass was shifted down upon Endo H digestion
      on SDS/PAGE and one  band was predominant compared to the other,
      and 3 indicates that a higher molecular mass band shifted down
      upon Endo H digestion on SDS/PAGE and both bands were equal
      intensity on Western blot}
    \item{Toxicity}{Toxicity index from Spoko et al. (2006). The index
    varies between 1 and 5, where 1 means the strain is dead, and 5
    indicates no difference in growth rate compared with the wild type strain.}
    \item{sign.norm}{Over-expression strains that show a significant
      (P<0.001) growth rate phenotype (LSCrate) in synthetic defined
      medium conditions (Warringer et al., 2003). An average of two
      replicates is given. Strains that do not show a significant
      difference in doubling time compared with the wild-type strain are
      indicated by 0.}
    \item{all.norm}{Phenotypes (significant or not) of over-expression
      strains (LSCrate) in synthetic defined medium conditions
      (Warringer et al., 2003). An average of two replicates is given.}
    \item{sign.NaCl}{Over-expression strains that show a significant
      (P<0.001) growth rate phenotype (LPIrate) in NaCl. An average of two
      replicates is given. Strains that do not show a significant
      difference in doubling time compared with the wild-type strain
      under NACL stress are indicated by 0.}
    \item{all.NaCl.LSC}{Phenotypes (significant or not) of over-expression
      strains (LSCrate) in NaCl (Warringer et al., 2003).
      An average of two replicates is given.}
     \item{all.NaCl.LPI}{Phenotypes (significant or not) of over-expression
      strains (LPIrate) in NaCl (Warringer et al., 2003).
      An average of two replicates is given.}
    \item{sign.caff}{Over-expression strains that show a significant
      (P<0.001) growth rate phenotype (LPIrate) in caffeine. An average of two
      replicates is given. Strains that do not show a significant
      difference in doubling time compared with the wild-type strain
      under caffeine stress are indicated by 0.}
    \item{all.caff.LSC}{Phenotypes (significant or not) of over-expression
      strains (LSCrate) in caffeine (Warringer et al., 2003).
      An average of two replicates is given}
    \item{all.caff.LPI}{Phenotypes (significant or not) of over-expression
      strains (LPIrate) in caffeine (Warringer et al., 2003).
      An average of two replicates is given.}
    \item{sign.paraq.}{Over-expression strains that show a significant
      (P<0.001) growth rate phenotype (LPIrate) in paraquat. An average of two
      replicates is given. Strains that do not show a significant
      difference in doubling time compared with the wild-type strain
      under paraquat stress are indicated by 0.}
    \item{all.paraq.LSC}{Phenotypes (significant or not) of overexpression
      strains (LSCrate) in paraquat (Warringer et al., 2003).
      An average of two replicates is given.}
    \item{all.paraq.LPI}{Phenotypes (significant or not) of overexpression
      strains (LPIrate) in paraquat (Warringer et al., 2003).
      An average of two replicates is given.}
  }
}
\source{
  Osterberg et al (2006), supplementary information:
  \url{http://www.pnas.org/content/vol0/issue2006/images/data/0604078103/DC1/04078Table1.xls}
  or  \url{ftp://genome-ftp.stanford.edu/pub/yeast/systematic_results/phenotypes}
}  
\references{
Osterberg M, Kim H, Warringer J, Melen K, Blomberg A, von Heijne
G. (2006) Phenotypic effects of membrane protein overexpression in Saccharomyces
cerevisiae. PNAS. 103(30):11148-53. PMID: 16847257 
}

\author{N. LeMeur}

\examples{
data(OsterbergRaw)
str(OsterbergRaw)

}
\keyword{datasets}

