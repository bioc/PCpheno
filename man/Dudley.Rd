\name{DudleyPheno}
\docType{data}
\alias{Dudley}
\alias{DudleyGenelist}
\alias{DudleyPhenoFull}
\alias{DudleyPhenoM}
\alias{Dudleyresult}
\alias{DudleySign}
\docType{data}
\title{List of fitness defect score generated from Dudley et al 2005}
\description{
  Dudley et al (2005) create a  collection of gene-deletion mutants to
  determine genes that contribute to a  particular phenotype in specific
  environmental conditions. This list is generated from a fitness
  analysis under 21 different experimental conditions.
 } 
 \usage{
data(DudleyGenelist)
data(DudleyPhenoFull)
data(DudleyPhenoM)
data(DudleySign)
}
\format{
  \code{DudleyGenelist} is a character vector of length 814 genes.
  \code{DudleyPhenoFull} is a dataframe of 814 genes by 23 elements.
  The column contains the yeast gene name. The 22 remaining columns are
  the experimental conditions (see details). The values obtained in
  the different condition are the fitness defect scores for the
  814 genes sensitive to the experimental condition, as defined
  by Dudley et al (2005).
  \code{DudleyPhenoM} is a 814 by 22 incidence matrix
  with rownames corresponding to the genes names and
  columns to an experimental condition. This matrix
  contains a 1 in the (i,j) position if the i-th gene
  is sensitive to the experimental condition of the j-th column;
  it contains a 0 otherwise.
  
  \describe{ 
    \item{Yname}{Yeast systematic gene name}
    \item{benomyl}{15ug/ml benomyl,microtubule function}
    \item{CaCl2}{0.7M calcium chloride, divalent cation}
    \item{CAD}{55uM Cadmium, heavy metal}
    \item{Caff}{2mg/ml Caffeine}
    \item{cyclohex}{0.18ug/ml cycloheximide, protein synthesis}
    \item{DTT}{unknown}
    \item{EtOH}{YPD + 6\% Ethanol}
    \item{FeLim}{iron limited, nutrient limited condition}
    \item{HU}{11.4mg/ml Hydroxyurea, DNA replication and repair}
    \item{HygroB}{50ug/ml hygromycin B, aminoglycosides}
    \item{lowPO4}{Low phosphate, nutrient limited condition}
    \item{MPA}{20ug/ml mycophenolic acid, transcriptional elongation}
    \item{NaCl}{1.2M sodium chloride, general stress condition}
    \item{Paraq}{1mM paraquat, oxidative stress}
    \item{pH3}{Low pH, general stress condition}
    \item{rap}{0.1ug/ml rapamycin, protein synthesis}
    \item{Sorb}{1.2M sorbitol, general stress condition}
    \item{UV}{100J/m2 ultra-violet, DNA replication and repair}
    \item{YPGal}{2\% galactose, carbon source}
    \item{YPGly}{3\% glycerol, carbon source}
    \item{YPLac}{2\% lactate, carbon source}
    \item{YPRaff}{2\% raffinose, carbon source}
  }
  \code{DudleySign} is a list of dataframe that summaryzes in which complexes
    the gene related to the phenotype are found, the size of the
    complexes and the associated p-value. This is the result
    of applying a Hypergeometric test (see \code{CoHyperGParams-class}
    for more details) and the \code{complexStatus} function.
    
   \code{Dudleyresult} is a data.frame that summaryzes the number of
    sensitive genes per condition, how many of those genes are present in
    the ScISI interactome and the associated p-value. This is the result
    of applying a Hypergeometric test (see \code{CoHyperGParams-class}
    for more details) and the \code{complexStatus} function.
 }
\source{
  Dudley et al (2005), supplementary information:
  \url{http://arep.med.harvard.edu/pheno/default.htm}
}
\references{
  Aim\'ee Marie Dudley, Daniel Maarten Janse, Amos Tanay, Ron
  Shamir and George McDonald Church. (2005).A global view of pleiotropy
  and phenotypically derived gene function in yeast. Molecular Systems Biology doi:10.1038/msb4100004  
}

\author{N. LeMeur}

\examples{
data(DudleyPhenoFull)
data(DudleyPhenoM)
}
\keyword{datasets}

