\name{GiaeverPheno}
\alias{GiaeverPheno}
\alias{GiaeverGene}
\alias{GiaeverExpCdt}
\alias{Giaeverresult}
\docType{data}
\title{List of fitness defect score generated from Giaever et al 2002}
\description{
  Giaever et al (2002) create a  collection of gene-deletion mutants to
  determine genes that contribute to a  particular phenotype in specific
  environmental conditions. This list is generated from a fitness
  analysis under six different experimental conditions.
 }
\usage{
data(GiaeverPheno)
data(GiaeverGene)
data(GiaeverExpCdt)
}
\format{
  \code{GiaeverPheno} is a list with 31 elements. The name of each
  element is a experimental condition (see details). The value of each
  element are the fitness defect scores for the genes sensitive to the
  experimental condition, as defined by Giaever et al (2002).

  \code{GiaeverGene} Vector of the systematic gene names of the 5898 tested
  genes. Note that some updates have been made for the list to be
  consistent with \emph{Saccharomyces Genome Database}.
  
  \code{GiaeverExpCdt} is a 3 columns dataframe with fileID from
  which the data were extracted, the generation time (growth time) and
  the condition (media).
  
  \describe{ 
    \item{gen.}{generations}
    \item{rep.}{replicate}
    \item{ypg5a,ypg5b}{yeast/peptone/galactose 5 gen. rep. a and b  ==> carbone source}
    \item{ypg15a ypg15b}{yeast/peptone/galactose 15 gen. rep.  a and b ==> carbone source}
    \item{sorbitol5a sorbitol5b}{1.5M Sorbitol 5 gen. rep. a and b ==> sugar, osmotic stress}
    \item{sorbitol20a sorbitol15b}{1.5M Sorbitol 20  and 15 gen. rep. a and b respectively==> sugar, osmotic stress}
    \item{NaCl5a NaCl5b}{1M NaCl 5 gen. rep.  a and b ==> salt, osmotic stress}
    \item{NaCl15a NaCl15b}{1M NaCl 15 gen. rep. a and b ==> salt, osmotic stress}
    \item{lysM5a lysM5b}{lysine minus 5 gen. rep. a and b ==> lack of required AA}
    \item{thM5a}{threonine minus 5 gen. rep. a ==> lack of required AA}
    \item{trpM5a trpM5b}{tritophanee minus 5 gen. rep. a ==> lack of required AA}
    \item{minimalPlus5a minimalPlus5b}{minimal + histidine/leuvine/uracile 5 gen. rep. a and b}
    \item{minimalPlus15a minimalPlus15b}{minimal + histidine/leuvine/uracile 15 gen. rep. a and b}
    \item{minimalC5a minimalC5b}{minimal complete 5 gen. rep. a and b}
    \item{nystatin5a nystatin5b}{Nystatin 5 gen. rep. a and b ==> antifungal drug}
    \item{nystatin15a nystatin15b}{Nystatin 5 gen. rep. a and b ==> antifungal drug}
    \item{pH8g5a pH8g5b}{pH 8 5 gen. rep. a and b ==> alkali stress}
    \item{pH8g15a pH8g20b}{pH 8 15 and 20 gen. rep. a and b respectively ==> alkali stress}
  }
  Note: in their study they confound the 15 and 20 generations.
  
  \code{Giaeverresult} is a data.frame that summaryzes the number of
    sensitive genes per condition, how many of those genes are present in
    the ScISI interactome and the associated p-value. This is the result
    of applying a Hypergeometric test (see \code{CoHyperGParams-class}
    for more details) and the \code{complexStatus} function.
 
}
\source{
  Giaever et al (2002), supplementary information:
  \url{http://genomics.lbl.gov/YeastFitnessData/websitefiles/cel_index.html}
  Saccharomyces Genome Database (last update 03/17/06):
  \url{http://www.yeastgenome.org/}
}
\references{
  
Giaever G et al (2002) Functional profiling of the Saccharomyces cerevisiae
genome. Nature. 418(6896):387-91. PMID: 12140549 
}

\author{N. LeMeur}

\examples{
data(GiaeverPheno)
data(GiaeverExpCdt)
data(GiaeverGene)
}
\keyword{datasets}


