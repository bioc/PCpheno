\name{getFDgene}
\alias{getFDgene}

\title{Get fitness defect genes}
\description{
  Function to select genes that present a significant growth defect
  according to the condition(media) or generation time.
  }
\usage{
getFDgene(data,condition,cutoff,mode="generation",subset)
}
\arguments{
  \item{data}{List of fitness defect scores for genes tested at
    different experimental conditions.}
  \item{condition}{Dataframe of experimental conditions}
  \item{cutoff}{Numerical vector of length one or more, defining the threshold of
    'significance' for the fitness defect score}
  \item{mode}{Character string defining the base of the selection either
    'condition' (media) or 'generation' time, Default=generation.}
  \item{subset}{Numerical vector or list to which apply the different cutoffs.}
}

\value{
  Reduced list of gene fitness scores per experimental condition
  according to the experimental condition or the generation time. 
}
\author{N. LeMeur}
\references{
Giaever G. et al. (2002) Functional profiling of the Saccharomyces cerevisiae
genome. Nature. 418(6896):387-91. PMID: 12140549 
}
\examples{
 data(GiaeverPheno)
 data(GiaeverExpCdt)
 ##Select all the genes, in the different experimental conditions, that present a fitness score above 20, 100 and 100 at 5, 15 and 20 generations, respectively 
 fitnessGen <- getFDgene(GiaeverPheno,condition=GiaeverExpCdt,cutoff=c(20,100,100),mode="generation",subset=c(5,15,20))
 ##Select all the genes, that present a fitness score above 15 and 100
 ##in the condition set A and B respectively, independently of the generation time 
fitnessCondt <- getFDgene(GiaeverPheno,condition=GiaeverExpCdt,cutoff=c(100,15),mode="condition",subset=list(a=c(1:23,27,30,31),b=c(24:26,28,29)))
}
\keyword{data}
\keyword{manip}
