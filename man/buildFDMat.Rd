\name{buildFDMat}
\alias{buildFDMat}

\title{Build fitness defect contingency matrix}
\description{
  Function to build a fitness defect contingency matrix where rows
  correspond to tested genes and columns to experimental conditions.
  }
\usage{
buildFDMat(data,genenames,condition)
}
\arguments{
  \item{data}{List of 'significant' fitness defect scores and the
    associated genes at different experimental conditions.}
  \item{condition}{Character vector of the different experimental
    conditions tested}
  \item{genenames}{Character vector of all the tested genes for fitness defect.}
}

\value{
  Contingency matrix of genes that present significant fitness defect in different experimental conditions. 
}
\author{N. LeMeur}

\examples{
 data(GiaeverPheno)
 data(GiaeverExpCdt)
 data(GiaeverGene)
 fitnessData <- getFDgene(GiaeverPheno,condition=GiaeverExpCdt,cutoff=c(20,100,100),mode="generation",subset=c(5,15,20))
 GiaeverPhenoM <- buildFDMat(data=fitnessData,genenames=GiaeverGene,condition=GiaeverExpCdt[,3])
}
\keyword{data}
\keyword{manip}
