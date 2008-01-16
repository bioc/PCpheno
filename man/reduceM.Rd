\name{reduceM}
\alias{reduceM}
\title{Resize a matrix}
\description{
  Resize a matrix to the number of rows commun to a vector.
}
\usage{reduceM(x, mat, threshold=0)}

\arguments{
  \item{x}{Character or numeric vector.}
  \item{mat}{Matrix sharing rownames with the supplied vextor x.}
  \item{threshold}{Threshold upon column. Only the columns with a
    \code{colSums} above the  threshold are kept.}
  
}
\value{
  Resized matrix.
}
\author{N. LeMeur}
\examples{
mat <- matrix(c(1:25), nrow = 5, ncol = 5, dimnames = list(c(LETTERS[1:5]), c(1:5)))
xx <- LETTERS[c(2, 4, 5)]
reduceM(xx, mat)
}
\keyword{data}
\keyword{manip}
