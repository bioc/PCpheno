\name{truncName}
\alias{truncName}

\title{Truncate character strings}
\description{
  Truncate character strings
}
\usage{
truncName(x,n)
}

\arguments{
  \item{x}{Character string}
  \item{n}{Maximum length (in characters)}
}

\value{
  Character string 
}

\author{N. LeMeur}

\examples{
xx <- "Anticonstitutionnelement is a family name"
truncName(xx,5)
}
\keyword{data}
\keyword{manip}
