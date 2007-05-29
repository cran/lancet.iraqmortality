\name{prep.houses}
\alias{prep.houses}
\title{Loads up houses dataset from the mortality.zip file}

\description{

Returns a data frame containing renamed and cleaned up variables of the
'houses' from the provided mortality.zip file in the data directory.
See the package vignette for a description of these variables. If no
mortality.zip is provided, then this function will not work.

}

\usage{
prep.houses()
}

\value{
  Returns a data frame with information on the 1,849 households
  interviewed in Burnham et al (2006). Descibed as the \bold{houses}
  data frame in the vignette.
  }

\seealso{
  \code{\link{prep.deaths}}
}

\author{Arjun Ravi Narayan, David Kane}

\keyword{datasets}

