\name{prep.deaths}
\alias{prep.deaths}
\title{Loads up deaths dataset from the mortality.zip file}

\description{

Returns a data frame containing renamed and cleaned up variables of the
'deaths' from the provided mortality.zip file in the data directory.
See the package vignette for a description of these variables. If no
mortality.zip is provided, then this function will not work.

}
\usage{
prep.deaths()
}

\value{
  Returns a data frame with information on the 629 deaths recorded in
  Burnham et al (2006). Descibed as the \bold{deaths} data frame in the
  vignette.
  }

\seealso{
  \code{\link{prep.houses}}
}

\author{Arjun Ravi Narayan, David Kane}

\keyword{datasets}
