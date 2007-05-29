\name{Overview}
\docType{package}
\alias{lancet.iraqmortality}
\alias{lancet.iraqmortality-package}
\title{Overview of lancet.iraqmortality package}

\description{

The Lancet has published two surveys (Roberts et al (2004) and Burnham
et al (2006)) on Iraq mortality before and after the US-led
invasion. This package serves three purposes. First, it includes a
portion of the summary data related to the first study, both the raw
.xls file distributed by the authors (\code{/doc/lancet1.xls}) and a cleaned up R
dataframe (type \code{data(lancet1)}). Second, it provides simple
functions for working with data from the second study. The authors have
distributed this data only to selected researchers. Those researchers
can use this package to work with that data more easily. Third, for
researchers without such access, we provide a vignette --- type
\code{vignette(mortality)} --- which serves as a guided tour of some of
the more interesting aspects of the data. The code for the vignette is
provided in \code{/doc\_rnw/mortality.Rnw}. This location is
non-standard but necessary since it prevents the automatic checking of
the vignette, which would fail since the necessary data is not
distributed with the package.
}

\author{David Kane, Arjun Ravi Narayan}

\references{

  \url{http://en.wikipedia.org/wiki/Lancet_surveys_of_mortality_before_and_after_the_2003_invasion_of_Iraq}
  
  "Mortality before and after the 2003 invasion of Iraq: cluster sample
  survey" by Les Roberts, Riyadh Lafta, Richard Garfield, Jamal
  Khudhairi, and Gilbert Burnham. The Lancet, October 29, 2004.

  
  "Mortality after the 2003 invasion of Iraq: a cross-sectional cluster
  sample survey" by Gilbert Burnham, Riyadh Lafta, Shannon
  Doocy, and Les Roberts. The Lancet, October 11, 2006.

  \url{http://crookedtimber.org/2004/11/11/lancet-roundup-and-literature-review}

  \url{http://scienceblogs.com/deltoid/lancetiraq/}
  
  }

\keyword{package}
