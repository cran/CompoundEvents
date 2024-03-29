% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Empdis2.R
\name{Empdis2}
\alias{Empdis2}
\title{Bivariate empirical probability}
\usage{
Empdis2(mp,mt)
}
\arguments{
\item{mp}{monthly precipitation}

\item{mt}{monthly temperature}
}
\value{
The bivariate empirical probability
}
\description{
Compute bivariate empirical probability
}
\examples{
mp=matrix(rnorm(120,0,1),ncol=1)
mt=matrix(rnorm(120,0,1),ncol=1)
nd<-Empdis2(mp,mt)
}
\references{
Hao, Z. et al., 2019a. Statistical prediction of the severity of compound dry-hot events based on El Ni??o-Southern Oscillation. J. Hydrol., 572, 243-250.
}
