\name{CompoundEvents-package}
\docType{package}
\alias{CompoundEvents-package}
\title{Statistical Modeling of Compound Events}
\description{
Tools for extracting occurrences, assessing potential driving factors, predicting occurrences,
and quantifying impacts of compound events (with focus on dry-hot events) in hydrology and climatology.
}

\details{
Examples of compound events in hydroclimatology with focus on compound dry-hot events.
The function \code{\link{GetDH}} is used for extracting occurrences based on thresholds of dry and hot indicators.
The function \code{\link{ChangeDH}} is used for changes in compound dry and hot events.
The function \code{\link{DriverLogit}} is used for assessing potential driving factors of compound events based on logistic regression model.
The function \code{\link{PredLogit}} is used for predicting occurrences of compound events.
The function \code{\link{ImpactMGDH}} is used for quantifying impacts of compound dry and hot events on a sector (e.g., crop yield, GPP,NDVI) based on meta-Gaussian model.
The function \code{\link{SCEIDH}} is used for computing the standardized compound event index of compound dry-hot events.
}
\author{
Zengchao Hao
}
\references{


Hao, Z., et al.(2013). Changes in concurrent monthly precipitation and temperature extremes. Environ. Res. Lett. 8: 034014.


Hao, Z. et al. (2019). A monitoring and prediction system for compound dry and hot events. Environ. Res. Lett., 14:114034.


Hao, Z. et al. (2019). Statistical prediction of the severity of compound dry-hot events based on ENSO . J. Hydrol., 572: 243-250.

Feng, S. et al. (2019). Probabilistic evaluation of the impact of compound dry-hot events on global maize yields. Sci. Total. Environ., 689: 1228-1234.

Hao, Z et al. (2022). Compound droughts and hot extremes: Characteristics, drivers, changes, and impacts. Earth Sci. Rev., 235, 104241.
}

