% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{summary.mboottest}
\alias{summary.mboottest}
\title{S3 method to summarize objects of class mboottest}
\usage{
\method{summary}{mboottest}(object, digits = 3, ...)
}
\arguments{
\item{object}{object of type mboottest}

\item{digits}{rounding of output. 3 by default}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
Returns result summaries for objects of type mboottest
}
\description{
S3 method to summarize objects of class mboottest
}
\examples{
\dontrun{
requireNamespace("clubSandwich")
R <- clubSandwich::constrain_zero(2:3, coef(lm_fit))
wboottest <-
  mboottest(
    object = lm_fit,
    clustid = "group_id1",
    B = 999,
    R = R
  )
summary(wboottest)
print(wboottest)
nobs(wboottest)
pval(wboottest)
generics::tidy(wboottest)
}
}
