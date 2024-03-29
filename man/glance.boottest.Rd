% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{glance.boottest}
\alias{glance.boottest}
\title{S3 method to glance at objects of class boottest}
\usage{
\method{glance}{boottest}(x, ...)
}
\arguments{
\item{x}{object of type boottest}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
A single row summary "glance" of an object of type boottest
- lists characteristics of the input regression model
}
\description{
S3 method to glance at objects of class boottest
}
\examples{
\dontrun{
requireNamespace("fwildclusterboot")
data(voters)
lm_fit <- lm(
proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
  data = voters
)
boot <- boottest(lm_fit,
  B = 9999,
  param = "treatment",
  clustid = "group_id1"
)
generics::glance(boot)
}
}
