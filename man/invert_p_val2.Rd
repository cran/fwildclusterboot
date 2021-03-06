% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/invert_p_val2.R
\name{invert_p_val2}
\alias{invert_p_val2}
\title{Calculation of Confidence Sets}
\usage{
invert_p_val2(
  object,
  boot_iter,
  point_estimate,
  se_guess,
  clustid,
  sign_level,
  vcov_sign,
  impose_null,
  p_val_type,
  tol,
  maxiter
)
}
\arguments{
\item{object}{A  object of type boottest}

\item{boot_iter}{An integer. Number of bootstrap iterations}

\item{point_estimate}{A scalar. Point estimate of the coefficient of interest from the regression model}

\item{se_guess}{A scalar vector of dimension 2. A guess of the standard error that initiates the p-value inversion.}

\item{clustid}{A vector with the clusters}

\item{sign_level}{A numeric between 0 and 1. Sets to confidence level: sign_level = 0.05 returns 0.95\% confidence intervals}

\item{vcov_sign}{Controls addition / substraction of individual covariance matrices for multiway clustering}

\item{impose_null}{Logical. Controls if the null hypothesis is imposed on the bootstrap dgp or not. Null imposed - WCR - by default. If FALSE, unrestricted WCU}

\item{p_val_type}{type Type of p-value. By default "two-tailed". Other options: "equal-tailed", ">", "<"}

\item{tol}{the desired accuracy (convergence tolerance) for confidence interval inversion. 1e-6 by default.}

\item{maxiter}{maximum number of iterations for confidence interval inversion. 10 by default.}
}
\value{
A list containing the calculated confidence interval, the t-statistics
and corresponding p-values used in the grid search.
}
\description{
Inverts the bootstrap p-value and calculates confidence sets
}
