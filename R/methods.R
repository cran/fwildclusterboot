boottest <- function(object,
                     ...) {
  #' 
  #' Fast wild cluster bootstrap inference
  #' 
  #'  
  #' `boottest` is a S3 method that allows for fast wild cluster 
  #' bootstrap inference for objects of class lm, fixest and felm by  implementing
  #' the fast wild bootstrap algorithm developed in Roodman et al., 2019.
  #' 
  #' @param object An object of type lm, fixest of felm
  #' @param ... other arguments
  #' 
  #' @export
  #' 
  #' @return An object of class \code{boottest}
  #' 
  #' \item{p_val}{The bootstrap p-value.}
  #' \item{t_stat}{The bootstrap t-statistic.}
  #' \item{conf_int}{The bootstrap confidence interval.}
  #' \item{param}{The tested parameter.}
  #' \item{N}{Sample size. Might differ from the regression sample size if the 
  #'          cluster variables contain NA values.}
  #' \item{B}{Number of Bootstrap Iterations.}
  #' \item{clustid}{Names of the cluster Variables.}
  #' \item{N_G}{Dimension of the cluster variables as used in boottest.}
  #' \item{sign_level}{Significance level used in boottest.}
  #' \item{type}{Distribution of the bootstrap weights.}
  #' \item{p_test_vals}{All p-values calculated while calculating the confidence
  #'       interval.}
  #' \item{test_vals}{All t-statistics calculated while calculating the
  #'       confidence interval.}
  #' \item{regression}{The regression object used in boottest.}
  #' \item{call}{Function call of boottest.}
  #' 
  #' @section Confidence Intervals:
  #' \code{boottest} computes confidence intervals by inverting p-values. 
  #'       In practice, the following procedure is used:
  #' \itemize{
  #' \item Based on an initial guess for starting values, calculate p-values 
  #'       for 26 equal spaced points between the starting values.
  #' \item Out of the 26 calculated p-values, find the two pairs of values x 
  #'       for which the corresponding p-values px cross the significance level
  #'       sign_level.
  #' \item Feed the two pairs of x into an numerical root finding procedure and
  #'       solve for the root. boottest currently relies on 
  #'       \code{stats::uniroot} and sets an absolute tolerance of 1e-06 and 
  #'       stops the procedure after 10 iterations.
  #' }
  #' @section Standard Errors:
  #' \code{boottest} does not calculate standard errors.
  #' @references Roodman et al., 2019, "Fast and wild: Bootstrap inference in 
  #'             STATA using boottest", The STATA Journal. 
  #'             (\url{https://journals.sagepub.com/doi/full/10.1177/1536867X19830877})
  #' @examples
  #' library(fwildclusterboot)
  #' data(voters)
  #' lm_fit <-lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration,
  #'          data = voters)
  #' boot1 <- boottest(lm_fit, 
  #'                   B = 9999, 
  #'                   param = "treatment",
  #'                    clustid = "group_id1")
  #' boot2 <- boottest(lm_fit,
  #'                   B = 9999,
  #'                   param = "treatment",
  #'                  clustid = c("group_id1", "group_id2"))
  #' boot3 <- boottest(lm_fit,
  #'                   B = 9999,
  #'                   param = "treatment",
  #'                   clustid = c("group_id1", "group_id2"),
  #'                   sign_level = 0.2,
  #'                   seed = 8,
  #'                   beta0 = 2)
  #' summary(boot1)
  #' plot(boot1)

  UseMethod("boottest")
}

#' tidy <- function(object, ...) {
#'   #' S3 method to summarize objects of class boottest into tidy data.frame
#'   #' @export
#'   #' @param object object of type boottest
#'   #' @param ... other arguments
#'   UseMethod("tidy")
#' }
#' 
#' glance <- function(object, ...) {
#'   #' S3 method to summarize objects of class boottest
#'   #' only needed for use with modelsummary package
#'   #' broom needs to be imported
#'   #' @export
#'   #' @param object object of type boottest
#'   #' @param ... other arguments
#'   UseMethod("glance")
#' }