gtools_permutations <-
  function(n,
           r,
           v = 1:n,
           set = TRUE,
           repeats.allowed = FALSE) {
    #' copied from the permutations package as long as it is orphaned on CRAN
    #' @param n size of source vector
    #' @param r size of target vector
    #' @param v source vector. defaults to 1:n
    #' @param set logical flag for duplicates
    #' @param repeats.allowed logical flag
    #' @noRd
    
    if (mode(n) != "numeric" || length(n) != 1 || n < 1 ||
        (n %% 1) != 0) {
      stop("bad value of n")
    }
    if (mode(r) != "numeric" || length(r) != 1 || r < 1 ||
        (r %% 1) != 0) {
      stop("bad value of r")
    }
    if (!is.atomic(v) || length(v) < n) {
      stop("v is either non-atomic or too short")
    }
    if ((r > n) & repeats.allowed == FALSE) {
      stop("r > n and repeats.allowed=FALSE")
    }
    if (set) {
      v <- unique(sort(v))
      if (length(v) < n) {
        stop("too few different elements")
      }
    }
    v0 <- vector(mode(v), 0)
    if (repeats.allowed) {
      sub <- function(n, r, v) {
        if (r == 1) {
          matrix(v, n, 1)
        } else if (n == 1) {
          matrix(v, 1, r)
        } else {
          inner <- Recall(n, r - 1, v)
          cbind(
            rep(v, rep(nrow(inner), n)),
            matrix(
              t(inner),
              ncol = ncol(inner),
              nrow = nrow(inner) * n,
              byrow = TRUE
            )
          )
        }
      }
    } else {
      sub <- function(n, r, v) {
        if (r == 1) {
          matrix(v, n, 1)
        } else if (n == 1) {
          matrix(v, 1, r)
        } else {
          X <- NULL
          for (i in 1:n) {
            X <- rbind(X, cbind(v[i], Recall(n -
                                               1, r - 1, v[-i])))
          }
          X
        }
      }
    }
    sub(n, r, v[1:n])
  }
