test_that("test r against Julia I: stochastic tests", {


  skip_on_cran()
  skip_on_ci()
  skip_if_not(
    find_proglang("julia"), 
    message = "skip test as julia installation not found."
  )

  if(TRUE){
    reltol <- 0.05

    N <- 10000
    seed <- 875784

    data1 <<- fwildclusterboot:::create_data(
      N = 10000,
      N_G1 = 20,
      icc1 = 0.5,
      N_G2 = 20,
      icc2 = 0.2,
      numb_fe1 = 10,
      numb_fe2 = 10,
      seed = 908369,
      weights = 1:N / N
    )

    lm_fit <- lm(proposition_vote ~ treatment + log_income,
                 data = data1
    )



    lm_fit_weights <- lm(
      proposition_vote ~ treatment + log_income,
      weights = data1$weights,
      data = data1
    )
    lm_fits <- list(ols = lm_fit, wls = lm_fit_weights)

    # object = lm_fit
    # impose_null = FALSE
    # type = "rademacher"
    # p_val_type = "two-tailed"


    cat("Part 1: Large B Tests", "\n")


    for (object in lm_fits) {
      cat("start ols/wls", "\n")
      set.seed(12391786)
      # type <- "rademacher"
      for (type in c("rademacher", "webb", "mammen", "norm")) {
        for (p_val_type in c("two-tailed", "equal-tailed", ">", "<")) {
          for (impose_null in c(TRUE, FALSE)) {
            cat(
              paste(
                "type:",
                type,
                "p-val:",
                p_val_type,
                "null imposed:",
                impose_null
              ),
              "\n"
            )

            cat(type, "\n")
            cat(p_val_type, "\n")
            cat(impose_null, "\n")

            # cat("Check 1:", "\n")
            if (p_val_type %in% c("two-tailed", "equal-tailed")) {
              boot_r <-
                suppressWarnings(
                  boottest(
                    object,
                    clustid = "group_id1",
                    B = 19999,
                    param = "treatment",
                    type = type,
                    p_val_type = p_val_type,
                    impose_null = impose_null
                  )
                )
              # pracma::tic()
              boot_jl <-
                suppressWarnings(
                  boottest(
                    object,
                    clustid = "group_id1",
                    B = 19999,
                    param = "treatment",
                    type = type,
                    p_val_type = p_val_type,
                    impose_null = impose_null,
                    engine = "WildBootTests.jl"
                  )
                )
              # pracma::toc()
              res <-
                expect_equal(
                  boot_r$p_val,
                  boot_jl$p_val[1],
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
              res <-
                expect_equal(
                  boot_r$t_stat,
                  boot_jl$t_stat[1],
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
              res <-
                expect_equal(
                  boot_r$conf_int,
                  c(boot_jl$conf_int),
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
            } else {
              boot_r <-
                suppressWarnings(
                  boottest(
                    object,
                    clustid = "group_id1",
                    B = 19999,
                    param = "treatment",
                    type = type,
                    p_val_type = p_val_type,
                    impose_null = impose_null,
                    conf_int = FALSE
                  )
                )
              boot_jl <-
                suppressWarnings(
                  boottest(
                    object,
                    clustid = "group_id1",
                    B = 19999,
                    param = "treatment",
                    type = type,
                    p_val_type = p_val_type,
                    impose_null = impose_null,
                    conf_int = FALSE,
                    engine = "WildBootTests.jl"
                  )
                )
              res <-
                expect_equal(
                  boot_r$p_val,
                  boot_jl$p_val[1],
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
              res <-
                expect_equal(
                  boot_r$t_stat,
                  boot_jl$t_stat[1],
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
            }

            rm(boot_r, boot_jl)



            # multi-param hypotheses
            # cat("Check 2:", "\n")
            if (p_val_type %in% c("two-tailed", "equal-tailed")) {
              boot_r <-
                suppressWarnings(
                  boottest(
                    object,
                    clustid = "group_id1",
                    B = 19999,
                    param = c("treatment", "log_income"),
                    R = c(1, 0.1),
                    r = 0.1,
                    type = type,
                    p_val_type = p_val_type,
                    impose_null = impose_null
                  )
                )
              boot_jl <-
                suppressWarnings(
                  boottest(
                    object,
                    clustid = "group_id1",
                    B = 19999,
                    param = c("treatment", "log_income"),
                    R = c(1, 0.1),
                    r = 0.1,
                    type = type,
                    p_val_type = p_val_type,
                    impose_null = impose_null,
                    engine = "WildBootTests.jl"
                  )
                )
              res <-
                expect_equal(
                  boot_r$p_val,
                  boot_jl$p_val[1],
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
              res <-
                expect_equal(
                  boot_r$t_stat,
                  boot_jl$t_stat[1],
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
              res <-
                expect_equal(
                  boot_r$conf_int,
                  c(boot_jl$conf_int),
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
            } else {
              boot_r <-
                suppressWarnings(
                  boottest(
                    object,
                    clustid = "group_id1",
                    B = 19999,
                    param = c("treatment", "log_income"),
                    R = c(1, 0.1),
                    r = 0.1,
                    type = type,
                    p_val_type = p_val_type,
                    impose_null = impose_null,
                    conf_int = FALSE
                  )
                )
              boot_jl <-
                suppressWarnings(
                  boottest(
                    object,
                    clustid = "group_id1",
                    B = 19999,
                    param = c("treatment", "log_income"),
                    R = c(1, 0.1),
                    r = 0.1,
                    type = type,
                    p_val_type = p_val_type,
                    impose_null = impose_null,
                    conf_int = FALSE,
                    engine = "WildBootTests.jl"
                  )
                )
              res <-
                expect_equal(
                  boot_r$p_val,
                  boot_jl$p_val[1],
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
              res <-
                expect_equal(
                  boot_r$t_stat,
                  boot_jl$t_stat[1],
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
            }

            rm(boot_r, boot_jl)

            # --------------------------------------------------
            # and all with twoway clustering:
            # cat("Check 3:", "\n")
            if (p_val_type %in% c("two-tailed", "equal-tailed")) {
              boot_r <-
                suppressWarnings(
                  boottest(
                    object,
                    clustid = c("group_id1", "group_id2"),
                    B = 19999,
                    param = "treatment",
                    type = type,
                    p_val_type = p_val_type,
                    impose_null = impose_null,
                    nthreads = 6
                  )
                )
              boot_jl <-
                suppressWarnings(
                  boottest(
                    object,
                    clustid = c("group_id1", "group_id2"),
                    B = 19999,
                    param = "treatment",
                    type = type,
                    p_val_type = p_val_type,
                    impose_null = impose_null,
                    engine = "WildBootTests.jl"
                  )
                )
              res <-
                expect_equal(
                  boot_r$p_val,
                  boot_jl$p_val[1],
                  tolerance = reltol
                )
              # if(res == FALSE){print(res)}
              rm(res)
              res <-
                expect_equal(
                  boot_r$t_stat,
                  boot_jl$t_stat[1],
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
              res <-
                expect_equal(
                  boot_r$conf_int,
                  c(boot_jl$conf_int),
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
            } else {
              boot_r <-
                suppressWarnings(
                  boottest(
                    object,
                    clustid = c("group_id1", "group_id2"),
                    B = 19999,
                    param = "treatment",
                    type = type,
                    p_val_type = p_val_type,
                    impose_null = impose_null,
                    nthreads = 6,
                    conf_int = FALSE
                  )
                )
              boot_jl <-
                suppressWarnings(
                  boottest(
                    object,
                    clustid = c("group_id1", "group_id2"),
                    B = 19999,
                    param = "treatment",
                    type = type,
                    p_val_type = p_val_type,
                    impose_null = impose_null,
                    conf_int = FALSE,
                    engine = "WildBootTests.jl"
                  )
                )
              res <-
                expect_equal(
                  boot_r$p_val,
                  boot_jl$p_val[1],
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
              res <-
                expect_equal(
                  boot_r$t_stat,
                  boot_jl$t_stat[1],
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
            }

            rm(boot_r, boot_jl)

            # multi-param hypotheses

            # cat("Check 4:", "\n")
            if (p_val_type %in% c("two-tailed", "equal-tailed")) {
              boot_r <-
                suppressWarnings(
                  boottest(
                    object,
                    clustid = c("group_id1", "group_id2"),
                    B = 19999,
                    param = c("treatment", "log_income"),
                    R = c(1, 0.1),
                    type = type,
                    p_val_type = p_val_type,
                    impose_null = impose_null,
                    nthreads = 6
                  )
                )
              boot_jl <-
                suppressWarnings(
                  boottest(
                    object,
                    clustid = c("group_id1", "group_id2"),
                    B = 19999,
                    param = c("treatment", "log_income"),
                    R = c(1, 0.1),
                    type = type,
                    p_val_type = p_val_type,
                    impose_null = impose_null,
                    engine = "WildBootTests.jl"
                  )
                )
              res <-
                expect_equal(
                  boot_r$p_val,
                  boot_jl$p_val[1],
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
              res <-
                expect_equal(
                  boot_r$t_stat,
                  boot_jl$t_stat[1],
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
              res <-
                expect_equal(
                  boot_r$conf_int,
                  c(boot_jl$conf_int),
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
            } else {
              boot_r <-
                suppressWarnings(
                  boottest(
                    object,
                    clustid = c("group_id1", "group_id2"),
                    B = 19999,
                    param = c("treatment", "log_income"),
                    R = c(1, 0.1),
                    type = type,
                    p_val_type = p_val_type,
                    impose_null = impose_null,
                    nthreads = 6,
                    conf_int = FALSE
                  )
                )
              boot_jl <-
                suppressWarnings(
                  boottest(
                    object,
                    clustid = c("group_id1", "group_id2"),
                    B = 19999,
                    param = c("treatment", "log_income"),
                    R = c(1, 0.1),
                    type = type,
                    p_val_type = p_val_type,
                    impose_null = impose_null,
                    conf_int = FALSE,
                    engine = "WildBootTests.jl"
                  )
                )
              res <-
                expect_equal(
                  boot_r$p_val,
                  boot_jl$p_val[1],
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
              res <-
                expect_equal(
                  boot_r$t_stat,
                  boot_jl$t_stat[1],
                  tolerance = reltol,
                  ignore_attr = TRUE
                )
              # if(res == FALSE){print(res)}
              rm(res)
            }

            rm(boot_r, boot_jl)

            # --------------------------------------------------------------- #
            # test subcluster bootstrap


            if (p_val_type %in% c("two-tailed", "equal-tailed")) {
              # bootcluster variable not in clustid 1
              boot_r <- suppressWarnings(
                boottest(
                  lm_fit,
                  clustid = "group_id1",
                  bootcluster = c("group_id1", "Q1_immigration"),
                  B = 19999,
                  param = "treatment",
                  type = "rademacher",
                  p_val_type = p_val_type
                )
              )

              boot_jl1 <- suppressWarnings(
                boottest(
                  lm_fit,
                  clustid = "group_id1",
                  bootcluster = c("group_id1", "Q1_immigration"),
                  B = 19999,
                  param = "treatment",
                  type = "rademacher",
                  p_val_type = p_val_type
                )
              )

              expect_equal(boot_r$p_val, boot_jl1$p_val, tolerance = reltol)
              expect_equal(boot_r$t_stat, boot_jl1$t_stat)
              if (p_val_type %in% c("two-tailed", "equal-tailed")) {
                expect_equal(boot_r$conf_int, boot_jl1$conf_int,
                             tolerance = reltol
                )
              }

              # bootcluster variable not in clustid 2
              # currently: bug in fwildclusterboot when not all bootcluster
              # variables \in clustid OR specified in lm() (e.g. drop
              # Q2_defense from lm_fit -> error)
              boot_r <-
                suppressWarnings(
                  boottest(
                    lm_fit,
                    clustid = "group_id1",
                    bootcluster = c("group_id1", "year"),
                    B = 19999,
                    param = "treatment",
                    type = "rademacher",
                    p_val_type = p_val_type
                  )
                )
              boot_jl1 <-
                suppressWarnings(
                  boottest(
                    lm_fit,
                    clustid = "group_id1",
                    bootcluster = c("group_id1", "year"),
                    B = 19999,
                    param = "treatment",
                    type = "rademacher",
                    p_val_type = p_val_type
                  )
                )

              expect_equal(boot_r$p_val, boot_jl1$p_val, tolerance = reltol)
              expect_equal(boot_r$t_stat, boot_jl1$t_stat)
              if (p_val_type %in% c("two-tailed", "equal-tailed")) {
                expect_equal(boot_r$conf_int, boot_jl1$conf_int,
                             tolerance = reltol
                )
              }

              boot_r <-
                suppressWarnings(
                  boottest(
                    lm_fit,
                    clustid = "group_id2",
                    bootcluster = c("group_id2", "state"),
                    B = 19999,
                    param = "treatment",
                    type = "rademacher",
                    p_val_type = p_val_type
                  )
                )
              boot_jl1 <-
                suppressWarnings(
                  boottest(
                    lm_fit,
                    clustid = "group_id2",
                    bootcluster = c("group_id2", "state"),
                    B = 19999,
                    param = "treatment",
                    type = "rademacher",
                    p_val_type = p_val_type
                  )
                )

              expect_equal(boot_r$p_val, boot_jl1$p_val, tolerance = reltol)
              expect_equal(boot_r$t_stat, boot_jl1$t_stat)
              if (p_val_type %in% c("two-tailed", "equal-tailed")) {
                expect_equal(boot_r$conf_int, boot_jl1$conf_int,
                             tolerance = reltol
                )
              }

              # clustid variale not in bootcluster & bootcluster variable
              # not in clustid
              boot_r <-
                suppressWarnings(
                  boottest(
                    lm_fit,
                    clustid = c("group_id1", "group_id2"),
                    bootcluster = c("group_id1", "group_id2", "Q1_immigration"),
                    B = 19999,
                    param = "treatment",
                    nthreads = 6
                  )
                )

              boot_jl1 <-
                suppressWarnings(
                  boottest(
                    lm_fit,
                    clustid = c("group_id1", "group_id2"),
                    bootcluster = c("group_id1", "group_id2", "Q1_immigration"),
                    B = 19999,
                    param = "treatment"
                  )
                )


              expect_equal(boot_r$p_val, boot_jl1$p_val, tolerance = reltol)
              expect_equal(boot_r$t_stat, boot_jl1$t_stat)
              if (p_val_type %in% c("two-tailed", "equal-tailed")) {
                expect_equal(boot_r$conf_int, boot_jl1$conf_int,
                             tolerance = reltol
                )
              }


              # clustid variale not in bootcluster & bootcluster variable
              # not in clustid
              boot_r <-
                suppressWarnings(
                  boottest(
                    lm_fit,
                    clustid = c("group_id1", "group_id2"),
                    bootcluster = c("group_id1", "Q1_immigration"),
                    B = 19999,
                    param = "treatment",
                    nthreads = 6
                  )
                )

              boot_jl1 <-
                suppressWarnings(
                  boottest(
                    lm_fit,
                    clustid = c("group_id1", "group_id2"),
                    bootcluster = c("group_id1", "Q1_immigration"),
                    B = 19999,
                    param = "treatment"
                  )
                )


              expect_equal(boot_r$p_val, boot_jl1$p_val, tolerance = reltol)
              expect_equal(boot_r$t_stat, boot_jl1$t_stat)
              if (p_val_type %in% c("two-tailed", "equal-tailed")) {
                expect_equal(boot_r$conf_int, boot_jl1$conf_int,
                             tolerance = reltol
                )
              }

              # clustid variale not in bootcluster & bootcluster variable
              # not in clustid

              boot_r <-
                suppressWarnings(
                  boottest(
                    lm_fit,
                    clustid = c("group_id1", "group_id2"),
                    bootcluster = c("group_id1"),
                    B = 499999,
                    param = "treatment"
                  )
                )
              boot_jl1 <-
                suppressWarnings(
                  boottest(
                    lm_fit,
                    clustid = c("group_id1", "group_id2"),
                    bootcluster = c("group_id1"),
                    B = 499999,
                    param = "treatment"
                  )
                )

              expect_equal(boot_r$p_val, boot_jl1$p_val, tolerance = reltol)
              expect_equal(boot_r$t_stat, boot_jl1$t_stat)
              if (p_val_type %in% c("two-tailed", "equal-tailed")) {
                expect_equal(boot_r$conf_int, boot_jl1$conf_int,
                             tolerance = reltol
                )
              }
            }
          }
        }
      }
    }
  } else {
    message(
      "test-r-vs-julia.R skipped as JULIA_BINDIR not found."
    )
  }

})
