test_that("multiplication works", {
  
  skip_on_cran()
  
  # see this issue: https://github.com/s3alfisc/fwildclusterboot/issues/26

  data1 <<- fwildclusterboot:::create_data(N = 500, N_G1 = 6, icc1 = 0.01, N_G2 = 4, icc2 = 0.01, numb_fe1 = 10, numb_fe2 = 10, seed = 9865)

  feols1 <- feols(proposition_vote ~ treatment + ideology1 + log_income + group_id2,
    data = data1,
    weights = ~weights
  )
  feols2 <- feols(proposition_vote ~ treatment + ideology1 + log_income + group_id2,
    data = data1,
    weights = data1$weights
  )

  feols3 <- feols(proposition_vote ~ treatment + ideology1 + log_income + group_id2,
    data = data1,
    cluster = ~group_id1
  )
  feols4 <- feols(proposition_vote ~ treatment + ideology1 + log_income + group_id2,
    data = data1,
    cluster = data1$group_id1
  )

  boot1 <- boottest(feols1, param = "treatment", B = 999, clustid = "group_id1", seed = 12342)
  boot2 <- boottest(feols2, param = "treatment", B = 999, clustid = "group_id1", seed = 12342)
  boot3 <- boottest(feols3, param = "treatment", B = 999, clustid = "group_id1", seed = 12342)
  boot4 <- boottest(feols4, param = "treatment", B = 999, clustid = "group_id1", seed = 12342)

  expect_equal(generics::tidy(boot1), generics::tidy(boot2))
  expect_equal(generics::tidy(boot3), generics::tidy(boot4))




  # test invariance of boottest() results to type of fixed effect variable
  # (numeric vs factor vs character)
  # test issue https://github.com/s3alfisc/fwildclusterboot/issues/14
  # raised by Timothée


  # Test 1: one cluster variable is numeric vs no cluster variable is numeric

  data(voters)

  to_char <- c("Q1_immigration", "Q2_defense", "group_id1")
  sapply(voters[, to_char], class)

  voters_1 <<- voters
  voters_1$Q1_immigration <- as.numeric(voters_1$Q1_immigration)
  sapply(voters_1[, to_char], class)

  feols_fit <- feols(proposition_vote ~ treatment + log_income | Q2_defense, data = voters)
  feols_fit_2 <- feols(proposition_vote ~ treatment + log_income | Q2_defense, data = voters_1)
  lfe_fit <- lfe::felm(proposition_vote ~ treatment + log_income | Q2_defense, data = voters)
  lfe_fit_2 <- lfe::felm(proposition_vote ~ treatment + log_income | Q2_defense, data = voters_1)

  boot1 <-
    suppressWarnings(boottest(feols_fit,
      clustid = c("Q1_immigration", "Q2_defense"),
      B = 9999,
      param = "treatment",
      bootcluster = "min"
    ))
  boot2 <-
    suppressWarnings(boottest(feols_fit_2,
      clustid = c("Q1_immigration", "Q2_defense"),
      B = 9999,
      param = "treatment",
      bootcluster = "min"
    ))
  boot3 <-
    suppressWarnings(boottest(feols_fit,
      clustid = c("Q1_immigration", "Q2_defense"),
      B = 9999,
      param = "treatment",
      bootcluster = "min"
    ))
  boot4 <-
    suppressWarnings(boottest(feols_fit,
      clustid = c("Q1_immigration", "Q2_defense"),
      B = 9999,
      param = "treatment",
      bootcluster = "min"
    ))

  expect_equal(boot1$p_val, boot2$p_val)
  expect_equal(boot2$p_val, boot3$p_val)
  expect_equal(boot3$p_val, boot4$p_val)
  expect_equal(boot4$p_val, boot1$p_val)


  # Test 2: one fixed effect is numeric vs no fixed effect is numeric

  data(voters)

  to_char <- c("Q1_immigration", "Q2_defense", "group_id1")
  sapply(voters[, to_char], class)

  voters_1 <<- voters
  voters_1$Q2_defense <- as.numeric(voters_1$Q2_defense)
  sapply(voters_1[, to_char], class)

  feols_fit <- feols(proposition_vote ~ treatment + log_income | Q2_defense, data = voters)
  feols_fit_2 <- feols(proposition_vote ~ treatment + log_income | Q2_defense, data = voters_1)
  lfe_fit <- lfe::felm(proposition_vote ~ treatment + log_income | Q2_defense, data = voters)
  lfe_fit_2 <- lfe::felm(proposition_vote ~ treatment + log_income | Q2_defense, data = voters_1)

  boot1 <-
    suppressWarnings(boottest(feols_fit,
      clustid = c("Q1_immigration"),
      B = 9999,
      param = "treatment",
      bootcluster = "min",
      seed = 123
    ))
  boot2 <-
    suppressWarnings(boottest(feols_fit_2,
      clustid = c("Q1_immigration"),
      B = 9999,
      param = "treatment",
      bootcluster = "min",
      seed = 123
    ))
  boot3 <-
    suppressWarnings(boottest(lfe_fit,
      clustid = c("Q1_immigration"),
      B = 9999,
      param = "treatment",
      bootcluster = "min",
      seed = 123
    ))
  boot4 <-
    suppressWarnings(boottest(lfe_fit_2,
      clustid = c("Q1_immigration"),
      B = 9999,
      param = "treatment",
      bootcluster = "min",
      seed = 123
    ))
  expect_equal(boot1$p_val, boot2$p_val)
  expect_equal(boot2$p_val, boot3$p_val)
  expect_equal(boot3$p_val, boot4$p_val)
  expect_equal(boot4$p_val, boot1$p_val)


  # Test 3: all fixed effects and cluster variables are numeric vs factors

  data(voters)

  to_char <- c("Q1_immigration", "Q2_defense", "group_id1")
  voters$group_id1 <- as.factor(voters$group_id1)
  sapply(voters[, to_char], class)

  voters_1 <<- voters
  voters_1$Q1_immigration <- as.numeric(voters_1$Q1_immigration)
  voters_1$Q2_defense <- as.numeric(voters_1$Q2_defense)

  sapply(voters_1[, to_char], class)

  feols_fit <- feols(proposition_vote ~ treatment + log_income | Q2_defense, data = voters)
  feols_fit_2 <- feols(proposition_vote ~ treatment + log_income | Q2_defense, data = voters_1)
  lfe_fit <- lfe::felm(proposition_vote ~ treatment + log_income | Q2_defense, data = voters)
  lfe_fit_2 <- lfe::felm(proposition_vote ~ treatment + log_income | Q2_defense, data = voters_1)

  boot1 <-
    suppressWarnings(boottest(feols_fit,
      clustid = c("Q1_immigration"),
      B = 9999,
      param = "treatment",
      bootcluster = "min"
    ))
  boot2 <-
    suppressWarnings(boottest(feols_fit_2,
      clustid = c("Q1_immigration"),
      B = 9999,
      param = "treatment",
      bootcluster = "min"
    ))
  boot3 <-
    suppressWarnings(boottest(lfe_fit,
      clustid = c("Q1_immigration"),
      B = 9999,
      param = "treatment",
      bootcluster = "min"
    ))
  boot4 <-
    suppressWarnings(boottest(lfe_fit_2,
      clustid = c("Q1_immigration"),
      B = 9999,
      param = "treatment",
      bootcluster = "min"
    ))
  expect_equal(boot1$p_val, boot2$p_val)
  expect_equal(boot2$p_val, boot3$p_val)
  expect_equal(boot3$p_val, boot4$p_val)
  expect_equal(boot4$p_val, boot1$p_val)


  # Test 4: Test 3, but now with two fixed effects


  data(voters)

  to_char <- c("Q1_immigration", "Q2_defense", "group_id1")
  voters$group_id1 <- as.factor(voters$group_id1)
  sapply(voters[, to_char], class)

  voters_1 <<- voters
  voters_1$Q1_immigration <- as.numeric(voters_1$Q1_immigration)
  voters_1$Q2_defense <- as.numeric(voters_1$Q2_defense)

  sapply(voters_1[, to_char], class)

  feols_fit <- feols(proposition_vote ~ treatment + log_income | Q1_immigration + Q2_defense, data = voters)
  feols_fit_2 <- feols(proposition_vote ~ treatment + log_income | Q1_immigration + Q2_defense, data = voters_1)
  lfe_fit <- lfe::felm(proposition_vote ~ treatment + log_income | Q1_immigration + Q2_defense, data = voters)
  lfe_fit_2 <- lfe::felm(proposition_vote ~ treatment + log_income | Q1_immigration + Q2_defense, data = voters_1)

  boot1 <-
    suppressWarnings(boottest(feols_fit,
      clustid = c("Q1_immigration"),
      B = 9999,
      param = "treatment",
      bootcluster = "min"
    ))
  boot2 <-
    suppressWarnings(boottest(feols_fit_2,
      clustid = c("Q1_immigration"),
      B = 9999,
      param = "treatment",
      bootcluster = "min"
    ))
  boot3 <-
    suppressWarnings(boottest(lfe_fit,
      clustid = c("Q1_immigration"),
      B = 9999,
      param = "treatment",
      bootcluster = "min"
    ))
  boot4 <-
    suppressWarnings(boottest(lfe_fit_2,
      clustid = c("Q1_immigration"),
      B = 9999,
      param = "treatment",
      bootcluster = "min"
    ))

  expect_equal(boot1$p_val, boot2$p_val)
  expect_equal(boot2$p_val, boot3$p_val)
  expect_equal(boot3$p_val, boot4$p_val)
  expect_equal(boot4$p_val, boot1$p_val)



  # What if a fixed effect is a character?


  data(voters)

  to_char <- c("Q1_immigration", "Q2_defense", "group_id1")
  sapply(voters[, to_char], class)

  voters_1 <<- voters
  voters_1$Q1_immigration <- as.character(voters_1$Q1_immigration)
  voters_1$Q2_defense <- as.character(voters_1$Q2_defense)

  sapply(voters_1[, to_char], class)

  feols_fit <- feols(proposition_vote ~ treatment + log_income | Q1_immigration + Q2_defense, data = voters)
  feols_fit_2 <- feols(proposition_vote ~ treatment + log_income | Q1_immigration + Q2_defense, data = voters_1)
  lfe_fit <- lfe::felm(proposition_vote ~ treatment + log_income | Q1_immigration + Q2_defense, data = voters)
  lfe_fit_2 <- lfe::felm(proposition_vote ~ treatment + log_income | Q1_immigration + Q2_defense, data = voters_1)

  boot1 <-
    suppressWarnings(boottest(feols_fit,
      clustid = c("Q1_immigration"),
      B = 9999,
      param = "treatment",
      bootcluster = "min"
    ))
  boot2 <-
    suppressWarnings(boottest(feols_fit_2,
      clustid = c("Q1_immigration"),
      B = 9999,
      param = "treatment",
      bootcluster = "min"
    ))
  boot3 <-
    suppressWarnings(boottest(lfe_fit,
      clustid = c("Q1_immigration"),
      B = 9999,
      param = "treatment",
      bootcluster = "min"
    ))
  boot4 <-
    suppressWarnings(boottest(lfe_fit_2,
      clustid = c("Q1_immigration"),
      B = 9999,
      param = "treatment",
      bootcluster = "min"
    ))

  expect_equal(boot1$p_val, boot2$p_val)
  expect_equal(boot2$p_val, boot3$p_val)
  expect_equal(boot3$p_val, boot4$p_val)
  expect_equal(boot4$p_val, boot1$p_val)


  # Test 4 with fe = ON in suppressWarnings(boottest()

  data(voters)

  to_char <- c("Q1_immigration", "Q2_defense", "group_id1")
  voters$group_id1 <- as.factor(voters$group_id1)
  sapply(voters[, to_char], class)

  voters_1 <<- voters
  voters_1$Q1_immigration <- as.numeric(voters_1$Q1_immigration)
  voters_1$Q2_defense <- as.numeric(voters_1$Q2_defense)

  sapply(voters_1[, to_char], class)

  feols_fit <- feols(proposition_vote ~ treatment + log_income | Q1_immigration + Q2_defense, data = voters)
  feols_fit_2 <- feols(proposition_vote ~ treatment + log_income | Q1_immigration + Q2_defense, data = voters_1)
  lfe_fit <- lfe::felm(proposition_vote ~ treatment + log_income | Q1_immigration + Q2_defense, data = voters)
  lfe_fit_2 <- lfe::felm(proposition_vote ~ treatment + log_income | Q1_immigration + Q2_defense, data = voters_1)

  boot1 <-
    suppressWarnings(boottest(feols_fit,
      clustid = c("Q1_immigration"),
      B = 9999,
      fe = "Q2_defense",
      param = "treatment",
      bootcluster = "min"
    ))
  boot2 <-
    suppressWarnings(boottest(feols_fit_2,
      clustid = c("Q1_immigration"),
      B = 9999,
      fe = "Q2_defense",
      param = "treatment",
      bootcluster = "min"
    ))
  boot3 <-
    suppressWarnings(boottest(lfe_fit,
      clustid = c("Q1_immigration"),
      B = 9999,
      fe = "Q2_defense",
      param = "treatment",
      bootcluster = "min"
    ))
  boot4 <-
    suppressWarnings(boottest(lfe_fit_2,
      clustid = c("Q1_immigration"),
      B = 9999,
      fe = "Q2_defense",
      param = "treatment",
      bootcluster = "min"
    ))

  expect_equal(boot1$p_val, boot2$p_val)
  expect_equal(boot2$p_val, boot3$p_val)
  expect_equal(boot3$p_val, boot4$p_val)
  expect_equal(boot4$p_val, boot1$p_val)
})