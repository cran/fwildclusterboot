## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fwildclusterboot)

## ---- error = FALSE, warning = FALSE, message = FALSE-------------------------

# load data set voters included in fwildclusterboot
data(voters)

# estimate the regression model via lm
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , data = voters)

# model with interaction
lm_fit_interact <- lm(proposition_vote ~ treatment + ideology1 + log_income:Q1_immigration , data = voters)


## -----------------------------------------------------------------------------
# boottest on an object of type lm
boot_lm <- boottest(lm_fit, clustid = "group_id1", param = "treatment", B = 9999)

## -----------------------------------------------------------------------------
#names(coef(lm_fit_interact))
boot_lm_interact <- boottest(lm_fit_interact, clustid = "group_id1", param = "log_income:Q1_immigration1", B = 9999)

## -----------------------------------------------------------------------------
# fwildclusterboot's internal summary() method
summary(boot_lm)

if(requireNamespace("modelsummary")){
  # summary via the modelsummary package
  library(modelsummary)
  msummary(list(boot_lm, boot_lm_interact), 
            estimate = "{estimate} ({p.value})", 
           statistic = "[{conf.low}, {conf.high}]")  
}



## -----------------------------------------------------------------------------
plot(boot_lm)

## -----------------------------------------------------------------------------
boot_lm <- boottest(lm_fit, clustid = c("group_id1", "group_id2"), param = "treatment", B = 9999)
summary(boot_lm)

## -----------------------------------------------------------------------------
boot_lm_rade <- boottest(lm_fit, 
                    clustid = c("group_id1", "group_id2"), 
                    param = "treatment", 
                    B = 999,
                    type = "rademacher")
boot_lm_webb <- boottest(lm_fit, 
                    clustid = c("group_id1", "group_id2"), 
                    param = "treatment", 
                    B = 999,
                    type = "webb")

if(requireNamespace("modelsummary")){
  library(modelsummary)
  msummary(list(boot_lm_rade, boot_lm_webb), 
          estimate = "{estimate} ({p.value})", 
         statistic = "[{conf.low}, {conf.high}]")
}

## -----------------------------------------------------------------------------
boot_lm_5 <- boottest(lm_fit, 
                    clustid = c("group_id1"),
                    param = "treatment", B = 9999, 
                    sign_level = 0.05)
boot_lm_10 <- boottest(lm_fit, 
                    clustid = c("group_id1"),
                    param = "treatment", B = 9999, 
                    sign_level = 0.10)

if(requireNamespace("modelsummary")){
  library(modelsummary)
  msummary(list(boot_lm_5, boot_lm_10), 
          estimate = "{estimate} ({p.value})", 
         statistic = "[{conf.low}, {conf.high}]")
}

## -----------------------------------------------------------------------------
boot_lm1 <- boottest(lm_fit, 
                    clustid = c("group_id1", "group_id2"), 
                    param = "treatment",
                    B = 9999, 
                    bootcluster = "min")

boot_lm2 <- boottest(lm_fit, 
                    clustid = c("group_id1", "group_id2"), 
                    param = "treatment",
                    B = 9999, 
                    bootcluster = "group_id1")

if(requireNamespace("modelsummary")){
  library(modelsummary)
  msummary(list(boot_lm1, boot_lm2), 
          estimate = "{estimate} ({p.value})", 
         statistic = "[{conf.low}, {conf.high}]")
}

## -----------------------------------------------------------------------------

if(requireNamespace("fixest")){
  # estimate the regression model via feols
  library(fixest)
  feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration , data = voters)
  boot_feols <- boottest(feols_fit, 
                         clustid = "group_id1", 
                         param = "treatment", 
                         B = 9999, 
                         fe = "Q1_immigration")
}


## -----------------------------------------------------------------------------
boot_min <- boottest(lm_fit,
                    clustid = c("group_id1", "group_id2"), 
                    param = "treatment", 
                    B = 9999, 
                    bootcluster = "min")
boot_var <- boottest(lm_fit,
                    clustid = c("group_id1", "group_id2"), 
                    param = "treatment", 
                    B = 9999, 
                    bootcluster = "group_id1")
boot_2var <- boottest(lm_fit,
                    clustid = c("group_id1", "group_id2"), 
                    param = "treatment", 
                    B = 9999, 
                    bootcluster = c("group_id1", "Q1_immigration"))

if(requireNamespace("modelsummary")){
  library(modelsummary)
  msummary(model = list(boot_min, boot_2var), 
         estimate = "{estimate} ({p.value})", 
         statistic = "[{conf.low}, {conf.high}]")
}



## -----------------------------------------------------------------------------
# regression with weights / WLS
lm_w_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income, weights = voters$weights, data = voters)

boot_w_lm <- boottest(lm_w_fit, 
                       clustid = "group_id1", 
                       param = "treatment", 
                       B = 9999)


## ---- eval = FALSE------------------------------------------------------------
#  boot_lm <- boottest(lm_fit,
#                         clustid = "group_id1",
#                         param = "treatment",
#                         B = 9999,
#                         nthreads = 2)

