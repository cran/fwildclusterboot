## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fwildclusterboot)

## -----------------------------------------------------------------------------
library(fixest)
library(lfe)

# load data set voters included in fwildclusterboot
data(voters)

# estimate the regression model via lm
lm_fit <- lm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , data = voters)

# estimate the regression model via feols
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , data = voters)

# estimate the regression model via felm
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income + Q1_immigration , data = voters)

# model with interaction
lm_fit_interact <- lm(proposition_vote ~ treatment + ideology1 + log_income:Q1_immigration , data = voters)


## -----------------------------------------------------------------------------
# boottest on an object of type lm
boot_lm <- boottest(lm_fit, clustid = "group_id1", param = "treatment", B = 9999)
# boottest on an object of type fixest
boot_feols <- boottest(feols_fit, clustid = "group_id1", param = "treatment", B = 9999)
# boottest on an object of type felm
boot_felm <- boottest(felm_fit, clustid = "group_id1", param = "treatment", B = 9999)

## -----------------------------------------------------------------------------
names(coef(lm_fit_interact))
boot_lm_interact <- boottest(lm_fit_interact, clustid = "group_id1", param = "log_income:Q1_immigration1", B = 9999)

## -----------------------------------------------------------------------------
library(modelsummary)

# fwildclusterboot's internal summary() method
summary(boot_lm)

# summary via the modelsummary package
msummary(list(boot_lm, boot_feols), 
          estimate = "{estimate} ({p.value})", 
         statistic = "[{conf.low}, {conf.high}]")


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

msummary(list(boot_lm_rade, boot_lm_webb), 
          estimate = "{estimate} ({p.value})", 
         statistic = "[{conf.low}, {conf.high}]")


## -----------------------------------------------------------------------------
boot_lm_5 <- boottest(lm_fit, 
                    clustid = c("group_id1"),
                    param = "treatment", B = 9999, 
                    sign_level = 0.05)
boot_lm_10 <- boottest(lm_fit, 
                    clustid = c("group_id1"),
                    param = "treatment", B = 9999, 
                    sign_level = 0.10)

msummary(list(boot_lm_5, boot_lm_10), 
          estimate = "{estimate} ({p.value})", 
         statistic = "[{conf.low}, {conf.high}]")

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

msummary(list(boot_lm1, boot_lm2), 
          estimate = "{estimate} ({p.value})", 
         statistic = "[{conf.low}, {conf.high}]")

## -----------------------------------------------------------------------------
# estimate the regression model via feols
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration , data = voters)

# estimate the regression model via felm
felm_fit <- felm(proposition_vote ~ treatment + ideology1 + log_income | Q1_immigration , data = voters)

boot_feols <- boottest(feols_fit, 
                       clustid = "group_id1", 
                       param = "treatment", 
                       B = 9999, 
                       fe = "Q1_immigration")

boot_felm <- boottest(felm_fit, 
                      clustid = "group_id1",
                      param = "treatment", 
                      B = 9999, 
                      fe = "Q1_immigration")


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

msummary(model = list(boot_min, boot_2var), 
         estimate = "{estimate} ({p.value})", 
         statistic = "[{conf.low}, {conf.high}]", 
         conf_int = 0.05)




## -----------------------------------------------------------------------------
# regression with weights / WLS
feols_fit <- feols(proposition_vote ~ treatment + ideology1 + log_income, weights = voters$weights, data = voters)

boot_feols <- boottest(feols_fit, 
                       clustid = "group_id1", 
                       param = "treatment", 
                       B = 9999)


## -----------------------------------------------------------------------------
boot_feols <- boottest(feols_fit, 
                       clustid = "group_id1", 
                       param = "treatment", 
                       B = 9999, 
                       nthreads = 2)

