# showcase different weighting methods can be used with jointVIP
# working with brfss example
library(jointVIP)
library(dplyr)
library(tableone)
library(knitr)
library(optmatch)
df = read.csv("../../Data/JointVIP/sm_brfss_example.csv")
if(all(df[,1] == 1:nrow(df))){df <- df[,-c(1)]}


# 1.0. SBW ----------------------------------------------------------------

# Define treatment indicator and
t_ind = "smoke"
# moment covariates
bal = list()
bal$bal_cov = c('age_over65', 'age_25to34',
                'age_35to44', 'race_black',
                'average_drinks')
# Set tolerances
bal$bal_tol = c(0.104460060, 0.128696894,
                0.155178052, 0.190584344,
                3.546026641)
bal$bal_std = "group" # unsure about this parameter?

# Solve for the Average Treatment Effect on the Treated, ATT (default)
bal$bal_alg = FALSE
sbwatt_object = sbw(dat = df, ind = t_ind, out = "COPD", bal = bal)
# # Solve for a Conditional Average Treatment Effect, CATE
# sbwcate_object = sbw(dat = data_frame, ind = t_ind, out = "Y", bal = bal,
# sol = list(sol_nam = "quadprog"), par = list(par_est = "cate", par_tar = "X1 > 1 & X3 <= 0.22"))
# # Solve for the population mean, POP
# tar = colMeans(bal_cov)
# names(tar) = bal$bal_cov
# sbwpop_object = sbw(dat = data_frame, ind = t_ind, out = "Y", bal = bal,
# sol = list(sol_nam = "quadprog"), par = list(par_est = "pop"))
# # Solve for a target population mean, AUX
# sbwaux_object = sbw(dat = data_frame, bal = bal,
# sol = list(sol_nam = "quadprog"), par = list(par_est = "aux", par_tar = tar*1.05))
# # Solve for the ATT using the tuning algorithm
# bal$bal_alg = TRUE
# bal$bal_sam = 1000
# sbwatttun_object = sbw(dat = data_frame, ind = t_ind, out = "Y", bal = bal,
# sol = list(sol_nam = "quadprog"), par = list(par_est = "att", par_tar = NULL))
# Check
summarize(sbwatt_object)
# summarize(sbwcate_object)
# summarize(sbwpop_object)
# summarize(sbwaux_object)
# summarize(sbwatttun_object)
# Estimate
estimate(sbwatt_object)
# estimate(sbwcate_object)
# estimate(sbwpop_object)
# estimate(sbwatttun_object)
# Visualize
visualize(sbwatt_object)
# visualize(sbwcate_object)
# visualize(sbwpop_object)
# visualize(sbwaux_object)
# visualize(sbwatttun_object)


# 2.0. Weightit -----------------------------------------------------------

library("cobalt")

bal.tab(smoke ~ age_over65 + age_25to34 + age_35to44 + race_black + average_drinks,
        data = df, estimand = "ATT", m.threshold = .05)

library("WeightIt")


# 2.1. IPW ----------------------------------------------------------------

# inverse probability weights from propensity scores generated through logistic regression
# ps for propensity score weighting
W.out <- weightit(smoke ~ age_over65 + age_25to34 + age_35to44 + race_black + average_drinks,
                  data = df, estimand = "ATT", method = "ps")
W.out
summary(W.out)
bal.tab(W.out, m.threshold = .05, disp.v.ratio = TRUE)

# 2.2. Entropy balance ----------------------------------------------------

# entropy balancing,
# which guarantees perfect balance on specified moments
# of the covariates while minimizing the entropy
# (a measure of dispersion) of the weights

W.out <- weightit(smoke ~ age_over65 + age_25to34 + age_35to44 + race_black + average_drinks,
                  data = df, estimand = "ATT", method = "ebal")

summary(W.out)

bal.tab(W.out, m.threshold = .05, disp.v.ratio = TRUE)

library(survey)
# Independent Sampling design (with replacement)
d.w <- svydesign(~1, weights = W.out$weights,
                 data = df)
fit <- svyglm(COPD ~ smoke, design = d.w)
coef(fit)
# Robust standard errors and confidence intervals
summary(fit)
confint(fit)

# unsure why this didnt work
# library("boot")
# est.fun <- function(data, index) {
#   W.out <- weightit(smoke ~ age_over65 + age_25to34 + age_35to44 + race_black + average_drinks,
#                     data = data[index,], estimand = "ATT", method = "ebal")
#   fit <- glm(COPD ~ smoke, data = data[index,], weights = W.out$weights)
#   return(coef(fit)["treat"])
# }
# boot.out <- boot(est.fun, data = df, R = 999)
# boot.ci(boot.out, type = "bca")


# 3.0. opt weight aka sbw ----------------------------------------------

#Balancing covariates with respect to race (multinomial)
(W2 <- weightit(smoke ~ age_over65 + age_25to34 + age_35to44 + race_black + average_drinks,
                data = df,
                method = "optweight", estimand = "ATE",
                tols = .01))
summary(W2)
bal.tab(W2)
