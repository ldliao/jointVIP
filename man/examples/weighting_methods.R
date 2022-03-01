# showcase different weighting methods can be used with jointVIP
# working with brfss example
library(jointVIP)
library(dplyr)
library(tableone)
library(knitr)
library(optmatch)
library(sbw)

df = read.csv("../../Data/JointVIP/sm_brfss_example.csv")
if(all(df[,1] == 1:nrow(df))){df <- df[,-c(1)]}

set.seed(1234567)
pilot_prop = 0.2

treatment = 'smoke'
outcome = 'COPD'
covariates = names(df)[!names(df) %in% c(treatment,
                                         outcome)]

pilot_sample_num = sample(which(df %>% pull(treatment) == 0),
                          length(which(df %>% pull(treatment) == 0)) *
                            pilot_prop)
pilot_df = df[pilot_sample_num, ]
analysis_df = df[-pilot_sample_num, ]

# run jointVIP here
res_VIP <- plot_jointVIP(
  pilot_df = pilot_df,
  analysis_df = analysis_df,
  treatment = treatment,
  covariates = covariates,
  outcome = outcome,
  use_denom = 'both',
  use_abs = T
)
res_VIP$VIP

# reassigning names
df = analysis_df
# adding prognostic score and propensity score into the dataframe
df$prognostic_score = res_VIP$prognostic_scores
df$propensity_score = res_VIP$propensity_scores

# create tolerance dataframe ordered by bias
tol_df = res_VIP$measures[order(res_VIP$measures$bias_std_diff_pilot,
                                decreasing = T),
                          c('label' ,'tol_suggest')]


# 1.0. SBW ----------------------------------------------------------------

# Define treatment indicator and
t_ind = "smoke"
# moment covariates
bal = list()
bal$bal_cov = head(tol_df$label)
# Set tolerances
bal$bal_tol = head(tol_df$tol_suggest)
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
# summarize(sbwatt_object)
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

dim(sbwatt_object$dat_weights) # 4153   18 so it did remove some

post_weighted_df = sbwatt_object$dat_weights[,covariates] * sbwatt_object$dat_weights$sbw_weights
post_weighted_df$smoke = sbwatt_object$dat_weights$smoke
post_weighted_df$COPD = sbwatt_object$dat_weights$COPD
post_weighted_df$prog_score = sbwatt_object$dat_weights$prognostic_score * sbwatt_object$dat_weights$sbw_weights
post_weighted_df$prop_score = sbwatt_object$dat_weights$propensity_score * sbwatt_object$dat_weights$sbw_weights
brfss_vip_weighted = plot_jointVIP(
  pilot_df = pilot_df,
  analysis_df = analysis_df,
  post_analysis_df = post_weighted_df,
  # brfss.match[,c(outcome, treatment, covariates)],
  treatment = treatment,
  covariates = covariates,
  outcome = outcome,
  use_abs = T,
  use_denom = 'standard',
  plot_title="Joint variable importance",
  label_cutoff_std_diff = 0.15,
  label_cutoff_control_cor = 0.1,
  post_prop = post_weighted_df$prop_score,
  post_prog = post_weighted_df$prog_score,
)
brfss_vip_weighted$VIP

write.csv(post_weighted_df,'../../Data/JointVIP/sm_brfss_sbw.csv')

# 2.0. Weightit -----------------------------------------------------------

library("cobalt")

bal.tab(smoke ~ age_over65 + age_25to34 + age_35to44 + race_black + average_drinks,
        data = analysis_df, estimand = "ATT", m.threshold = .05)

library("WeightIt")


# 2.1. IPW ----------------------------------------------------------------

# inverse probability weights from propensity scores generated through logistic regression
# ps for propensity score weighting
W.out <- weightit(smoke ~ age_over65 + age_25to34 + age_35to44 + race_black + average_drinks,
                  data = df, estimand = "ATT", method = "ps")

#Trimming at 90th percentile
(W.trim <- trim(W.out, lower= F, at = c(.9)))

summary(W.trim)
#Note that only the control weights were trimmed



post_weighted_df = analysis_df[row.names(W.trim$covs),] * W.trim$weights
post_weighted_df$smoke = W.trim$treat
post_weighted_df$COPD = analysis_df[row.names(W.trim$covs), 'COPD']
brfss_vip_weighted_ipw = plot_jointVIP(
  pilot_df = pilot_df,
  analysis_df = analysis_df,
  post_analysis_df = post_weighted_df,
  # brfss.match[,c(outcome, treatment, covariates)],
  treatment = treatment,
  covariates = covariates,
  outcome = outcome,
  use_abs = T,
  use_denom = 'standard',
  plot_title="Joint variable importance",
  label_cutoff_std_diff = 0.15,
  label_cutoff_control_cor = 0.1
)
brfss_vip_weighted_ipw$VIP
write.csv(post_weighted_df,'../../Data/JointVIP/sm_brfss_ipw.csv')

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

# Balancing covariates with respect to race (multinomial)
(W2 <- weightit(smoke ~ age_over65 + age_25to34 + age_35to44 + race_black + average_drinks,
                data = df,
                method = "optweight", estimand = "ATE",
                tols = .01))
summary(W2)
bal.tab(W2)
