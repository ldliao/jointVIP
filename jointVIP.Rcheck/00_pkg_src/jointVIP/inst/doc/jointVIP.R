## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
devtools::load_all(".")

## ----setup, message=FALSE-----------------------------------------------------
# install once its on CRAN!
# install.packages("jointVIP")

# devtools::install_github("ldliao/jointVIP")

# load jointVIP package
library(jointVIP)

# data to use for example
library(causaldata)

# matching method shown in example
library(MatchIt)
library(optmatch)

# weighting method shown in example
library(WeightIt)
library(optweight)

## ----data_cleaning------------------------------------------------------------
# load data for estimating earnings from 1978
# treatment is the NSW program
pilot_df = cps_mixtape
analysis_df = nsw_mixtape

transform_earn <- function(data, variables){
  data = data.frame(data)
  log_variables = sapply(variables, 
                         function(s){paste0('log_',s)})
  data[,log_variables] =
    apply(data[,variables], 2, 
        function(x){ifelse(x == 0, 
                           log(x + 1), 
                           log(x))})
  return(data)
}

pilot_df <- cps_mixtape
pilot_df <- transform_earn(pilot_df, c('re74', 're75', 're78'))

analysis_df <- nsw_mixtape
analysis_df <- transform_earn(analysis_df, c('re74', 're75', 're78'))

## ----jointVIP_obj-------------------------------------------------------------
treatment = 'treat'
outcome = 'log_re78'
covariates = c(names(analysis_df)[!names(analysis_df) %in% c(treatment,
                                           outcome, "data_id",
                                           "re74", "re75",
                                           "re78")])

new_jointVIP = create_jointVIP(treatment = treatment,
                               outcome = outcome,
                               covariates = covariates,
                               pilot_df = pilot_df,
                               analysis_df = analysis_df)

## ----summary------------------------------------------------------------------
summary(new_jointVIP, 
        smd = "OVB-based",
        use_abs = TRUE,
        bias_tol = 0.01)

## ----print--------------------------------------------------------------------
print(new_jointVIP,
      smd = "OVB-based",
      use_abs = TRUE,
      bias_tol = 0.01)

## ----plot, dpi=300, fig.asp = 0.75, fig.width = 6, out.width = "80%", fig.align = "center"----
plot(new_jointVIP)

## ----optmatch_example---------------------------------------------------------
# 1:1 optimal matching w/o replacement
m.out <- matchit(
  treat ~ log_re75 + log_re74,
  data = analysis_df,
  method = "optimal",
  distance = "mahalanobis"
)
optmatch_df <- match.data(m.out)[, c(treatment, outcome, covariates)]

## ----op-----------------------------------------------------------------------
# ordering for the weightit
ordered_analysis_df = analysis_df[order(analysis_df$treat, decreasing = T),]

optwt <- weightit(treat ~ log_re74 + log_re75,
                  data = ordered_analysis_df,
                  method = "optweight", estimand = "ATE",
                  tols = 0.005, include.obj = TRUE)
# summary(optwt)

optwt_df = ordered_analysis_df[row.names(optwt$covs),
                               covariates] * optwt$weights
optwt_df$treat = optwt$treat
optwt_df$log_re78 = analysis_df[row.names(optwt$covs), 'log_re78']

## ----post_opt, dpi=300, fig.asp = 0.75, fig.width = 6, out.width = "80%", fig.align = "center", message=FALSE----
post_optmatch_jointVIP <- create_post_jointVIP(new_jointVIP, 
                                               post_analysis_df = optmatch_df)

summary(post_optmatch_jointVIP)
print(post_optmatch_jointVIP)
plot(post_optmatch_jointVIP, plot_title = "Post-match jointVIP using optimal matching")

## ----post_wt, dpi=300, fig.asp = 0.75, fig.width = 6, out.width = "80%", fig.align = "center", message=FALSE----
post_optwt_jointVIP = create_post_jointVIP(new_jointVIP,
                                           post_analysis_df = optwt_df)
summary(post_optwt_jointVIP)
print(post_optwt_jointVIP)
plot(post_optwt_jointVIP, plot_title = "Post-weighting jointVIP using optimal weighting")

