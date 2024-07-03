## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
devtools::load_all(".")

## ----data_cleaning, include=FALSE---------------------------------------------
# load jointVIP package
library(jointVIP)

# data to use for example
library(causaldata)

# matching methods shown in example
library(MatchIt)
library(optmatch)
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

m.out <- matchit(
  treat ~ log_re75 + log_re74,
  data = analysis_df,
  method = "optimal",
  distance = "mahalanobis"
)
optmatch_df <- match.data(m.out)[, c(treatment, outcome, covariates)]

post_optmatch_jointVIP <- create_post_jointVIP(new_jointVIP, 
                                               post_analysis_df = optmatch_df)


## ----setup, eval=FALSE--------------------------------------------------------
#  library(jointVIP)
#  
#  # gentle reminder of how to create a new jointVIP object
#  new_jointVIP = create_jointVIP(treatment = treatment,
#                                 outcome = outcome,
#                                 covariates = covariates,
#                                 pilot_df = pilot_df,
#                                 analysis_df = analysis_df)
#  
#  # gentle reminder of how to create a new post_jointVIP object
#  post_optmatch_jointVIP = create_post_jointVIP(new_jointVIP,
#                                               post_analysis_df = optmatch_df)

## ----sumnprint----------------------------------------------------------------
# # simplest usage
# summary(new_jointVIP)

summary(new_jointVIP, 
        smd = 'standard', 
        use_abs = FALSE, 
        bias_tol = 0.005)

print(new_jointVIP, 
      smd = 'standard', 
      use_abs = FALSE, 
      bias_tol = 0.005)

# not run
# get_measures(new_jointVIP, smd = 'OVB-based')

## ----plot_ex, dpi=300, fig.asp = 0.75, fig.width = 6, out.width = "80%", fig.align = "center", message=FALSE----
# # simplest usage
# plot(new_jointVIP)

plot(new_jointVIP, 
     smd = 'standard', 
     use_abs = FALSE, 
     plot_title = 'Signed version of the jointVIP with standard/pooled SMD')

plot(new_jointVIP, 
     bias_curve_cutoffs = c(0.005, 0.05, 0.10),
     text_size = 5, 
     label_cut_std_md = 0.1,
     max.overlaps = 15,
     plot_title = 'Increased text size and bias curve specifications',
     expanded_y_curvelab = 0.002
     #label_cut_outcome_cor = 0.2,
     #label_cut_bias = 0.1
     )

plot(new_jointVIP, 
     OVB_curves = FALSE,
     add_var_labs = FALSE,
     plot_title = 'No bias curves or variable labels'
     )

## ----sumnprint_post, dpi=300, fig.asp = 0.75, fig.width = 6, out.width = "80%", fig.align = "center", message=FALSE----
# get_post_measures(post_optmatch_jointVIP, smd = 'OVB-based')

summary(post_optmatch_jointVIP,
        use_abs = FALSE,
        bias_tol = 0.01,
        post_bias_tol = 0.001)

print(post_optmatch_jointVIP,
      bias_tol = 0.001)

plot(post_optmatch_jointVIP, 
     plot_title = "Post-match jointVIP using rcbalance matching",
     smd = 'OVB-based',
     use_abs = FALSE,
     add_post_labs = TRUE,
     post_label_cut_bias = 0.001)

