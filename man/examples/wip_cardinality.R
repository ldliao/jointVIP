library(sbw)
library(designmatch)
library(dplyr)
library(jointVIP)

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

pilot_df$average_drinks = 100+pilot_df$average_drinks*100
analysis_df$average_drinks = 100+analysis_df$average_drinks*100


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


# 1.1. step 1 -------------------------------------------------------------
# use cardinality matching to find the largest sample of matched pairs for which
# all the covariates are finely balanced.

# Treatment indicator; note that the data needs to be sorted in decreasing order
analysis_df = analysis_df[order(analysis_df %>% pull(treatment), decreasing = T),]

# according to this treatment indicator
t_ind = analysis_df %>% pull(treatment)

# mom_covs = analysis_df[, c('average_drinks',
#                            'age_over65',
#                            'weight')]
# mom_tols = c(257, 0.0754, 167)
mom_covs = analysis_df[, c('age_over65',
                           'age_25to34')]
mom_tols = c(0.0754, 0.15)

# round(absstddif(mom_covs, t_ind, .05), 2)

# Solver options
t_max = 60*5
solver = "glpk"
approximate = 0
solver = list(name = solver,
              t_max = t_max,
              approximate = approximate,
              round_cplex = 0, trace = 0)

# Match
out_1 = cardmatch(t_ind, mom = list(covs = mom_covs,
                                    tols = mom_tols),
                  solver=solver)

# Indices of the treated units and matched controls
t_id_1 = out_1$t_id
c_id_1 = out_1$c_id


# Mean balance
covs = mom_covs
meantab(covs, t_ind, t_id_1, c_id_1)
t_ind = df$COPD
t_ind_2 = t_ind[c(t_id_1, c_id_1)]

table(t_ind_2)

step_1_df = df[c(t_id_1, c_id_1),]
rownames(step_1_df) = NULL


post_VIP <- plot_jointVIP(
  pilot_df = pilot_df,
  analysis_df = analysis_df,
  post_analysis_df = step_1_df,
  treatment = treatment,
  covariates = covariates,
  outcome = outcome,
  use_denom = 'pilot',
  post_prop = step_1_df$propensity_score,
  post_prog = step_1_df$prognostic_score,
  use_abs = T
)

post_VIP$VIP


