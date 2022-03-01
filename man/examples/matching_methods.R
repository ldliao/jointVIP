# showcase different matching methods can be used with jointVIP
# working with brfss example
library(jointVIP)
library(dplyr)
library(tableone)
library(knitr)
library(optmatch)

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

# coarsen continuous
df$weight_cat <- df$weight > median(df$weight)
df$average_drinks_cat <- df$average_drinks > mean(df$average_drinks)

cat_only_covs = c(covariates[!covariates %in% c('average_drinks', 'weight')], 'average_drinks_cat', 'weight_cat')
df$pilot_sample_indi = ifelse(row.names(df) %in% pilot_sample_num,
                              'pilot',
                              'analysis')
write.csv(df[,c(treatment, outcome, cat_only_covs, 'pilot_sample_indi')],
          '../../Data/JointVIP/sm_brfss_cat_only.csv')

sm_brfss_cat = df[,c(treatment, outcome, cat_only_covs, 'pilot_sample_indi')]

# reassigning names
# adding prognostic score and propensity score into the dataframe
analysis_df$prognostic_score = res_VIP$prognostic_scores
analysis_df$propensity_score = res_VIP$propensity_scores

# create tolerance dataframe ordered by bias
tol_df = res_VIP$measures[order(res_VIP$measures$bias_std_diff_pilot,
                                decreasing = T),
                          c('label' ,'tol_suggest')]


# 1.0. Cardinality matching -----------------------------------------------

library(designmatch)


# 1.1. step 1 -------------------------------------------------------------
# use cardinality matching to find the largest sample of matched pairs for which
# all the covariates are finely balanced.

# Treatment indicator; note that the data needs to be sorted in decreasing order
analysis_df = analysis_df[order(analysis_df$smoke, decreasing = T),]

# according to this treatment indicator
t_ind = analysis_df$smoke

# # grabbing top 6 to match on
# mom_covs = analysis_df[, c(head(tol_df)$label)]
# mom_tols = head(tol_df$tol_suggest)

# mom_covs = analysis_df[, c('age_over65',
#                            'age_25to34',
#                            'race_black',
#                            'average_drinks')]
# mom_tols = c(7.539945e-02, 1.497689e-01,
#              1.412281e-01, 2.569560e+00)
# round(absstddif(mom_covs, t_ind, .05), 2)

mom_covs = analysis_df[, c('age_over65',
                           'age_25to34',
                           'race_black')]
mom_tols = c(7.539945e-02, 1.497689e-01,
             1.412281e-01)

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


# # Fine balance (note here we are getting an approximate solution)
# for (i in 1:ncol(mom_covs)) {
#   print(finetab(mom_covs[, i], t_id_1, c_id_1))
# }
# New treatment indicator
t_ind_2 = t_ind[c(t_id_1, c_id_1)]
table(t_ind_2)

step_1_df = analysis_df[c(t_id_1, c_id_1),]
rownames(step_1_df) = NULL
write.csv(step_1_df[, c(treatment, outcome, covariates)],
          '../../Data/JointVIP/sm_brfss_matched_card1.csv')


# 1.2. step 2 -------------------------------------------------------------
# use optimal matching (minimum distance matching) to find the (re)pairing of
# treated and control that minimizes the total sum of covariate distances between matched
# pairs. For this, use the function 'distmatch' which is a wrapper for 'bmatch'.

# New treatment indicator
t_ind_2 = t_ind[c(t_id_1, c_id_1)]
table(t_ind_2)

# make distance structure
maha.vals = mahalanobis(df[,covariates],
            colMeans(df[,covariates]),
            cov(df[,covariates]),
            tol = 1e-22)


# To build the distance matrix, the idea is to use strong predictors of the outcome
dist_mat_2 = abs(outer(maha.vals[t_id_1], maha.vals[c_id_1], "-"))
dim(dist_mat_2)
# Match
out_2 = distmatch(t_ind_2, dist_mat_2, solver)
# Indices of the treated units and matched controls
t_id_2 = t_id_1[out_2$t_id]
c_id_2 = c_id_1[out_2$c_id-length(out_2$c_id)]

# Covariate balance is preserved...
meantab(covs, t_ind, t_id_2, c_id_2)
# for (i in 1:ncol(fine_covs)) {
#   print(finetab(fine_covs[, i], t_id_2, c_id_2))
# }

# ... but covariate distances are reduced
distances_step_1 = sum(diag(dist_mat_2))
distances_step_2 = sum(diag(dist_mat_2[out_2$t_id, out_2$c_id-length(out_2$c_id)]))
distances_step_1
distances_step_2

# The mean difference in outcomes is the same... (?)
mean(df$COPD[t_id_1])-mean(df$COPD[c_id_1])
mean(df$COPD[t_id_2])-mean(df$COPD[c_id_2])
# ... but their standard deviation is reduced (?)
sd(df$COPD[t_id_1])-sd(df$COPD[c_id_1])
sd(df$COPD[t_id_2])-sd(df$COPD[c_id_2])

step_1_df = analysis_df[c(t_id_1, c_id_1),]
rownames(step_1_df) = NULL
write.csv(step_1_df[, c(treatment, outcome, covariates)],
          '../../Data/JointVIP/sm_brfss_matched_card1.csv')

step_2_df = df[c(t_id_2, c_id_2),]
rownames(step_2_df) = NULL
write.csv(step_2_df[, c(treatment, outcome, covariates)],
          '../../Data/JointVIP/sm_brfss_matched_card2.csv')

# 2. rcbalance ------------------------------------------------------------

library(rcbalance)

#read in dataframe

# coarsen continuous
pilot_df$weight_cat <- pilot_df$weight > median(pilot_df$weight)
pilot_df$average_drinks_cat <- pilot_df$average_drinks > mean(pilot_df$average_drinks)

analysis_df$weight_cat <- df$weight > median(df$weight)
analysis_df$average_drinks_cat <- df$average_drinks > mean(df$average_drinks)

write.csv(analysis_df[,vars_to_save],'../../Data/JointVIP/sm_brfss_cat_only.csv')


# run jointVIP here
res_VIP <- plot_jointVIP(
  pilot_df = pilot_df,
  analysis_df = analysis_df,
  treatment = treatment,
  covariates = c(covariates,
                 'weight_cat',
                 'average_drinks_cat'),
  outcome = outcome,
  use_denom = 'both',
  use_abs = T
)
res_VIP$VIP
# after running grab the analysis_df

df = analysis_df
# define fine balance levels
l1 <- c("age_over65")
l2 <- c(l1, "race_white", "age_25to34")
l3 <- c(l2, "race_black", "age_35to44")
# define variables for Mahalanobis distance
maha.vars <- c(covariates[!covariates %in% c('weight', 'average_drinks')],
               'weight_cat',
               'average_drinks_cat')

# make distance structure
my.dist <- build.dist.struct(z=df$smoke,
                             X=df[,maha.vars])

# maybe its from caliper
# check this!

# compute match
match.out <- rcbalance(my.dist, fb.list = list(l1, l2, l3),
                       treated.info = df[df$smoke == 1,],
                       control.info = df[df$smoke == 0,],
                       exclude.treated = T)

vars_to_save = names(analysis_df)[!names(analysis_df) %in% c("weight", "average_drinks")]

matched_df = analysis_df[c(which(analysis_df$smoke == 1),
                           which(analysis_df$smoke == 0)[match.out$matches]),
                vars_to_save]

matched_df = matched_df[,vars_to_save]
rownames(matched_df) = NULL
write.csv(matched_df,'../../Data/JointVIP/matched_brfss.csv')


rownames(analysis_df) = NULL
write.csv(analysis_df[,vars_to_save],'../../Data/JointVIP/sm_brfss_cat_only.csv')


sm_brfss_cat = read.csv('../../Data/JointVIP/sm_brfss_cat_only.csv')
sm_brfss_cat = sm_brfss_cat[,-1]
pilot_df = sm_brfss_cat[sm_brfss_cat$pilot_sample_indi == 'pilot',]
analysis_df = sm_brfss_cat[sm_brfss_cat$pilot_sample_indi != 'pilot',]

covariates = names(sm_brfss_cat)[!names(sm_brfss_cat) %in% c(treatment,
                                                             outcome,
                                                             "pilot_sample_indi")]
matched_df = read.csv('../../Data/JointVIP/matched_brfss.csv')


brfss_vip_match = plot_jointVIP(
  pilot_df = pilot_df,
  analysis_df = analysis_df,
  post_analysis_df = matched_df,
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
brfss_vip_match$VIP
