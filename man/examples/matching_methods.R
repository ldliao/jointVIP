# showcase different matching methods can be used with jointVIP
# working with brfss example
library(jointVIP)
library(dplyr)
library(tableone)
library(knitr)
library(optmatch)
df = read.csv("../../Data/JointVIP/sm_brfss_example.csv")
if(all(df[,1] == 1:nrow(df))){df <- df[,-c(1)]}

# 1.0. Cardinality matching -----------------------------------------------

library(designmatch)


# 1.1. step 1 -------------------------------------------------------------
# use cardinality matching to find the largest sample of matched pairs for which
# all the covariates are finely balanced.

# Treatment indicator; note that the data needs to be sorted in decreasing order
df = df[order(df$smoke),]

# according to this treatment indicator
t_ind = df$smoke

# Fine balance
fine_covs = df[, c("race_white", "age_25to34", "race_black",
                   "age_35to44", "average_drinks")]
fine = list(covs = fine_covs)

# Solver options
t_max = 60*5
solver = "glpk"
approximate = 0
solver = list(name = solver, t_max = t_max, approximate = approximate,
              round_cplex = 0, trace = 0)

# Match
out_1 = cardmatch(t_ind, mom=mean,
                  fine = fine, solver=solver)

# Indices of the treated units and matched controls
t_id_1 = out_1$t_id
c_id_1 = out_1$c_id

# Mean balance
covs = df[,c("sex", "weight", "race_white", "age_18to24", "age_over65")]
meantab(covs, t_ind, t_id_1, c_id_1)
# Fine balance (note here we are getting an approximate solution)
for (i in 1:ncol(fine_covs)) {
  print(finetab(fine_covs[, i], t_id_1, c_id_1))
}

# 1.2. step 2 -------------------------------------------------------------
# use optimal matching (minimum distance matching) to find the (re)pairing of
# treated and control that minimizes the total sum of covariate distances between matched
# pairs. For this, use the function 'distmatch' which is a wrapper for 'bmatch'.

# New treatment indicator
t_ind_2 = t_ind[c(t_id_1, c_id_1)]
table(t_ind_2)
# To build the distance matrix, the idea is to use strong predictors of the outcome
dist_mat_2 = abs(outer(df$age_over65[t_id_1], df$age_25to34[c_id_1], "-"))
dim(dist_mat_2)
# Match
out_2 = distmatch(t_ind_2, dist_mat_2, solver)
# Indices of the treated units and matched controls
t_id_2 = t_id_1[out_2$t_id]
c_id_2 = c_id_1[out_2$c_id-length(out_2$c_id)]

# Covariate balance is preserved...
meantab(covs, t_ind, t_id_2, c_id_2)
for (i in 1:ncol(fine_covs)) {
  print(finetab(fine_covs[, i], t_id_2, c_id_2))
}

# ... but covariate distances are reduced
distances_step_1 = sum(diag(dist_mat_2))
distances_step_2 = sum(diag(dist_mat_2[out_2$t_id, out_2$c_id-length(out_2$c_id)]))
distances_step_1
distances_step_2

# The mean difference in outcomes is the same...
mean(df$COPD[t_id_1])-mean(df$COPD[c_id_1])
mean(df$COPD[t_id_2])-mean(df$COPD[c_id_2])
# ... but their standard deviation is reduced
sd(df$COPD[t_id_1])-sd(df$COPD[c_id_1])
sd(df$COPD[t_id_2])-sd(df$COPD[c_id_2])


# 2. rcbalance ------------------------------------------------------------

library(rcbalance)

#read in dataframe

#define variables for Mahalanobis distance
maha.vars <- names(df)[!names(df) %in% c('COPD', 'smoke')]

#make distance structure
my.dist <- build.dist.struct(z=df$smoke, X=df[,maha.vars])
# coarsen continuous
df$weight_ct <- df$weight > median(df$weight)
df$average_drinks_ct <- df$average_drinks > mean(df$average_drinks)

# define fine balance levels
l1 <- c("age_over65")
l2 <- c(l1, "age_25to34", "race_black","average_drinks_ct")
l3 <- c(l2,"weight_ct","race_white")
l4 <- c(l3,"race_hispanic","sex")

# compute match
match.out <- rcbalance(my.dist, fb.list = list(l1,l2,l3,l4),
                       treated.info = df[df$smoke == 1,],
                       control.info = df[df$smoke == 0,],
                       exclude.treated = F)

outcome = 'COPD'
treatment = 'smoke'
covariates = names(df)[!names(df) %in% c(treatment,
                                         outcome,
                                         "weight_ct", "average_drinks_ct")]



matched_df = df[c(which(df$smoke == 1), which(df$smoke == 0)[match.out$matches]),
                ]
match_df = matched_df[,c(outcome, treatment, covariates)]
rownames(matched_df) = NULL
write.csv(matched_df,'../../Data/JointVIP/matched_brfss.csv')
