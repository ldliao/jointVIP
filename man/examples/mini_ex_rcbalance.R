##### minimal example for rcbalance #####
# to load package if needed
# library(devtools)
# build()
# load_all()
# library(jointVIP)

library(rcbalance)

#read in dataframe
df = read.csv("../../Data/JointVIP/sm_brfss_example.csv")
if(all(df[,1] == 1:nrow(df))){df <- df[,-c(1)]}

set.seed(1234567)
pilot_prop = 0.2

treatment = 'smoke'
outcome = 'COPD'

pilot_sample_num = sample(which(df %>% pull(treatment) == 0),
                          length(which(df %>% pull(treatment) == 0)) *
                            pilot_prop)
pilot_df = df[pilot_sample_num, ]
analysis_df = df[-pilot_sample_num, ]

# coarsen continuous
pilot_df$weight_cat <- pilot_df$weight > median(pilot_df$weight)
pilot_df$average_drinks_cat <- pilot_df$average_drinks > mean(pilot_df$average_drinks)

analysis_df$weight_cat <- analysis_df$weight > median(analysis_df$weight)
analysis_df$average_drinks_cat <- analysis_df$average_drinks > mean(analysis_df$average_drinks)

covariates = names(analysis_df)[!names(analysis_df) %in% c(treatment,
                                         outcome,
                                         'average_drinks',
                                         'weight')]
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
# after running use analysis_df



# define fine balance levels
l1 <- c("age_over65")
l2 <- c(l1, "race_white", "age_25to34")
l3 <- c(l2, "race_black", "age_35to44")
# define variables for Mahalanobis distance
maha.vars <- covariates

# make distance structure
my.dist <- build.dist.struct(z=analysis_df$smoke,
                             X=analysis_df[,maha.vars])
                             # caliper
                             # calip.option = 'none')


# maybe its from caliper
# check this!
table(analysis_df$smoke)

# compute match
match.out <- rcbalance(my.dist, fb.list = list(l1, l2, l3),
                       treated.info = df[df$smoke == 1,],
                       control.info = df[df$smoke == 0,],
                       exclude.treated = F) # exclude.treated = T

vars_to_save = names(analysis_df)[!names(analysis_df) %in% c("weight", "average_drinks")]

matched_df = analysis_df[c(which(analysis_df$smoke == 1),
                           which(analysis_df$smoke == 0)[match.out$matches]),
                         vars_to_save]

matched_df = matched_df[,vars_to_save]
table(matched_df$smoke)
