## code to prepare `brfss_og` dataset goes here

library(usethis)
library(fastDummies)
library(dplyr)

## using brfss example
brfss_og <-
  read.csv("http://static.lib.virginia.edu/statlab/materials/data/brfss_2015_sample.csv")
## data cleaning
brfss_og$COPD = ifelse(factor(brfss_og$COPD) == 'No', 0, 1) # reference is no
brfss_og$RACE = factor(brfss_og$RACE)
brfss_og$RACE <-
  relevel(brfss_og$RACE, ref = 'White') # reference is majority in data
brfss_og$AGE = factor(brfss_og$AGE)
brfss_og$SEX = ifelse(factor(brfss_og$SEX) == 'Female', 0, 1) # reference is majority in data
## dichotimize the variables
brfss_og = dummy_cols(brfss_og)
df = (brfss_og[, !(names(brfss_og) %in% c("RACE", "AGE"))])
outcome = 'COPD'
treatment = 'SMOKE'
covariates = names(brfss_og)[!names(brfss_og) %in% c(outcome, treatment)]
## cleaned data
df = (brfss_og[, !(names(brfss_og) %in% c("RACE", "AGE"))])
names(df) <- c("COPD", "smoke", "sex", "weight",
               "average_drinks", "race_white", "race_black",
               "race_hispanic", "race_other", "age_18to24",
               "age_25to34", "age_35to44", "age_45to54",
               "age_55to64", "age_over65")
brfss = df
write.csv(brfss, "data-raw/brfss.csv")
use_data(brfss, overwrite = TRUE)
