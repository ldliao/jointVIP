library(magrittr)
library(jointVIP)
library(tidyverse)
library(MatchIt)
library(optmatch)

# dgp ---------------------------------------------------------------------

sigmoid = function(x) {
  1 / (1 + exp(-x))
}

dgp <- function(n = 1000){
  # confounders
  Xs <- tibble(
    !!!setNames(replicate(5, as.numeric(runif(n) > 0.5), simplify = FALSE), paste0("X_", 1:5))
  )
  # treatment only
  Ws <- tibble(
    !!!setNames(replicate(30, as.numeric((runif(n)-0.4) > 0.5), simplify = FALSE), paste0("W_", 1:30))
  )
  # outcome only
  Vs <- tibble(
    !!!setNames(replicate(3, as.numeric(runif(n) > 0.5), simplify = FALSE), paste0("V_", 1:3))
  )
  R_ <- tibble(
    !!!setNames(replicate(30, as.numeric(runif(n) > 0.5), simplify = FALSE), paste0("R_", 1:30))
  )
  df <- bind_cols(Xs, Ws, Vs, R_)

  # Define your customized function
  get_treatment <- function(x_columns, w_columns) {
    x_sum <- sum(x_columns, na.rm = TRUE)
    w_sum <- sum((-1)^seq_along(w_columns) * w_columns, na.rm = TRUE)
    treatment <- rbinom(1,1,sigmoid(0.2 * x_sum + 0.5 * w_sum - 3))
    return(treatment)
  }

  get_outcome <- function(x_columns, v_columns, treat) {
    x_sum <- sum(x_columns, na.rm = TRUE)
    v_sum <- sum(v_columns, na.rm = TRUE)
    outcome <- 3*v_sum - 2*x_sum + 0.5*treat + rnorm(1, sd = 0.5)
    return(outcome)
  }

  df %<>%
    rowwise() %>%
    mutate(treatment = get_treatment(across(starts_with("X_")),
                                     across(starts_with("W_")))) %>%
    mutate(outcome = get_outcome(across(starts_with("X_")),
                                 across(starts_with("V_")), treatment))
  return(data.frame(df))
}


# pilot and analysis data -------------------------------------------------

set.seed(23843)
pilot_df <- dgp(5000)
table(pilot_df$treatment)

pilot_df %<>%
  filter(treatment == 0) %>%
  slice_sample(n=4000)

## sanity check for pilot_df
dim(pilot_df)
table(pilot_df$treatment)

# set.seed(19823)
set.seed(2233)
analysis_df <- dgp(3000)
print(table(analysis_df$treatment))

# unadjusted jointVIP -----------------------------------------------------

treatment <- "treatment"
outcome <- "outcome"
covariates <- names(pilot_df)[!names(pilot_df) %in% c(treatment, outcome)]
new_jointVIP = jointVIP::create_jointVIP(treatment = treatment,
                                         outcome = outcome,
                                         covariates = covariates,
                                         pilot_df = pilot_df,
                                         analysis_df = analysis_df)

p_og = plot(new_jointVIP,
            plot_title = "Unadjusted jointVIP",
            expanded_y_curvelab = 0.09, bias_curve_cutoffs = c(0.03, 0.01, 0.005)) +
  ylim(c(0, 0.57))

# matching on every variable ----------------------------------------------

x_var_names <- grep("^X", names(analysis_df), value = TRUE)
w_var_names <- grep("^W", names(analysis_df), value = TRUE)
v_var_names <- grep("^V", names(analysis_df), value = TRUE)
r_var_names <- grep("^R", names(analysis_df), value = TRUE)
# Combine the variable names into a formula
formula_str <- paste("treatment ~", paste(c(x_var_names, w_var_names, v_var_names), collapse = " + "))

m.out_all <- matchit(
  as.formula(formula_str),
  data = analysis_df,
  method = "optimal",
  distance = "mahalanobis", estimand = "ATT"
)

optmatch_df_all <- match.data(m.out_all)[, c(treatment, outcome, covariates)]
post_optmatch_jointVIP_all <- create_post_jointVIP(new_jointVIP,
                                                   post_analysis_df = optmatch_df_all)

optmatch_df_all = as.data.frame(optmatch_df_all)
trt_all = optmatch_df_all[row.names(m.out_all$match.matrix[!is.na(m.out_all$match.matrix), ,drop = FALSE]),]$outcome
cnrl_all = optmatch_df_all[m.out_all$match.matrix[!is.na(m.out_all$match.matrix), ,drop = FALSE],]$outcome
sensitivitymw::senmwCI(trt_all - cnrl_all, method = 't', one.sided = FALSE)

# only using SMD for selection --------------------------------------------

initial_pooled_terms = get_measures(new_jointVIP, smd = "pooled") %>%
  arrange(desc(abs(std_md))) %>%
  filter(abs(std_md) > 0.1) %>%
  rownames()

m.out_pooled1 <- matchit(
  treatment ~ W_22 + W_29 + W_30 + W_15 + W_8 + W_13 + W_17 +
    W_1 + W_21 + W_19 + W_16 + W_12 + W_26 + W_27 + W_11 + W_6 + W_2 +
    X_4 + R_19 + R_10 + X_3 + W_25 + W_23 + W_4,
  data = analysis_df,
  method = "optimal",
  distance = "mahalanobis", estimand = "ATT"
)

testthat::expect_equal(initial_pooled_terms, (attr(terms(treatment ~ W_22 + W_29 + W_30 + W_15 + W_8 + W_13 + W_17 +
                                                           W_1 + W_21 + W_19 + W_16 + W_12 + W_26 + W_27 + W_11 + W_6 + W_2 +
                                                           X_4 + R_19 + R_10 + X_3 + W_25 + W_23 + W_4) , "term.labels")))

optmatch_df_pooled1 <- match.data(m.out_pooled1)[, c(treatment, outcome, covariates)]
post_optmatch_jointVIP_pooled1 <- create_post_jointVIP(new_jointVIP,
                                                       post_analysis_df = optmatch_df_pooled1)

trt_pooled1 = analysis_df[row.names(m.out_pooled1$match.matrix[!is.na(m.out_pooled1$match.matrix), ,drop = FALSE]),]$outcome
cnrl_pooled1 = analysis_df[m.out_pooled1$match.matrix[!is.na(m.out_pooled1$match.matrix), ,drop = FALSE],]$outcome
sensitivitymw::senmwCI(trt_pooled1 - cnrl_pooled1, method = 't', one.sided = FALSE)

extra_terms = get_post_measures(post_optmatch_jointVIP_pooled1, smd = "pooled") %>%
  arrange(desc(abs(post_std_md))) %>%
  filter(abs(post_std_md) > 0.1) %>%
  rownames()

m.out_pooled2 <- matchit(
  treatment ~ R_30+W_10+R_1+R_28+W_5+R_11+W_20+W_8+R_14+
    R_27+W_24+W_12+X_4+W_9+R_2+W_22+W_29+W_30+
    W_15+W_13+W_17+W_1+W_21+W_19+W_16+W_26+W_27+
    W_11+W_6+W_2+R_19+R_10+X_3+W_25+W_23+W_4,
  data = analysis_df,
  method = "optimal",
  distance = "mahalanobis", estimand = "ATT"
)

testthat::expect_equal(unique(c(extra_terms, initial_pooled_terms)),
                       (attr(terms(treatment ~ R_30+W_10+R_1+R_28+W_5+R_11+W_20+W_8+R_14+
                                     R_27+W_24+W_12+X_4+W_9+R_2+W_22+W_29+W_30+
                                     W_15+W_13+W_17+W_1+W_21+W_19+W_16+W_26+W_27+
                                     W_11+W_6+W_2+R_19+R_10+X_3+W_25+W_23+W_4) , "term.labels")))


optmatch_df_pooled2 <- match.data(m.out_pooled2)[, c(treatment, outcome, covariates)]
post_optmatch_jointVIP_pooled2 <- create_post_jointVIP(new_jointVIP,
                                                       post_analysis_df = optmatch_df_pooled2)


extra_terms2 = get_post_measures(post_optmatch_jointVIP_pooled2, smd = "pooled") %>%
  arrange(desc(abs(post_std_md))) %>%
  filter(abs(post_std_md) > 0.1) %>%
  rownames()

unique(c(extra_terms2, extra_terms, initial_pooled_terms)) == unique(c(extra_terms2, extra_terms, initial_pooled_terms))

trt_pooled2 = analysis_df[row.names(m.out_pooled2$match.matrix[!is.na(m.out_pooled2$match.matrix), ,drop = FALSE]),]$outcome
cnrl_pooled2 = analysis_df[m.out_pooled2$match.matrix[!is.na(m.out_pooled2$match.matrix), ,drop = FALSE],]$outcome
sensitivitymw::senmwCI(trt_pooled2 - cnrl_pooled2, method = 't', one.sided = FALSE)


# using jointVIP for design -----------------------------------------------

# print(new_jointVIP, bias_tol = 0.01)

# initial match
m.out_1 <- matchit(
  treatment ~ X_4 + X_3 + V_2 + X_2 + X_5 + V_3 ,
  data = analysis_df,
  method = "optimal",
  distance = "mahalanobis", estimand = "ATT"
)

optmatch_df_1 <- match.data(m.out_1)[, c(treatment, outcome, covariates)]
post_optmatch_jointVIP_1 <- create_post_jointVIP(new_jointVIP,
                                                 post_analysis_df = optmatch_df_1)


trt_1 = analysis_df[row.names(m.out_1$match.matrix[!is.na(m.out_1$match.matrix), ,drop = FALSE]),]$outcome
cnrl_1 = analysis_df[m.out_1$match.matrix[!is.na(m.out_1$match.matrix),,drop = FALSE],]$outcome
mean(trt_1 - cnrl_1)
sensitivitymw::senmwCI(trt_1 - cnrl_1, method = 't', one.sided = FALSE)

## double check
get_post_measures(post_optmatch_jointVIP_1) %>%
  arrange(desc(abs(post_bias))) %>%
  filter(abs(post_bias) >= 0.01) %>%
  rownames()

p_1 <- plot(post_optmatch_jointVIP_1,
            plot_title = "First iteration post-match jointVIP",
            expanded_y_curvelab = 0.09, bias_curve_cutoffs = c(0.03, 0.01, 0.005)) +
  ylim(c(0, 0.57))


# second match
m.out_2 <- matchit(
  treatment ~ X_4 + X_3 + V_2 + X_2 + X_5 + V_3 + X_1,
  data = analysis_df,
  method = "optimal",
  distance = "mahalanobis", estimand = "ATT"
)

optmatch_df_2 <- match.data(m.out_2)[, c(treatment, outcome, covariates)]
post_optmatch_jointVIP_2 <- create_post_jointVIP(new_jointVIP,
                                                 post_analysis_df = optmatch_df_2)

trt_2 = analysis_df[row.names(m.out_2$match.matrix[!is.na(m.out_2$match.matrix), ,drop = FALSE]),]$outcome
cnrl_2 = analysis_df[m.out_2$match.matrix[!is.na(m.out_2$match.matrix), ,drop = FALSE],]$outcome

# sensitivitymv::senmvCI(trt - cnrl, method = 't', gamma = 1)
sensitivitymw::senmwCI(trt_2 - cnrl_2, method = 't', one.sided = FALSE)

## double check
get_post_measures(post_optmatch_jointVIP_2) %>%
  arrange(desc(abs(post_bias))) %>%
  filter(abs(post_bias) >= 0.01) %>%
  rownames()

p_2 <- plot(post_optmatch_jointVIP_2,
            plot_title = "Second iteration post-match jointVIP",
            expanded_y_curvelab = 0.09, bias_curve_cutoffs = c(0.03, 0.01, 0.005)) +
  ylim(c(0, 0.57))


# third match
m.out_3 <- matchit(
  treatment ~ X_4 + X_3 + V_2 + X_2 + X_5 + V_3 + X_1 + V_1,
  data = analysis_df,
  method = "optimal",
  distance = "mahalanobis", estimand = "ATT"
)

optmatch_df_3 <- match.data(m.out_3)[, c(treatment, outcome, covariates)]
post_optmatch_jointVIP_3 <- create_post_jointVIP(new_jointVIP,
                                                 post_analysis_df = optmatch_df_3)

## double check
get_post_measures(post_optmatch_jointVIP_3) %>%
  arrange(desc(abs(post_bias))) %>%
  filter(abs(post_bias) >= 0.01) %>%
  rownames()


p_3 <- plot(post_optmatch_jointVIP_3,
            plot_title = "Third iteration post-match jointVIP",
            expanded_y_curvelab = 0.09, bias_curve_cutoffs = c(0.03, 0.01, 0.005)) +
  ylim(c(0, 0.57))


trt_3 = analysis_df[row.names(m.out_3$match.matrix[!is.na(m.out_3$match.matrix), ,drop = FALSE]),]$outcome
cnrl_3 = analysis_df[m.out_3$match.matrix[!is.na(m.out_3$match.matrix), ,drop = FALSE],]$outcome

sensitivitymw::senmwCI(trt_3 - cnrl_3, method = 't', one.sided = FALSE)

# the difference here is just 0 because we got 0.5 exactly
## obtain the post standardized mean difference
get_post_measures(post_optmatch_jointVIP_3, smd = "pooled") %>%
  arrange(desc(abs(post_std_md))) %>%
  filter(abs(post_std_md) > 0.1) %>%
  rownames()


# graph them with cowplot -------------------------------------------------
library(cowplot)

combined_p = plot_grid(p_og, p_1, p_2, p_3, ncol = 2, labels = c('A', 'B', 'C', 'D'), label_size = 15)
ggsave(plot = combined_p,
       filename = "combined_all.pdf",
       width = 12, height = 8, dpi = 600)
