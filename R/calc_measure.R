#' @import dplyr tools ggplot2
NULL

#' Create a data frame filled with the difference and denominator
#' of both the analysis sample and pilot sample.
#' It also contains pilot vs outcome correlation.
#' This is done for all the covariates.
#'
#' @param df data.frame of the original data
#' @param covariates vector of strings or list denoting column names of interest
#' @param treatment string denoting the name of the treatment variable
#' @param outcome string denoting the name of the outcome variable
#' @param pilot_prop proportion denoted as the pilot sample
#' @param seed random number to control for randomness, make it reproducible
#' @param use_abs boolean to denote whether absolute values are used to construct the measures
get_measures = function(df, covariates, treatment, outcome,
                        pilot_prop=0.2, seed = 2521323,
                        use_abs=F){
  set.seed(seed=seed)
  pilot_sample_num = sample(which(df %>% pull(treatment) == 0),
                            length(which(df %>% pull(treatment) == 0)) *
                              pilot_prop)
  pilot_sample = df[pilot_sample_num, ]
  analysis_sample = df[-pilot_sample_num, ]

  # propensity score
  props = get_props(df=df, covariates=covariates,
                    treatment=treatment,
                    pilot_sample_num=pilot_sample_num)

  # prognostic score
  progs = get_progs(df=df, covariates=covariates,
                     outcome=outcome, pilot_sample_num=pilot_sample_num)

  # set scores
  pilot_sample$prognostic_score = progs$progs[pilot_sample_num]
  analysis_sample$prognostic_score = progs$progs[-pilot_sample_num]

  pilot_sample$propensity_score = props$props[pilot_sample_num]
  analysis_sample$propensity_score = props$props[-pilot_sample_num]

  # standardized differences
  # in the analysis sample
  analysis_diff = apply(analysis_sample[, !(names(analysis_sample) %in% c(treatment, outcome))],
                        2,
                        get_diff,
                        analysis_sample = analysis_sample,
                        treatment = treatment)
  analysis_denom = apply(
    analysis_sample[, !(names(analysis_sample) %in% c(treatment, outcome))],
    2,
    get_analysis_denom,
    analysis_sample = analysis_sample,
    treatment = treatment
  )
  pilot_denom = apply(pilot_sample[, !(names(pilot_sample) %in% c(treatment, outcome))], 2, get_pilot_denom)

  # pearson coorrelation
  # in the control sample
  pilot_cor = apply(pilot_sample[, !(names(pilot_sample) %in% c(treatment, outcome))],
                    2,
                    get_pilot_cor,
                    pilot_sample = pilot_sample,
                    outcome = outcome)


  if (use_abs) {
    analysis_diff = abs(analysis_diff)
    pilot_cor = abs(pilot_cor)
  }

  standard_difference = analysis_diff / analysis_denom
  standard_difference_pilot = analysis_diff / pilot_denom
  control_cor = pilot_cor
  bias_std_diff_analysis_denom = # stats::sd(pilot_sample %>% pull(outcome)) *
    # removing this means that we are looking at the bias curves
    # where 0.005 "bias" is bias measured divided by standard deviation of the outcome
    # note this is okay because it is the same for every variable
    pilot_cor *
    standard_difference
  bias_std_diff_pilot_denom = # stats::sd(pilot_sample %>% pull(outcome)) *
    pilot_cor *
    standard_difference_pilot
  # pairwise min
  min_denom = pmin(bias_std_diff_analysis_denom, bias_std_diff_pilot_denom)
  standard_difference_max = analysis_diff / min_denom
  bias_max = # stats::sd(pilot_sample %>% pull(outcome)) *
    pilot_cor *
    standard_difference_max

  measures = data.frame(
    standard_difference = standard_difference,
    standard_difference_pilot = standard_difference_pilot,
    control_cor = control_cor,
    bias_std_diff_analysis_denom = # stats::sd(pilot_sample %>% pull(outcome)) *
      # removing this means that we are looking at the bias curves
      # where 0.005 "bias" is bias measured divided by standard deviation of the outcome
      # note this is okay because it is the same for every variable
      bias_std_diff_analysis_denom,
    bias_std_diff_pilot_denom = # stats::sd(pilot_sample %>% pull(outcome)) *
      bias_std_diff_pilot_denom,
    # pairwise max
    standard_difference_max = pmax(standard_difference, standard_difference_pilot),
    bias_max = # stats::sd(pilot_sample %>% pull(outcome)) *
      pmax(bias_std_diff_analysis_denom, bias_std_diff_pilot_denom),
    label = names(pilot_cor)
  )


  progs$progs_plot = ggplot(data.frame(prognostic_score = analysis_sample$prognostic_score), aes_q(quote(prognostic_score),
                                         fill = factor(analysis_sample %>% pull(treatment)))) +
    geom_histogram(alpha = 0.5,
                   binwidth = 0.05,
                   position = "identity") +
    theme_minimal() +
    scale_fill_discrete(name = toTitleCase(tolower(as.character(treatment))),
                        labels = c("control", "treatment")) +
    labs(x = "Prognostic score",
         y = "Count",
         title = "Prognostic score comparison")

  props$props_plot = ggplot(data.frame(propensity_score = analysis_sample$propensity_score), aes_q(quote(propensity_score),
                                                                                                   fill = factor(analysis_sample %>% pull(treatment)))) +
    geom_histogram(alpha = 0.5,
                   binwidth = 0.1,
                   position = "identity") +
    theme_minimal() +
    scale_fill_discrete(name = toTitleCase(tolower(as.character(treatment))),
                        labels = c("control", "treatment")) +
    labs(x = "Propensity score",
         y = "Count",
         title = "Propensity score comparison")

  return(list('measures'=measures, 'props'=props, 'progs'=progs))
}


#' Get standardized difference in analysis sample
#'
#' @param covariate covariate, note it is each column of the sample
#' @param analysis_sample sample for analysis containing treatment and control
#' @param treatment string denoting the name of the treatment variable
get_diff <- function(covariate, analysis_sample, treatment) {
  difference = mean(covariate[analysis_sample %>% pull(treatment) == 1]) - mean(covariate[analysis_sample %>% pull(treatment) == 0])
  return(difference)
}

#' Get standardized difference in analysis sample
#' @inheritParams get_diff
get_analysis_denom <- function(covariate, analysis_sample, treatment) {
  denom = sqrt(stats::var(covariate[analysis_sample %>% pull(treatment) == 1])/2 +
                 stats::var(covariate[analysis_sample %>% pull(treatment) == 0])/2)
  return(denom)
}

#' Get correlation in pilot sample
#'
#' @param covariate covariate, note it is each column of the sample
#' @param pilot_sample sample for analysis containing only control
#' @param outcome string denoting the name of the outcome variable
get_pilot_cor <- function(covariate, pilot_sample, outcome) {
  pilot_cor = stats::cor(covariate, pilot_sample %>% pull(outcome))
  return(pilot_cor)
}

#' Get denominator using the pilot sample
#' this is done by getting the standard deviation of the variable.
#'
#' @param covariate covariate, note it is each column of the sample
get_pilot_denom <- function(covariate) {
  denom = stats::sd(covariate)
  return(denom)
}


#' Plot propensity score comparison between treatment and control
#' and get propensity score.
#'
#'
#' @param df data.frame of the original data
#' @param covariates vector of strings or list denoting column names of interest
#' @param treatment string denoting the name of the treatment variable
#' @param pilot_sample_num vector of numbers indicating which samples are denoted as the pilot sample
get_props <- function(df,
                      covariates,
                      treatment,
                      pilot_sample_num) {

  ## unsure if this should be trained on analysis sample only
  ## or entire sample used analysis sample only for training
  ## implemented entire sample
  fmla = stats::as.formula(paste(paste(treatment, "~ "),
                                 paste(".", collapse = "+")))

  glm_prop_score <- stats::glm(fmla, family = stats::binomial(),
                               data = df[-pilot_sample_num, c(covariates, treatment)])

  prop_scores = data.frame(
    prop_score = stats::predict(glm_prop_score,
                                df, type = "response")
  )

  return(list('props' = prop_scores$prop_score,
              'props_fit' = glm_prop_score))
}

#' Plot prognostic score histogram and get prognostic score.
#'
#'
#' @param df data.frame of the original data
#' @param covariates vector of strings or list denoting column names of interest
#' @param outcome string denoting the name of the outcome variable
#' @param pilot_sample_num vector of numbers indicating which samples are denoted as the pilot sample
get_progs <- function(df, covariates, outcome, pilot_sample_num) {
  fmla = stats::as.formula(paste(paste(outcome, "~ "),
                                 paste(".", collapse = "+")))
  family = if (length(unique(df %>% pull(outcome))) != 2){stats::gaussian()}
  else{stats::binomial()}

  glm_prog_score <- stats::glm(fmla, family = family,
                               data = df[pilot_sample_num, c(covariates, outcome)])

  prog_scores = data.frame(prog_score = stats::predict(glm_prog_score,
                                                       df, type = "response"))
  # unsure if this is needed
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  prog_scores = range01(prog_scores)
  return(list('progs' = prog_scores$prog_score,
              'progs_fit' = glm_prog_score))
}
