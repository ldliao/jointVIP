#' @import dplyr tools ggplot2
NULL



#' Create a data frame filled with the difference and denominator
#' of both the analysis sample and pilot sample.
#' It also contains pilot vs outcome correlation.
#' This is done for all the covariates.
#'
#' @param pilot_df dataframe of the pilot data
#' @param analysis_df dataframe of the analysis data
#' @param covariates vector of strings or list denoting column names of interest
#' @param treatment string denoting the name of the treatment variable
#' @param outcome string denoting the name of the outcome variable
#' @param props_fit propensity score fit
#' @param progs_fit prognostic score fit
#' @param use_abs boolean to denote whether absolute values are used to construct the measures
#' @param use_denom which denom to use
get_measures_sm = function(pilot_df, analysis_df,
                           covariates, treatment, outcome,
                           props_fit, progs_fit,
                           use_abs=F, use_denom='standard'){

  # propensity score
  props_analysis = stats::predict(props_fit, analysis_df, type = "response")
  props_pilot = stats::predict(props_fit, pilot_df, type = "response")

  # props = get_props(pilot_df = pilot_df,
  #                   analysis_df = analysis_df,
  #                   covariates = covariates,
  #                   treatment=treatment)

  # prognostic score
  progs_analysis = stats::predict(progs_fit, analysis_df, type = "response")
  progs_pilot = stats::predict(progs_fit, pilot_df, type = "response")
  # progs = get_progs(pilot_df = pilot_df,
  #                   analysis_df = analysis_df,
  #                   covariates = covariates,
  #                   outcome = outcome)

  # set scores
  pilot_df$prognostic_score = progs_pilot
  pilot_df$propensity_score = props_pilot
  analysis_df$prognostic_score = progs_analysis
  analysis_df$propensity_score = props_analysis

  # standardized differences
  # in the analysis sample
  analysis_diff = apply(analysis_df[, !(names(analysis_df) %in% c(treatment, outcome))],
                        2,
                        get_diff,
                        analysis_df = analysis_df,
                        treatment = treatment)


  # pearson coorrelation
  # in the control sample
  pilot_cor = apply(pilot_df[, !(names(pilot_df) %in% c(treatment, outcome))],
                    2,
                    get_pilot_cor,
                    pilot_df = pilot_df,
                    outcome = outcome)

  if (use_abs) {
    analysis_diff = abs(analysis_diff)
    pilot_cor = abs(pilot_cor)
  }

  control_cor = pilot_cor

  if(use_denom=='standard'){
    analysis_denom = apply(
      analysis_df[, !(names(analysis_df) %in% c(treatment, outcome))],
      2,
      get_analysis_denom,
      analysis_df = analysis_df,
      treatment = treatment
    )
    standard_difference = analysis_diff / analysis_denom
    bias_std_diff_analysis = # stats::sd(pilot_sample %>% pull(outcome)) *
      # removing this means that we are looking at the bias curves
      # where 0.005 "bias" is bias measured divided by standard deviation of the outcome
      # note this is okay because it is the same for every variable
      pilot_cor *
      standard_difference
    measures = data.frame(
      raw_diff = analysis_diff,
      standard_difference = standard_difference,
      control_cor = control_cor,
      label = names(pilot_cor)
    )
  } else {
    if(use_denom=='pilot'){
      pilot_denom = apply(pilot_df[, !(names(pilot_df) %in% c(treatment, outcome))], 2, get_pilot_denom)

      standard_difference_pilot = analysis_diff / pilot_denom

      bias_std_diff_pilot = # stats::sd(pilot_sample %>% pull(outcome)) *
        pilot_cor *
        standard_difference_pilot

      measures = data.frame(
        raw_diff = analysis_diff,
        standard_difference_pilot = standard_difference_pilot,
        control_cor = control_cor,
        label = names(pilot_cor)
      )
    }
  }

  return(measures)
}


#' Create a data frame filled with the difference and denominator
#' of both the analysis sample and pilot sample.
#' It also contains pilot vs outcome correlation.
#' This is done for all the covariates.
#'
#' @param pilot_df dataframe of the pilot data
#' @param analysis_df dataframe of the analysis data
#' @param covariates vector of strings or list denoting column names of interest
#' @param treatment string denoting the name of the treatment variable
#' @param outcome string denoting the name of the outcome variable
#' @param use_abs boolean to denote whether absolute values are used to construct the measures
#' @param user_props user input propensity score for pilot and analysis sample, and model fit; if null calculates logistic regression
#' @param user_progs user input prognostic score for pilot and analysis sample, and model fit; if null calculates logistic/ linear regression
#' @param bias_tol bias tolerance; note always calculated by pilot sample
get_measures = function(pilot_df, analysis_df,
                        covariates, treatment, outcome,
                        use_abs=F,
                        user_props = NULL,
                        user_progs = NULL,
                        bias_tol = 0.005){

  if(is.null(user_props)){
    # propensity score
    props = get_props(pilot_df = pilot_df,
                      analysis_df = analysis_df,
                      covariates = covariates,
                      treatment=treatment)
  } else {
    props = user_props
  }

  if(is.null(user_progs)){
    # prognostic score
    progs = get_progs(pilot_df = pilot_df,
                      analysis_df = analysis_df,
                      covariates = covariates,
                      outcome = outcome)
  } else {
    progs = user_progs
  }


  # set scores
  pilot_df$prognostic_score = progs$progs_pilot
  pilot_df$propensity_score = props$props_pilot
  analysis_df$prognostic_score = progs$progs_analysis
  analysis_df$propensity_score = props$props_analysis

  # standardized differences
  # in the analysis sample
  analysis_diff = apply(analysis_df[, !(names(analysis_df) %in% c(treatment, outcome))],
                        2,
                        get_diff,
                        analysis_df = analysis_df,
                        treatment = treatment)
  analysis_denom = apply(
    analysis_df[, !(names(analysis_df) %in% c(treatment, outcome))],
    2,
    get_analysis_denom,
    analysis_df = analysis_df,
    treatment = treatment
  )
  pilot_denom = apply(pilot_df[, !(names(pilot_df) %in% c(treatment, outcome))], 2, get_pilot_denom)

  # pearson coorrelation
  # in the control sample
  pilot_cor = apply(pilot_df[, !(names(pilot_df) %in% c(treatment, outcome))],
                    2,
                    get_pilot_cor,
                    pilot_df = pilot_df,
                    outcome = outcome)


  if (use_abs) {
    analysis_diff = abs(analysis_diff)
    pilot_cor = abs(pilot_cor)
  }

  standard_difference = analysis_diff / analysis_denom
  standard_difference_pilot = analysis_diff / pilot_denom
  control_cor = pilot_cor
  bias_std_diff_analysis = # stats::sd(pilot_sample %>% pull(outcome)) *
    # removing this means that we are looking at the bias curves
    # where 0.005 "bias" is bias measured divided by standard deviation of the outcome
    # note this is okay because it is the same for every variable
    pilot_cor *
    standard_difference
  bias_std_diff_pilot = # stats::sd(pilot_sample %>% pull(outcome)) *
    pilot_cor *
    standard_difference_pilot
  # pairwise min
  min_denom = pmin(bias_std_diff_analysis, bias_std_diff_pilot)
  standard_difference_max = analysis_diff / min_denom
  bias_max = # stats::sd(pilot_sample %>% pull(outcome)) *
    pilot_cor *
    standard_difference_max
  tol_suggest = abs(bias_tol/ pilot_cor * pilot_denom)
  tol_std_pilot_denom = abs(bias_tol/ pilot_cor / stats::sd(pilot_df %>% pull(outcome)))
  measures = data.frame(
    raw_diff = analysis_diff,
    standard_difference = standard_difference,
    standard_difference_pilot = standard_difference_pilot,
    control_cor = control_cor,
    # pairwise max
    standard_difference_max = pmax(standard_difference, standard_difference_pilot),
    bias_std_diff_analysis = # stats::sd(pilot_sample %>% pull(outcome)) *
      # removing this means that we are looking at the bias curves
      # where 0.005 "bias" is bias measured divided by standard deviation of the outcome
      # note this is okay because it is the same for every variable
      bias_std_diff_analysis,
    bias_std_diff_pilot = # stats::sd(pilot_sample %>% pull(outcome)) *
      bias_std_diff_pilot,
    bias_max = # stats::sd(pilot_sample %>% pull(outcome)) *
      pmax(bias_std_diff_analysis, bias_std_diff_pilot),
    tol_suggest = tol_suggest,
    # tol_std_pilot_denom = tol_std_pilot_denom,
    label = names(pilot_cor)
  )


  progs$progs_plot = ggplot(data.frame(prognostic_score = analysis_df$prognostic_score),
                            aes_q(quote(prognostic_score),
                                  fill = factor(analysis_df %>% pull(treatment)))) +
    geom_histogram(alpha = 0.5,
                   binwidth = 0.05,
                   position = "identity") +
    scale_fill_discrete(name = toTitleCase(treatment),
                        labels = c("control", "treatment")) +
    labs(x = "Prognostic score",
         y = "Count",
         title = "Prognostic score comparison") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14)
    ) +
    theme(panel.background = element_rect(fill = "white"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          panel.border = element_rect(fill = NA, color = "black"),
          plot.background = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank())

  props$props_plot = ggplot(data.frame(propensity_score = analysis_df$propensity_score),
                            aes_q(quote(propensity_score),
                                  fill = factor(analysis_df %>% pull(treatment)))) +
    geom_histogram(alpha = 0.5,
                   binwidth = 0.1,
                   position = "identity") +
    theme_minimal() +
    scale_fill_discrete(name = toTitleCase(treatment),
                        labels = c("control", "treatment")) +
    labs(x = "Propensity score",
         y = "Count",
         title = "Propensity score comparison") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14)
    ) +
    theme(panel.background = element_rect(fill = "white"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          panel.border = element_rect(fill = NA, color = "black"),
          plot.background = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank())

  return(list('measures'=measures, 'props'=props, 'progs'=progs))
}


#' Get standardized difference in analysis sample
#'
#' @param covariate covariate, note it is each column of the sample
#' @param analysis_df dataframe of the analysis data
#' @param treatment string denoting the name of the treatment variable
get_diff <- function(covariate, analysis_df, treatment) {
  difference = mean(covariate[analysis_df %>% pull(treatment) == 1]) - mean(covariate[analysis_df %>% pull(treatment) == 0])
  return(difference)
}

#' Get standardized difference in analysis sample
#' @inheritParams get_diff
#' @param analysis_df dataframe of the analysis data
get_analysis_denom <- function(covariate, analysis_df, treatment) {
  denom = sqrt(stats::var(covariate[analysis_df %>% pull(treatment) == 1])/2 +
                 stats::var(covariate[analysis_df %>% pull(treatment) == 0])/2)
  return(denom)
}

#' Get correlation in pilot dataframe
#'
#' @param covariate covariate, note it is each column of the sample
#' @param pilot_df dataframe of the pilot data
#' @param outcome string denoting the name of the outcome variable
get_pilot_cor <- function(covariate, pilot_df, outcome) {
  pilot_cor = stats::cor(covariate, pilot_df %>% pull(outcome))
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
#' @param pilot_df dataframe of the pilot data
#' @param analysis_df dataframe of the analysis data
#' @param covariates vector of strings or list denoting column names of interest
#' @param treatment string denoting the name of the treatment variable
get_props <- function(pilot_df,
                      analysis_df,
                      covariates,
                      treatment) {

  ## unsure if this should be trained on analysis sample only
  ## or entire sample used analysis sample only for training
  ## implemented entire sample
  fmla = stats::as.formula(paste(paste(treatment, "~ "),
                                 paste(".", collapse = "+")))

  glm_prop_score <- stats::glm(fmla, family = stats::binomial(),
                               data = analysis_df[, c(covariates, treatment)])

  prop_score_analysis = stats::predict(glm_prop_score,
                              analysis_df, type = "response")

  prop_score_pilot = stats::predict(glm_prop_score,
                                    pilot_df, type = "response")


  return(list('props_analysis' = prop_score_analysis,
              'props_pilot' = prop_score_pilot,
              'props_fit' = glm_prop_score))
}

#' Plot prognostic score histogram and get prognostic score.
#'
#' @param pilot_df dataframe of the pilot data
#' @param analysis_df dataframe of the analysis data
#' @param covariates vector of strings or list denoting column names of interest
#' @param outcome string denoting the name of the outcome variable
get_progs <- function(pilot_df, analysis_df, covariates, outcome) {
  fmla = stats::as.formula(paste(paste(outcome, "~ "),
                                 paste(".", collapse = "+")))
  family = if (length(unique(analysis_df %>% pull(outcome))) != 2){stats::gaussian()}
  else{stats::binomial()}

  glm_prog_score <- stats::glm(fmla, family = family,
                               data = pilot_df[, c(covariates, outcome)])

  prog_score_analysis = stats::predict(glm_prog_score,
                                       analysis_df, type = "response")

  prog_score_pilot = stats::predict(glm_prog_score,
                               pilot_df, type = "response")

  return(list('progs_analysis' = prog_score_analysis,
              'progs_pilot' = prog_score_pilot,
              'progs_fit' = glm_prog_score))
}
