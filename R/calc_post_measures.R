#' @import dplyr tools ggplot2 ggrepel tidyr
#' @include calc_measures.R
NULL

#' Get standardized difference in analysis sample
#' @param post_analysis_df after matching data frame
#' @param measures measures from calculate measures
#' @param props properpensity score list containing the model
#' @param progs prognostic score list containing the model
#' @param treatment specify treatment
#' @param use_denom which denominator to use
#' @param joint_vip plot to add to
#' @param use_abs whether using absolute value or not
get_post_analysis_vip <- function(post_analysis_df, measures, props, progs,
                                  treatment, use_denom, joint_vip, use_abs
                                  ){
  post_analysis_diff = apply(post_analysis_df[, !(names(post_analysis_df) %in% c(treatment, outcome))],
                             2,
                             get_diff,
                             analysis_df = post_analysis_df,
                             treatment = treatment)

  # propensity score
  post_prop = stats::predict(props$props_fit,
                             post_analysis_df,
                             type = "response")
  # prognostic score
  post_prog = stats::predict(progs$progs_fit,
                             post_analysis_df,
                             type = "response")

  post_analysis_diff['propensity_score'] = mean(post_prop[post_analysis_df%>% pull(treatment) == 1]) -
    mean(post_prop[post_analysis_df%>% pull(treatment) == 0])
  post_analysis_diff['prognostic_score'] = mean(post_prog[post_analysis_df%>% pull(treatment) == 1]) -
    mean(post_prog[post_analysis_df%>% pull(treatment) == 0])

  post_analysis_standard_difference =
    post_analysis_diff * measures$measures$standard_difference / measures$measures$raw_diff
  post_analysis_standard_difference_pilot =
    post_analysis_diff * measures$measures$standard_difference_pilot / measures$measures$raw_diff
  bias_std_diff_post_analysis =
    measures$measures$control_cor *
    post_analysis_standard_difference
  bias_std_diff_post_pilot = # stats::sd(pilot_sample %>% pull(outcome)) *
    measures$measures$control_cor *
    post_analysis_standard_difference_pilot

  post_df = data.frame(post_analysis_raw_diff=post_analysis_diff,
                       post_analysis_standard_difference=post_analysis_standard_difference,
                       post_analysis_standard_difference_pilot=post_analysis_standard_difference_pilot,
                       bias_std_diff_post_analysis=bias_std_diff_post_analysis,
                       bias_std_diff_post_pilot=bias_std_diff_post_pilot)
  if(use_denom == 'standard'){
    post_df$bias = post_df$bias_std_diff_post_analysis
    post_df$std_diff = post_df$post_analysis_standard_difference
    measures$measures$std_diff = measures$measures$standard_difference
  } else {
    if(use_denom == "pilot"){
      post_df$bias = post_df$bias_std_diff_post_pilot
      post_df$std_diff = post_df$post_analysis_standard_difference_pilot
      measures$measures$std_diff = measures$measures$standard_difference_pilot
    } else {
      stop("both option hasn't been implemented yet for post-match")
    }
  }

  max_x = max(max(post_df$std_diff), max(measures$measures$std_diff), 0) + 0.1

  if (use_abs) {
    post_df$std_diff = abs(post_df$std_diff)
    min_x = 0
  } else {
    min_x = min(min(post_df$std_diff), min(measures$measures$std_diff), 0) - 0.1
  }

  post_df$control_cor = measures$measures$control_cor
  post_df$label = measures$measures$label
  # post_df[!((measures$measures$std_diff >= label_cutoff_std_diff) |
  #             (measures$measures$control_cor >= label_cutoff_control_cor)) &
  #           (measures$measures$bias >= label_cutoff_bias),'text_labels'] = ""

  ggplot(post_df, aes(x = std_diff,
                      y = control_cor)) + geom_point()

  joint_vip = joint_vip + geom_point(data=post_df,
                                     aes(x = std_diff,
                                         y = control_cor)) +
    xlim(c(min_x,max_x))
  return(joint_vip)
}
