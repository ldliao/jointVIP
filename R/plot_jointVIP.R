#' @import dplyr tools ggplot2 ggrepel tidyr
#' @include calc_measures.R calc_boot_variation.R
NULL

#' Get the measures and create plot
#'
#' @param measures the measures calculated for jointVIP
#' @param use_denom must be "standard", "pilot", or "min"
#' @param bias_curve_cutoffs cut offs of the bias curve one wishes to plot
#' @param use_abs boolean to denote whether absolute values are used to construct the measures
#' @param plot_title title of the plot to plot
#' @param pre_matched_alpha set to 1; changed if post-match exists
#' @param label_cutoff_std_diff text label cut off for standardized difference
#' @param label_cutoff_control_cor text label cut off for correlation
#' @param label_cutoff_bias text label cut off for bias
get_jointVIP <-
  function(measures,
           bias_curve_cutoffs = c(-0.05,-0.03,-0.01,-0.005, 0.005, 0.01, 0.03, 0.05),
           use_abs = F, use_denom = "standard",
           plot_title = "Joint variable importance",
           pre_matched_alpha = 1,
           label_cutoff_std_diff=NULL,
           label_cutoff_control_cor=NULL,
           label_cutoff_bias=NULL) {

    max_y = min(round(max(measures$control_cor), 1) + 0.1, 1)
    if (!use_abs) {
      min_y = max(round(min(measures$control_cor), 1) - 0.1,-1)
    } else{
      min_y = 0
    }

    if (!(use_denom %in% c("standard", "pilot", "min", "both"))){
      stop("The use_denom must be 'standard', 'pilot', 'min', or 'both' ; please check input!")
    }

    if (use_denom == 'pilot') {
      measures$std_diff = measures$standard_difference_pilot
      measures$bias = measures$bias_std_diff_pilot
    } else {
      if (use_denom == 'min') {
        measures$std_diff = measures$standard_difference_max
        measures$bias = measures$bias_max
      } else {
        measures$std_diff = measures$standard_difference
        measures$bias = measures$bias_std_diff_analysis}
    }

    if(is.null(label_cutoff_std_diff)){
      label_cutoff_std_diff = 0
    }
    if(is.null(label_cutoff_control_cor)){
      label_cutoff_control_cor = 0
    }
    if(is.null(label_cutoff_bias)){
      label_cutoff_bias = 0
    }

    measures$text_labels = measures$label
    measures[!((abs(measures$std_diff) >= label_cutoff_std_diff) |
                 (abs(measures$control_cor) >= label_cutoff_control_cor)) &
               (abs(measures$bias) >= label_cutoff_bias),'text_labels'] = ""
    # plot function
    p = ggplot(measures, aes(
      x = std_diff,
      y = control_cor,
      color = abs(bias),
      label = label
    )) +
      geom_point(alpha=pre_matched_alpha) +
      scale_color_gradient(low = "blue", high = "red") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        plot.title = element_text(size = 15)
      ) +
      labs(
        x = "Standardized difference",
        y = "Outcome correlation",
        title = plot_title,
        color = "Bias"
      ) +
      ylim(c(min_y, max_y)) +
      geom_text_repel(mapping = aes(label = text_labels), size = 3)

    bias_func = function(i) {
      i = force(i)
      f = function(x) {
        i / x
      }
      return(f)
    }
    for (b in bias_curve_cutoffs) {
      loop_input = paste("geom_function(fun = bias_func(",
                         b,
                         "), linetype = 'dotted',alpha = 0.4)",
                         sep = "")
      p <- p + eval(parse(text = loop_input))
    }

    if (!use_abs) {
      text_bias_lab = data.frame(
        x = c(bias_curve_cutoffs / max_y, bias_curve_cutoffs / min_y),
        y = c(rep(max_y, length(
          bias_curve_cutoffs
        )),
        rep(min_y, length(
          bias_curve_cutoffs
        ))),
        label = rep(bias_curve_cutoffs, 2)
      )
      if (0.005 %in% abs(bias_curve_cutoffs)) {
        for (ind in which(abs(rep(bias_curve_cutoffs, 2)) == 0.005)) {
          if (text_bias_lab[ind, 'y'] > 0) {
            text_bias_lab$y[ind] = max_y - 0.03
          }
          else {
            text_bias_lab$y[ind] = min_y + 0.03
          }
        }
        p = p + geom_function(
          fun = function(x) {
            0
          },
          linetype = 'dashed',
          alpha = 0.4
        ) +
          geom_vline(xintercept = 0,
                     linetype = 'dashed',
                     alpha = 0.4)
      }
    } else {
      text_bias_lab = data.frame(
        x = c(bias_curve_cutoffs / max_y),
        y = max_y,
        label = bias_curve_cutoffs
      )
      if (0.005 %in% bias_curve_cutoffs) {
        text_bias_lab$y[which(bias_curve_cutoffs == 0.005)] = max_y - 0.02
      }
      #min(round(max(measures$std_diff), 1) + 0.1, 1)))
    }

    p = p +
      annotate(
        "text",
        x = text_bias_lab$x,
        y = text_bias_lab$y,
        label = text_bias_lab$label,
        size = 2.5
      )

    if (use_denom == 'both'){
      if (use_abs==T){
        min_x = 0
      } else { min_x = min(pmin(measures$standard_difference,
                                measures$standard_difference_pilot))-0.1}
      m = measures[, c('standard_difference',
                       'standard_difference_pilot','label')]
      mm = tidyr::gather(m, 'std_diffs', 'values', -label)
      mm$cors = measures$control_cor
      mm$bias = measures$bias_std_diff_pilot
      p = p +
        geom_point(data = mm[mm$std_diffs == "standard_difference_pilot",],
                   aes(values, cors), alpha=0.4) +
        geom_line(data = mm, aes(values,cors, group = label), alpha=.4) +
        labs(subtitle = "Pilot standardized differences plotted with transparency") +
        xlim(c(min_x,max(measures$standard_difference_max)+0.1))
    } else {
      if (use_abs == T){
        p = p + xlim(c(0,max(measures$std_diff)+0.1))
      }
    }

    if (use_denom == "pilot"){
      p = p + labs(subtitle = "Standardized differences uses pilot sample variance in denominator.")
    }
    if (use_denom == "min"){
      p = p + labs(subtitle = "Standardized differences uses max difference.")
    }

    return(p)
  }


#' Wrapper for the plot
#'
#' @param pilot_df dataframe of the pilot data
#' @param analysis_df dataframe of the analysis data
#' @param treatment string denoting the name of the treatment variable
#' @param covariates vector of strings or list denoting column names of interest
#' @param outcome string denoting the name of the outcome variable
#' @param props user input propensity score list
#' @param progs user input prognostic score list
#' @param post_analysis_df post match analysis data frame
#' @param post_prop user input propensity score only used when post_analysis_df is used
#' @param post_prog user input prognostic score only used when post_analysis_df is used
#' @param bias_curve_cutoffs cut offs of the bias curve one wishes to plot
#' @param use_abs boolean to denote whether absolute values are used to construct the measures
#' @param use_denom must be "standard", "pilot", or "min"
#' @param plot_title title of the plot to plot
#' @param label_cutoff_std_diff text label cut off for standardized difference
#' @param label_cutoff_control_cor text label cut off for correlation
#' @param label_cutoff_bias text label cut off for bias
#' @return a list
#' @export
plot_jointVIP = function(pilot_df,
                         analysis_df,
                         treatment,
                         covariates,
                         outcome,
                         props = NULL,
                         progs = NULL,
                         post_analysis_df = NULL,
                         post_prop = NULL,
                         post_prog = NULL,
                         bias_curve_cutoffs = NULL,
                         use_abs = F,
                         use_denom = 'standard',
                         plot_title = "Joint variable importance",
                         label_cutoff_std_diff = NULL,
                         label_cutoff_control_cor = NULL,
                         label_cutoff_bias = NULL
                         ) {

  pilot_df = pilot_df[,c(covariates, treatment, outcome)]
  analysis_df = analysis_df[,c(covariates, treatment, outcome)]
  measures = get_measures(pilot_df=pilot_df, analysis_df=analysis_df,
                          covariates=covariates, treatment=treatment,
                          outcome=outcome,
                          use_abs=use_abs,
                          user_props = props,
                          user_progs = progs)
  progs = measures$progs
  props = measures$props
  if (is.null(bias_curve_cutoffs)) {
    if (use_abs) {
      bias_curve_cutoffs = c(0.005)
      bias_curve_cutoffs = c(bias_curve_cutoffs, seq(0.01, ceiling(max(max(abs(measures$measures$bias_max)), 0.03)/0.02)*0.02, 0.02))
    }
    else {
      bias_curve_cutoffs = c(0.005)
      bias_curve_cutoffs = c(bias_curve_cutoffs, seq(0.01, ceiling(max(max(abs(measures$measures$bias_max)), 0.03)/0.02)*0.02, 0.02))
      bias_curve_cutoffs = c(bias_curve_cutoffs, -1*bias_curve_cutoffs)
    }
  }

  if (! is.null(post_analysis_df)){
    pre_matched_alpha = 0.4
  } else {pre_matched_alpha = 1}

  joint_vip = get_jointVIP(
    measures$measures,
    use_denom = use_denom,
    bias_curve_cutoffs = bias_curve_cutoffs,
    use_abs = use_abs,
    plot_title = plot_title,
    pre_matched_alpha = pre_matched_alpha,
    label_cutoff_std_diff = label_cutoff_std_diff,
    label_cutoff_control_cor = label_cutoff_control_cor,
    label_cutoff_bias = label_cutoff_bias
  )


  if (! is.null(post_analysis_df)){
    post_analysis_vip = get_post_analysis_vip(post_analysis_df=post_analysis_df,
                                      measures=measures, props=props, progs=progs,
                                      treatment=treatment, use_denom=use_denom,
                                      joint_vip=joint_vip, use_abs=use_abs,
                                      post_prop = post_prop, post_prog = post_prog)

    post_analysis_vip$joint_vip = post_analysis_vip$joint_vip +
      labs(subtitle = "Pre-matched variables plotted with transparency")
    return(
      list(
        'VIP' = post_analysis_vip$joint_vip,
        'propensity_comparison' = post_analysis_vip$propensity_comparison,
        'prognostic_comparison' = post_analysis_vip$prognostic_comparison,
        'measures' = post_analysis_vip$measures
      )
    )
  }

  return(
    list(
      'VIP' = joint_vip,
      'propensity_comparison' = props$props_plot,
      'propensity_fit' = props$props_fit,
      'prognostic_comparison' = progs$progs_plot,
      'prognostic_fit' = progs$progs_fit,
      'measures' = measures$measures
    )
  )
}
