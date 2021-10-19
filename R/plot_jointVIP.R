#' @import dplyr tools ggplot2 ggrepel
#' @include calc_measure.R
NULL

#' Get the measures and create plot
#'
#' @param measures the measures calculated for jointVIP
#' @param use_pilot_denom if true uses the pilot denominator
#' @param bias_curve_cutoffs cut offs of the bias curve one wishes to plot
#' @param use_abs boolean to denote whether absolute values are used to construct the measures
#' @param plot_title title of the plot to plot
get_jointVIP <-
  function(measures,
           use_pilot_denom = F,
           bias_curve_cutoffs = c(-0.05,-0.03,-0.01,-0.005, 0.005, 0.01, 0.03, 0.05),
           use_abs = F,
           plot_title = "Joint variable importance") {
    max_y = min(round(max(measures$control_cor), 1) + 0.1, 1)
    if (!use_abs) {
      min_y = max(round(min(measures$control_cor), 1) - 0.1,-1)
    } else{
      min_y = 0
    }


    if (use_pilot_denom) {
      measures$std_diff = measures$standard_difference_pilot
      measures$bias = measures$bias_std_diff_pilot_denom
    } else {
      measures$std_diff = measures$standard_difference
      measures$bias = measures$bias_std_diff_analysis_denom
    }

    # plot function
    p = ggplot(measures,
               aes(
                 x = std_diff,
                 y = control_cor,
                 color = bias,
                 label = label
               )) +
      geom_point() +
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
      geom_text_repel(size = 3)

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
            text_bias_lab$y[ind] = max_y - 0.02
          }
          else {
            text_bias_lab$y[ind] = min_y + 0.02
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

      p = p + xlim(c(0,max(measures$std_diff)+0.1))
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

    return(p)
  }


#' Wrapper for the plot
#'
#' @param df data.frame of the original data
#' @param treatment string denoting the name of the treatment variable
#' @param covariates vector of strings or list denoting column names of interest
#' @param outcome string denoting the name of the outcome variable
#' @param pilot_prop proportion denoted as the pilot sample
#' @param seed int random seed
#' @param bias_curve_cutoffs cut offs of the bias curve one wishes to plot
#' @param use_abs boolean to denote whether absolute values are used to construct the measures
#' @param use_pilot_denom if true uses the pilot denominator
#' @param plot_title title of the plot to plot
#' @return a list
#' @export
plot_jointVIP = function(df,
                         treatment,
                         covariates,
                         outcome,
                         pilot_prop = 0.2,
                         seed = 2521323,
                         bias_curve_cutoffs = NULL,
                         use_abs = F,
                         use_pilot_denom = F,
                         plot_title = "Joint variable importance") {
  df = df[, c(covariates, treatment, outcome)]
  # sample splitting
  set.seed(seed = seed)

  # plot(std_diff, control_cor)
  measures = get_measures(df=df, covariates=covariates,
                          treatment=treatment, outcome=outcome,
                          pilot_prop=0.2, seed = seed,
                          use_abs=use_abs)
  progs = measures$progs
  props = measures$props
  if (is.null(bias_curve_cutoffs)) {
    if (use_abs) {
      bias_curve_cutoffs = c(0.005, 0.01, 0.03, 0.05)
    }
    else {
      bias_curve_cutoffs = c(-0.05,-0.03,-0.01,-0.005,
                             0.005, 0.01, 0.03, 0.05)
    }
  }

  joint_vip = get_jointVIP(
    measures$measures,
    use_pilot_denom = use_pilot_denom,
    bias_curve_cutoffs = bias_curve_cutoffs,
    use_abs = use_abs,
    plot_title = plot_title
  )

  return(
    list(
      'VIP' = joint_vip,
      'propensity_comparison' = props$props_plot,
      'propensity_fit' = props$props_fit,
      'prognostic_histogram' = progs$progs_plot,
      'prognostic_fit' = progs$progs_fit,
      'measures' = measures$measures
    )
  )
}
