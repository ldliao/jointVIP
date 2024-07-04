#' plot the jointVIP object
#'
#'
#' @param x a jointVIP object
#' @param ... custom options: `bias_curve_cutoffs`, `text_size`, `max.overlaps`, `label_cut_std_md`, `label_cut_outcome_cor`, `label_cut_bias`, `bias_curves`, `add_var_labs`, `expanded_y_curvelab`
#' @param smd specify the standardized mean difference is `cross-sample` or `pooled`
#' @param use_abs TRUE (default) for absolute measures
#' @param plot_title optional string for plot title
#'
#' @return a joint variable importance plot of class `ggplot`
#' @import ggplot2
#' @export
#' @examples
#' data <- data.frame(year = rnorm(50, 200, 5),
#'                    pop = rnorm(50, 1000, 500),
#'                    gdpPercap = runif(50, 100, 1000),
#'                    trt = rbinom(50, 1, 0.5),
#'                    out = rnorm(50, 1, 0.2))
#' # random 20 percent of control as pilot data
#' pilot_sample_num = sample(which(data$trt == 0),
#'                           length(which(data$trt == 0)) *
#'                           0.2)
#' pilot_df = data[pilot_sample_num, ]
#' analysis_df = data[-pilot_sample_num, ]
#' treatment = "trt"
#' outcome = "out"
#' covariates = names(analysis_df)[!names(analysis_df)
#'                                 %in% c(treatment, outcome)]
#' new_jointVIP = create_jointVIP(treatment = treatment,
#'                                outcome = outcome,
#'                                covariates = covariates,
#'                                pilot_df = pilot_df,
#'                                analysis_df = analysis_df)
#' plot(new_jointVIP)
plot.jointVIP <- function(x,
                          ...,
                          smd = 'cross-sample',
                          use_abs = TRUE,
                          plot_title = "Joint Variable Importance Plot") {
  if (any(is.null(names(list(...)))) & length(list(...)) > 0) {
    warning("anything passed in ... must be named or it'll be ignored")
  } else {
    params = list(...)
    if ("add_post_labs" %in% names(params) &
        'post_jointVIP' %in% class(x)) {
      params = within(params,
                      rm("add_post_labs"))
    }
    if ("post_label_cut_bias" %in% names(params) &
        'post_jointVIP' %in% class(x)) {
      params = within(params,
                      rm("post_label_cut_bias"))
    }
    if (length(params) > 0) {
      if (!all(
        names(params) %in% c(
          'bias_curve_cutoffs',
          'text_size',
          'max.overlaps',
          'label_cut_std_md',
          'label_cut_outcome_cor',
          'label_cut_bias',
          'bias_curves',
          'add_var_labs',
          'expanded_y_curvelab'
        )
      )) {
        stop(
          paste0(
            "custom plot options passed into ... must be one of the following:",
            'bias_curve_cutoffs',
            ' text_size',
            ' max.overlaps',
            ' label_cut_std_md',
            ' label_cut_outcome_cor',
            ' label_cut_bias',
            ' bias_curves',
            ' add_var_labs',
            ' expanded_y_curvelab'
          )
        )
      }
    }
  }

  if (use_abs) {
    measures = abs(get_measures(x, smd = smd))
  } else {
    measures = get_measures(x, smd = smd)
  }

  if (smd == "pooled") {
    p <- ggplot(measures,
                aes(x = .data$std_md,
                    y = .data$outcome_cor))  +
      geom_point()
  } else {
    p <- ggplot(measures,
                aes(
                  x = .data$std_md,
                  y = .data$outcome_cor,
                  color = abs(.data$bias),
                ))  +
      geom_point() +
      scale_color_gradient(low = "blue", high = "red")
  }
  # minimal plot
  p <- p +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14)
    ) +
    theme(
      panel.background = element_rect(fill = "white"),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      panel.border = element_rect(fill = NA, color = "black"),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank()
    )

  if (use_abs) {
    p <- p + labs(
      x = "Absolute Standardized Mean Difference",
      y = "Absolute Outcome Correlation",
      title = plot_title,
      subtitle = paste(smd, "SMD"),
      color = "Bias"
    ) + ylim(c(0, ceiling_dec(max(
      abs(measures$outcome_cor)
    ), 2)))
  } else {
    p <- p + labs(
      x = "Standardized Mean Difference",
      y = "Outcome Correlation",
      title = plot_title,
      subtitle = paste(smd, "SMD"),
      color = "Bias"
    ) + geom_function(
      fun = function(x) {
        0
      },
      linetype = 'dashed',
      color = 'grey7',
      alpha = 0.4
    ) +
      geom_vline(
        xintercept = 0,
        linetype = 'dashed',
        alpha = 0.4,
        color = 'grey7'
      ) +
      ylim(c(-ceiling_dec(max(
        abs(measures$outcome_cor)
      ), 2),
      ceiling_dec(max(
        abs(measures$outcome_cor)
      ), 2)))
  }

  bias_curves = list(...)[['bias_curves']]
  if (is.null(bias_curves)) {
    bias_curves = TRUE
  } else if (!is.logical(bias_curves)) {
    stop("`bias_curves` can only be set as TRUE or FALSE")
  }
  if (bias_curves) {
    if (smd == "cross-sample") {
      p <- add_bias_curves(p,
                           use_abs = use_abs,
                           measures = measures, ...)
    }
  }

  add_var_labs = list(...)[['add_var_labs']]
  if (is.null(add_var_labs)) {
    add_var_labs = TRUE
  } else if (!is.logical(add_var_labs)) {
    stop("`add_var_labs` can only be set as TRUE or FALSE")
  }
  if (add_var_labs) {
    p <- add_variable_labels(p,
                             measures = measures, ...)
  }
  p
}

#' plot the post_jointVIP object
#' this plot uses the same custom options as the jointVIP object
#'
#'
#' @param x a post_jointVIP object
#' @param ... custom options: `bias_curve_cutoffs`, `text_size`, `max.overlaps`, `label_cut_std_md`, `label_cut_outcome_cor`, `label_cut_bias`, `bias_curves`, `add_var_labs`, `expanded_y_curvelab`
#' @param smd specify the standardized mean difference is `cross-sample` or `pooled`
#' @param use_abs TRUE (default) for absolute measures
#' @param plot_title optional string for plot title
#' @param add_post_labs TRUE (default) show post-measure labels
#' @param post_label_cut_bias 0.005 (default) show cutoff above this number; suppressed if show_post_labs is FALSE
#'
#' @return a post-analysis joint variable importance plot of class `ggplot`
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @export
#' @examples
#' data <- data.frame(year = rnorm(50, 200, 5),
#'                    pop = rnorm(50, 1000, 500),
#'                    gdpPercap = runif(50, 100, 1000),
#'                    trt = rbinom(50, 1, 0.5),
#'                    out = rnorm(50, 1, 0.2))
#' # random 20 percent of control as pilot data
#' pilot_sample_num = sample(which(data$trt == 0),
#'                           length(which(data$trt == 0)) *
#'                           0.2)
#' pilot_df = data[pilot_sample_num, ]
#' analysis_df = data[-pilot_sample_num, ]
#' treatment = "trt"
#' outcome = "out"
#' covariates = names(analysis_df)[!names(analysis_df)
#'                                 %in% c(treatment, outcome)]
#' new_jointVIP = create_jointVIP(treatment = treatment,
#'                                outcome = outcome,
#'                                covariates = covariates,
#'                                pilot_df = pilot_df,
#'                                analysis_df = analysis_df)
#'
#' ## at this step typically you may wish to do matching or weighting
#' ## the results after can be stored as a post_data
#' ## the post_data here is not matched or weighted, only for illustrative purposes
#' post_data <- data.frame(year = rnorm(50, 200, 5),
#'                         pop = rnorm(50, 1000, 500),
#'                         gdpPercap = runif(50, 100, 1000),
#'                         trt = rbinom(50, 1, 0.5),
#'                         out = rnorm(50, 1, 0.2))
#' post_dat_jointVIP = create_post_jointVIP(new_jointVIP, post_data)
#' plot(post_dat_jointVIP)
plot.post_jointVIP <- function(x,
                               ...,
                               smd = 'cross-sample',
                               use_abs = TRUE,
                               plot_title = "Joint Variable Importance Plot",
                               add_post_labs = TRUE,
                               post_label_cut_bias = 0.005) {
  p <- NextMethod()
  if (use_abs) {
    post_measures = abs(get_post_measures(x, smd = smd))
  } else {
    post_measures = get_post_measures(x, smd = smd)
  }

  if (!is.logical(add_post_labs)) {
    stop("`add_post_labs` can only be set as TRUE or FALSE")
  }

  if (!(is.numeric(post_label_cut_bias) &
        (post_label_cut_bias > 0))) {
    stop("`post_label_cut_bias` must be a positive numeric")
  }


  # first turn off the original points
  p$layers[[1]] <- NULL

  add_var_labs = list(...)[['add_var_labs']]
  if (is.null(add_var_labs)) {
    add_var_labs = TRUE
  } else if (!is.logical(add_var_labs)) {
    stop("`add_var_labs` can only be set as TRUE or FALSE")
  }

  if (add_var_labs == TRUE) {
    # turn off the variable labels
    # which is the last layer
    p$layers[[length(p$layers)]] <- NULL
  }

  if (smd == "pooled") {
    p <- p + geom_point(data = post_measures,
                        aes(x = .data$post_std_md,
                            y = .data$outcome_cor))
  } else {
    p <- p + geom_point(data = post_measures,
                        aes(
                          x = .data$post_std_md,
                          y = .data$outcome_cor,
                          color = abs(.data$post_bias),
                        ))
  }
  arguments <- list(...)
  text_size <- arguments$text_size
  max.overlaps <- arguments$max.overlaps

  if (add_post_labs) {
    if (is.null(text_size)) {
      text_size = 3.5
    } else {
      if (!(is.numeric(text_size) &
            (text_size > 0))) {
        stop("`text_size` must be a positive numeric")
      }
    }
    if (is.null(max.overlaps)) {
      max.overlaps = 10
    } else {
      if (!(is.numeric(max.overlaps) &
            (max.overlaps > 0))) {
        stop("`max.overlaps` must be a positive numeric")
      }
    }
    post_measures$text_label <- row.names(post_measures)
    if (!(post_label_cut_bias == 0)) {
      post_measures[!(abs(round(post_measures$post_bias, 4)) >= post_label_cut_bias), 'text_label'] = ""
    }

    if (smd == "pooled") {
      p <- p + geom_text_repel(
        data = post_measures,
        aes(
          x = .data$post_std_md,
          y = .data$outcome_cor,
          label = .data$text_label,
        ),
        size = text_size,
        max.overlaps = max.overlaps
      )

    } else {
      p <- p + geom_text_repel(
        data = post_measures,
        aes(
          x = .data$post_std_md,
          y = .data$outcome_cor,
          color = abs(.data$post_bias),
          label = .data$text_label,
        ),
        size = text_size,
        max.overlaps = max.overlaps
      )
    }
  }
  if (ceiling_dec(max(abs(post_measures$post_bias)), 2) > ceiling_dec(max(abs(post_measures$bias)), 2)) {
    warning(
      "Color not scaled to previous pre-bias plot since the post-bias is greater than pre-bias"
    )

  } else {
    sc <- scale_color_gradient(low = 'blue',
                               high = 'red',
                               limits = c(0, ceiling_dec(max(
                                 abs(post_measures$bias)
                               ), 2)))
    p <- p + sc
  }

  return(p)
}


#' plot the bootstrap version of the jointVIP object
#'
#'
#' @param x a jointVIP object
#' @param ... custom options: `bias_curve_cutoffs`, `text_size`, `max.overlaps`, `label_cut_std_md`, `label_cut_outcome_cor`, `label_cut_bias`, `bias_curves`, `add_var_labs`
#' @param smd specify the standardized mean difference is `cross-sample` or `pooled`
#' @param use_abs TRUE (default) for absolute measures
#' @param plot_title optional string for plot title
#' @param B 100 (default) for the number of times the bootstrap step wished to run
#'
#' @return a joint variable importance plot of class `ggplot`
#' @import ggplot2
#' @export
#' @examples
#' data <- data.frame(year = rnorm(50, 200, 5),
#'                    pop = rnorm(50, 1000, 500),
#'                    gdpPercap = runif(50, 100, 1000),
#'                    trt = rbinom(50, 1, 0.5),
#'                    out = rnorm(50, 1, 0.2))
#' # random 20 percent of control as pilot data
#' pilot_sample_num = sample(which(data$trt == 0),
#'                           length(which(data$trt == 0)) *
#'                           0.2)
#' pilot_df = data[pilot_sample_num, ]
#' analysis_df = data[-pilot_sample_num, ]
#' treatment = "trt"
#' outcome = "out"
#' covariates = names(analysis_df)[!names(analysis_df)
#'                                 %in% c(treatment, outcome)]
#' new_jointVIP = create_jointVIP(treatment = treatment,
#'                                outcome = outcome,
#'                                covariates = covariates,
#'                                pilot_df = pilot_df,
#'                                analysis_df = analysis_df)
#' # more bootstrap number B would be typically used in real settings
#' # this is just a small example
#' set.seed(1234567891)
#' bootstrap.plot(new_jointVIP, B = 15)
bootstrap.plot <- function(x,
                           ...,
                           smd = 'cross-sample',
                           use_abs = TRUE,
                           plot_title = "Joint Variable Importance Plot",
                           B = 100) {
  if (!all(class(x) == 'jointVIP')) {
    stop("bootstrap_plot function only applicable to class jointVIP only!")
  }

  bias_curves = list(...)[['bias_curves']]
  if (is.null(bias_curves)) {
    if (smd == "cross-sample") {
      specified_bias_curves = TRUE
    } else {
      specified_bias_curves = FALSE
    }
  } else if (!is.logical(bias_curves)) {
    stop("`bias_curves` can only be set as TRUE or FALSE")
  } else {
    specified_bias_curves = bias_curves
  }

  p <-
    plot(
      x,
      ...,
      smd = smd,
      bias_curves = FALSE,
      use_abs = use_abs,
      plot_title = plot_title
    )

  boot_measures <- get_boot_measures(
    object = x,
    smd = smd,
    use_abs = use_abs,
    B = B
  )
  if (use_abs) {
    og <- abs(get_measures(x, smd = smd))
  } else {
    og <- get_measures(x, smd = smd)
  }

  p <-
    p + geom_segment(
      data = data.frame(t(boot_measures[, , 'outcome_cor'])),
      aes(
        x = og$std_md,
        xend = og$std_md,
        y = .data$X2.5.,
        yend = .data$X97.5.
      ),
      color = "cornsilk4",
      size = 1.5,
      alpha = 0.4
    ) +
    ylim(c(min(0,
               ifelse(
                 use_abs, 0, -ceiling_dec(max(abs(data.frame(
                   t(boot_measures[, , 'outcome_cor'])
                 ))), 2)
               )),
           ceiling_dec(max(data.frame(
             t(boot_measures[, , 'outcome_cor'])
           )), 2))) +
    geom_segment(
      data = data.frame(t(boot_measures[, , 'std_md'])),
      aes(
        x = .data$X2.5.,
        xend = .data$X97.5.,
        y = og$outcome_cor,
        yend = og$outcome_cor
      ),
      color = "cornsilk4",
      size = 1.5,
      alpha = 0.4
    )

  if (smd == "cross-sample" & specified_bias_curves) {
    p <- add_bias_curves(
      p,
      use_abs = use_abs,
      measures = og,
      expanded_y_curvelab =
        ceiling_dec(max(abs(max(
          abs(t(boot_measures[, , 'outcome_cor']))
        ))), 2) -
        ceiling_dec(max(abs(og$outcome_cor)), 2),
      ...
    )
  }
  return(p)
}
