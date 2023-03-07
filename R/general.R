## S3 generics and methods


#' create jointVIP object
#'
#' This is creates the jointVIP object & check inputs
#' @param treatment string denoting the name of the treatment variable
#' @param outcome string denoting the name of the outcome variable
#' @param covariates vector of strings or list denoting column names of interest
#' @param pilot_df data.frame of the pilot data
#' @param analysis_df data.frame of the analysis data
#'
#' @return a jointVIP object
#'
#' @export
#' @examples
#'
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
create_jointVIP <- function(treatment,
                            outcome,
                            covariates,
                            pilot_df,
                            analysis_df) {
  # support function to check inputs
  input.check <- function(treatment,
                          outcome,
                          covariates,
                          pilot_df,
                          analysis_df) {
    if (all(dim(pilot_df) == c(0, 0)) |
        all(dim(analysis_df) == c(0, 0))) {
      stop("both `pilot_df` and `analysis_df` cannot be empty data.frames")
    } else if (!"data.frame" %in% class(pilot_df) |
               !"data.frame" %in% class(analysis_df)) {
      stop("`pilot_df` and `analysis_df` must both have data.frame classes")
    } else if (all(!covariates %in% names(pilot_df)) |
               all(!covariates %in% names(analysis_df))) {
      stop("`covariates` must be in both pilot_df and analysis_df")
    }
    if (!(treatment %in% names(analysis_df))) {
      stop("`treatment` variable must be in analysis_df")
    } else if (!all(sapply(analysis_df[, treatment],
                           function(x) {
                             all(x %in% 0:1)
                           }))) {
      stop("`treatment` must be binary: 0 (control) and 1 (treated)")
    }
    if (var(pilot_df[, outcome]) == 0) {
      stop("`pilot_df` outcome must have some variation")
    }
    if (!all(pilot_df[, treatment] == 0)) {
      stop("`pilot_df` should only be controls only")
    }
  }
  # construction function
  new_jointVIP <- function(treatment,
                           outcome,
                           covariates,
                           pilot_df,
                           analysis_df) {
    input.check(treatment,
                outcome,
                covariates,
                pilot_df,
                analysis_df)
    pilot_df = pilot_df[, c(treatment, outcome, covariates)]
    analysis_df = analysis_df[, c(treatment, outcome, covariates)]
    structure(
      list(
        treatment = treatment,
        outcome = outcome,
        pilot_df = pilot_df,
        analysis_df = analysis_df
      ),
      class = "jointVIP"
    )
  }
  
  invisible(new_jointVIP(treatment,
                         outcome,
                         covariates,
                         pilot_df,
                         analysis_df))
}


#' create jointVIP object
#'
#' This is creates the post_jointVIP object & check inputs
#' @param object a jointVIP object
#' @param post_analysis_df post matched or weighted data.frame
#' @param wts user-supplied weights
#' @return a post_jointVIP object (subclass of jointVIP)
#'
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
#' post_dat_jointVIP =  create_post_jointVIP(new_jointVIP, post_data)
create_post_jointVIP <- function(object,
                                 post_analysis_df, 
                                 wts = NA) {
  if (all(dim(post_analysis_df) == c(0, 0))) {
    stop("`post_analysis_df` cannot be an empty data.frame")
  } else if (!"data.frame" %in% class(post_analysis_df)) {
    stop("`post_analysis_df` must be a data.frame class")
  }
  if (!setequal(names(post_analysis_df), names(object$analysis_df))) {
    stop(
      "`post_analysis_df` must have the same covariates, treatment, and outcome in `analysis_df`"
    )
  }
  if (!all(sapply(post_analysis_df[, object$treatment],
                  function(x) {
                    all(x %in% 0:1)
                  }))) {
    stop("`treatment` must be binary: 0 (control) and 1 (treated)")
  }
  
  if(!all(is.na(wts) & length(wts) == 1)){
    if(length(wts) != dim(post_analysis_df)[[1]]){
      stop("length of `wts` must be the same number of rows as `post_analysis_df`")
    }
    if(!all(is.numeric(wts))){
      stop("`wts` must be numeric")
    }
    if(any(is.na(wts) | all(wts ==  0))){
      stop("`wts` cannot contain NA or all be 0")
    }
  } else {wts = rep(1, dim(post_analysis_df)[[1]])}
  structure(
    list(
      treatment = object$treatment,
      outcome = object$outcome,
      pilot_df = object$pilot_df,
      analysis_df = object$analysis_df,
      post_analysis_df = post_analysis_df,
      wts = wts
    ),
    class = c("post_jointVIP", "jointVIP")
  )
}

#' Obtains a summary jointVIP object
#'
#'
#' @param object a jointVIP object
#' @param ... not used
#' @param smd specify the standardized mean difference is `cross-sample` or `pooled`
#' @param use_abs TRUE (default) for absolute measures
#' @param bias_tol numeric 0.01 (default) any bias above the absolute bias_tol will be summarized
#' @return no return value
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
#' summary(new_jointVIP)
summary.jointVIP <- function(object,
                             ...,
                             smd = 'cross-sample',
                             use_abs = TRUE,
                             bias_tol = 0.01) {
  if (any(is.null(names(list(...)))) & length(list(...)) > 0) {
    warning("anything passed in ... must be named or it'll be ignored")
  }
  
  if (use_abs) {
    measures = abs(get_measures(object, smd = smd))
  } else {
    measures = get_measures(object, smd = smd)
  }
  if (bias_tol < 0) {
    warning("`bias_tol` will be treated as positive")
  }
  measures = measures[order(abs(measures$bias),
                            decreasing = TRUE), ]
  summary_measures = measures[abs(round(measures$bias, 3)) >= abs(bias_tol),
                              "bias", drop = FALSE]
  
  if (use_abs == TRUE) {
    writeLines(sprintf("Max absolute bias is %.3f",
                       abs(max(measures$bias))))
  } else {
    writeLines(sprintf("Max bias is %.3f",
                       (max(measures$bias))))
    writeLines(sprintf("Min bias is %.3f",
                       (min(measures$bias))))
  }
  
  
  writeLines(sprintf(
    "%d variables are above the desired %s absolute bias tolerance",
    length(row.names(summary_measures)),
    abs(bias_tol)
  ))
  
  writeLines(sprintf("%d variables can be plotted",
                     length(row.names(measures))))
  invisible()
}

#' Obtains a summary post_jointVIP object
#'
#' @param object a post_jointVIP object
#' @param ... not used
#' @param smd specify the standardized mean difference is `cross-sample` or `pooled`
#' @param use_abs TRUE (default) for absolute measures
#' @param bias_tol numeric 0.01 (default) any bias above the absolute bias_tol will be summarized
#' @param post_bias_tol numeric 0.005 (default) any bias above the absolute bias_tol will be summarized
#' @return no return value
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
#' summary(post_dat_jointVIP)
summary.post_jointVIP <- function(object,
                                  ...,
                                  smd = 'cross-sample',
                                  use_abs = TRUE,
                                  bias_tol = 0.01,
                                  post_bias_tol = 0.005) {
  if (use_abs) {
    post_measures = abs(get_post_measures(object, smd = smd))
  } else {
    post_measures = get_post_measures(object, smd = smd)
  }
  post_measures = post_measures[order(abs(post_measures$bias),
                                      decreasing = TRUE), ]
  summary_post_measures = post_measures[abs(round(post_measures$bias, 3)) >= bias_tol,
                                        c("bias", "post_bias")]
  NextMethod()
  writeLines(sprintf("\nMax absolute post-bias is %.3f",
                     abs(max(
                       post_measures$post_bias
                     ))))
  writeLines(
    sprintf(
      "Post-measure has %d variable(s) above the desired %s absolute bias tolerance",
      sum(abs(post_measures$post_bias) >= post_bias_tol),
      post_bias_tol
    )
  )
  invisible()
}

#' Obtains a print for jointVIP object
#'
#'
#' @param x a jointVIP object
#' @param ... not used
#' @param smd specify the standardized mean difference is `cross-sample` or `pooled`
#' @param use_abs TRUE (default) for absolute measures
#' @param bias_tol numeric 0.01 (default) any bias above the absolute bias_tol will be printed
#'
#' @return measures used to create the plot of jointVIP
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
#' print(new_jointVIP)
print.jointVIP <- function(x,
                           ...,
                           smd = 'cross-sample',
                           use_abs = TRUE,
                           bias_tol = 0.01) {
  if (any(is.null(names(list(...)))) & length(list(...)) > 0) {
    warning("anything passed in ... must be named or it'll be ignored")
  }
  
  if (use_abs) {
    measures = abs(get_measures(x, smd = smd))
  } else {
    measures = get_measures(x, smd = smd)
  }
  measures = measures[order(abs(measures$bias),
                            decreasing = TRUE), ]
  summary_measures = measures[abs(round(measures$bias, 3)) >= bias_tol,
                              "bias", drop = FALSE]
  print(round(summary_measures, 3))
  invisible()
}

#' Obtains a print for post_jointVIP object
#'
#'
#' @param x a post_jointVIP object
#' @param ... not used
#' @param smd specify the standardized mean difference is `cross-sample` or `pooled`
#' @param use_abs TRUE (default) for absolute measures
#' @param bias_tol numeric 0.01 (default) any bias above the absolute bias_tol will be printed
#'
#' @return measures used to create the plot of jointVIP
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
#' print(post_dat_jointVIP)
print.post_jointVIP <- function(x,
                                ...,
                                smd = 'cross-sample',
                                use_abs = TRUE,
                                bias_tol = 0.01) {
  if (use_abs) {
    post_measures = abs(get_post_measures(x, smd = smd))
  } else {
    post_measures = get_post_measures(x, smd = smd)
  }
  post_measures = post_measures[order(abs(post_measures$bias),
                                      decreasing = TRUE), ]
  summary_post_measures = post_measures[abs(round(post_measures$bias, 3)) >= bias_tol,
                                        c("bias", "post_bias")]
  
  print(round(summary_post_measures, 3))
  invisible()
}


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

#' support function to plot bias curves
#'
#' @param p plot made with jointVIP object
#' @param ... encompasses other variables needed
#' @return a joint variable importance plot of class `ggplot` with curves
#' @import ggplot2
add_bias_curves <- function(p, ...) {
  use_abs = list(...)[['use_abs']]
  measures = list(...)[['measures']]
  bias_curve_cutoffs = list(...)[['bias_curve_cutoffs']]
  expanded_y = list(...)[['expanded_y_curvelab']]
  
  if (is.null(expanded_y)) {
    expanded_y = 0
  }
  if (is.null(bias_curve_cutoffs)) {
    if (use_abs) {
      bias_curve_cutoffs = c(0.005)
      bias_curve_cutoffs = c(bias_curve_cutoffs,
                             floor_dec(seq(0.011,
                                           max(
                                             abs(measures$bias)
                                           ),
                                           length.out = 4), 2))
      bias_curve_cutoffs = bias_curve_cutoffs[abs(bias_curve_cutoffs) >= 0.01 |
                                                abs(bias_curve_cutoffs) == 0.005]
    } else {
      bias_curve_cutoffs = c(0.005)
      
      bias_curve_cutoffs = c(bias_curve_cutoffs,
                             floor_dec(seq(0.011,
                                           max(
                                             abs(measures$bias)
                                           ),
                                           length.out = 4), 2))
      bias_curve_cutoffs = c(bias_curve_cutoffs, -1 * bias_curve_cutoffs)
      
      bias_curve_cutoffs = bias_curve_cutoffs[abs(bias_curve_cutoffs) >= 0.01 |
                                                abs(bias_curve_cutoffs) == 0.005]
    }
  } else if (!is.numeric(bias_curve_cutoffs)) {
    stop("`bias_curve_cutoffs` must be numeric")
  } else  if (0 %in% bias_curve_cutoffs) {
    warning("0 in the `bias_curve_cutoffs` will not be plotted")
  }
  
  bias_curve_cutoffs = bias_curve_cutoffs[!duplicated(bias_curve_cutoffs)]
  bias_curve_cutoffs = bias_curve_cutoffs[bias_curve_cutoffs != 0]
  
  bias_func = function(i) {
    i = force(i)
    f = function(x) {
      i / x
    }
    return(f)
  }
  
  for (b in bias_curve_cutoffs) {
    loop_input = paste(
      "geom_function(fun = bias_func(",
      b,
      "), colour = 'grey5', linetype = 'dotted', alpha = 0.5, na.rm=TRUE)",
      sep = ""
    )
    p <- p + eval(parse(text = loop_input))
  }
  
  if (use_abs) {
    text_bias_lab = data.frame(
      x = c(bias_curve_cutoffs / (expanded_y + ceiling_dec(max(
        abs(measures$outcome_cor)
      ), 2))),
      y = ceiling_dec(max(abs(
        measures$outcome_cor
      )), 2) - 0.002 + expanded_y,
      label = as.character(bias_curve_cutoffs)
    )
    text_bias_lab[text_bias_lab$label == 0.005, 'y'] =
      (ceiling_dec(max(abs(
        measures$outcome_cor
      )), 2) -
        min(c(max(
          bias_curve_cutoffs
        ), 0.02))) + expanded_y
    p <- p + geom_text(
      data = text_bias_lab,
      mapping = aes(
        x = as.numeric(.data$x),
        y = as.numeric(.data$y),
        label = .data$label
      ),
      color = 'grey3',
      alpha = 0.7,
      size = 3.5,
      check_overlap = FALSE
    )
  } else {
    text_bias_lab = data.frame(
      x = c(
        bias_curve_cutoffs /
          (expanded_y + ceiling_dec(max(
            abs(measures$outcome_cor)
          ), 2)),
        bias_curve_cutoffs /
          (expanded_y + ceiling_dec(max(
            abs(measures$outcome_cor)
          ), 2))
      ),
      y = c(
        rep(
          ceiling_dec(max(abs(
            measures$outcome_cor
          )), 2) - 0.002 + expanded_y,
          length(bias_curve_cutoffs)
        ),
        rep(
          -(ceiling_dec(max(
            abs(measures$outcome_cor)
          ), 2) - 0.002) - expanded_y,
          length(bias_curve_cutoffs)
        )
      ),
      label = c(bias_curve_cutoffs, -(bias_curve_cutoffs))
    )
    
    text_bias_lab[abs(as.numeric(text_bias_lab$label)) == 0.005,
                  'y'] =
      (text_bias_lab[abs(as.numeric(text_bias_lab$label)) == 0.005, 'y'] -
         min(c(max(
           bias_curve_cutoffs
         ), 0.1)) *
         sign(text_bias_lab[abs(as.numeric(text_bias_lab$label)) == 0.005, 'y']))
    
    text_bias_lab[abs(as.numeric(text_bias_lab$label)) == 0.005 &
                    text_bias_lab$x < 0,
                  'y'] =
      text_bias_lab[abs(as.numeric(text_bias_lab$label)) == 0.005 &
                      text_bias_lab$x < 0,
                    'y'] + 0.04
    
    p <- p + geom_text(
      data = text_bias_lab,
      mapping = aes(
        x = as.numeric(.data$x),
        y = as.numeric(.data$y),
        label = .data$label
      ),
      color = 'grey3',
      alpha = 0.7,
      size = 3,
      check_overlap = FALSE
    )
  }
  return(p)
}

#' support function to plot variable text labels
#'
#' @param p plot made with jointVIP object
#' @param ... encompasses other variables needed
#' @return a joint variable importance plot of class `ggplot` with curves
#' @importFrom ggrepel geom_text_repel
add_variable_labels <- function(p,
                                ...) {
  measures = list(...)[['measures']]
  arguments <- list(...)
  label_cut_std_md <- arguments$label_cut_std_md
  label_cut_outcome_cor <- arguments$label_cut_outcome_cor
  label_cut_bias <- arguments$label_cut_bias
  text_size <- arguments$text_size
  max.overlaps <- arguments$max.overlaps
  
  if (is.null(label_cut_std_md)) {
    label_cut_std_md = 0
  } else {
    if (!((is.numeric(label_cut_std_md)) &
          (label_cut_std_md > 0))) {
      stop("`label_cut_std_md` must be a positive numeric")
    }
  }
  if (is.null(label_cut_outcome_cor)) {
    label_cut_outcome_cor = 0
  } else {
    if (!(is.numeric(label_cut_outcome_cor) &
          (label_cut_outcome_cor > 0))) {
      stop("`label_cut_outcome_cor` must be a positive numeric")
    }
  }
  if (is.null(label_cut_bias)) {
    label_cut_bias = 0
  } else {
    if (!((is.numeric(label_cut_bias)) &
          (label_cut_bias > 0))) {
      stop("`label_cut_bias` must be a positive numeric")
    }
  }
  if (is.null(text_size)) {
    text_size = 3.5
  } else {
    if (!((is.numeric(text_size)) &
          (text_size > 0))) {
      stop("`text_size` must be a positive numeric")
    }
  }
  if (is.null(max.overlaps)) {
    max.overlaps = 10
  } else {
    if (!((is.numeric(max.overlaps)) &
          (max.overlaps > 0))) {
      stop("`max.overlaps` must be a positive numeric")
    }
  }
  measures$text_label <- row.names(measures)
  if (!(label_cut_std_md == 0 &
        label_cut_outcome_cor == 0 & label_cut_bias == 0)) {
    measures[!(((
      abs(measures$std_md) >= label_cut_std_md
    ) & (
      abs(measures$outcome_cor) >= label_cut_outcome_cor
    )) &
      (abs(measures$bias) >= label_cut_bias)), 'text_label'] = ""
  }
  
  p + geom_text_repel(
    data = measures,
    mapping = aes(label = .data$text_label),
    size = text_size,
    max.overlaps = max.overlaps
  )
}

#' support function for ceiling function with decimals
#'
#' @param num numeric
#' @param dec_place decimal place that is desired ceiling for
#' @return numeric number desired
ceiling_dec <-
  function(num, dec_place = 1) {
    round(num + 5 * 10 ^ (-dec_place - 1), dec_place)
  }


#' support function for floor function with decimals
#'
#' @param num numeric
#' @param dec_place decimal place that is desired floor for
#' @return numeric number desired
floor_dec <- function(num, dec_place = 1) {
  round(num - 5 * 10 ^ (-dec_place - 1), dec_place)
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
