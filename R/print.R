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

  if (any(is.null(names(list(...)))) & length(list(...)) > 0) {
    warning("anything passed in ... must be named or it'll be ignored")
  }

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
