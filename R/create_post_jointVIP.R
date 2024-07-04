#' create post_jointVIP object
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

  post_analysis_df = one_hot(post_analysis_df)

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
