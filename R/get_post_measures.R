#' Post-measures data frame to plot post-standardized omitted variable bias
#'
#' @param object post_jointVIP object
#' @param smd calculate standardized mean difference either using `cross-sample` or `pooled`
#' @return measures needed for jointVIP
#' @export
get_post_measures <- function(object, smd = "cross-sample"){
  measures <- get_measures(object, smd = smd)

  treated <- object$post_analysis_df[, object$treatment]
  covariates <- names(object$post_analysis_df)[!(names(object$post_analysis_df)
                                                 %in% c(object$treatment,
                                                        object$outcome))]

  w0 = object$wts[treated == 0]
  w1 = object$wts[treated == 1]

  post_md <- colSums(object$post_analysis_df[treated == 1,covariates]*w1)/sum(w1) -
    colSums(object$post_analysis_df[treated == 0,covariates]*w0)/sum(w0)

  post_measures = measures
  post_measures$post_std_md <- post_md/measures$pre_sd
  post_measures$post_bias <- post_measures$post_std_md*post_measures$outcome_cor
  post_measures[, c("outcome_cor",
                    "std_md",
                    "bias",
                    "post_std_md",
                    "post_bias")]
}
