#' Prepare data frame to plot standardized omitted variable bias
#' Marginal standardized mean differences and outcome correlation
#'
#' @param object jointVIP object
#' @param smd calculate standardized mean difference either using `cross-sample` or `pooled`
#' @return measures needed for jointVIP
#' @export
#' @importFrom stats sd var cor complete.cases
get_measures = function(object, smd="cross-sample"){
  treated <- object$analysis_df[, object$treatment]
  covariates <- names(object$analysis_df)[!(names(object$analysis_df)
                                            %in% c(object$treatment,
                                                   object$outcome))]

  md <- apply(object$analysis_df[,covariates], 2,
              function(x){
                mean(x[treated == 1]) - mean(x[treated == 0])
              })

  cs_denom <- apply(object$pilot_df[,covariates], 2, stats::sd)
  pooled_denom <- apply(object$analysis_df[,covariates], 2,
                        function(x){
                          if(stats::var(x[treated == 1]) == 0 &
                             stats::var(x[treated == 0]) == 0){NA}
                          else{
                            sqrt(stats::var(x[treated == 1])/2 +
                                   stats::var(x[treated == 0])/2)
                          }
                        })
  outcome_cor <- apply(object$pilot_df[,covariates],
                       2,
                       function(x){
                         stats::cor(x, object$pilot_df[,object$outcome])
                       })

  if(!smd %in% c("cross-sample", "pooled")){
    stop("smd options only include `cross-sample` or `pooled`")
  } else {
    smd_calc <- if(smd=="cross-sample"){md/cs_denom}else{md/pooled_denom}
  }

  measures = data.frame(
    outcome_cor = outcome_cor,
    std_md = smd_calc,
    bias = outcome_cor * smd_calc
  )

  if('post_jointVIP' %in% class(object)){
    denom = if(smd=="cross-sample"){cs_denom}else{pooled_denom}
    measures = data.frame(
      outcome_cor = outcome_cor,
      std_md = smd_calc,
      bias = outcome_cor * smd_calc,
      pre_sd = denom
    )
  }
  measures = check_measures(measures)
  return(measures)
}

