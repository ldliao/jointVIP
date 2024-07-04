#' Calculate bootstrapped variation
#' additional tool to help calculate the uncertainty of each variable's bias
#'
#' @param object jointVIP object
#' @param smd calculate standardized mean difference either using `cross-sample` or `pooled`
#' @param use_abs TRUE (default) for absolute measures
#' @param B 100 (default) for the number of times the bootstrap step wished to run
#' @return bootstrapped measures needed for bootstrap-jointVIP
#' @export
#' @importFrom stats sd var cor complete.cases
get_boot_measures = function(object,
                             smd = "cross-sample",
                             use_abs = TRUE,
                             B = 100) {
  if(!is.numeric(B)){
    stop("B is the number of bootstrap step should run; please input a numeric\nThe ceiling of such number will be used.")
  } else {
    if(B <= 10){
      stop("B is too small please make it a larger number")
    } else if (B >= 990000) {
      stop("B too large, please specify this number to be under 990000")
    }
  }

  B = ceiling(B)

  # original measures
  og = get_measures(object = object, smd = smd)

  # 3d bootstrap array result
  result <- array(0, dim = c(nrow(og),
                             2,
                             B))

  # select from random list of large numbers
  seeds <- paste(sample(1e4:(1e6 - 1), B, replace = F))

  pilot_df = object$pilot_df
  analysis_df = object$analysis_df

  for (b in (1:B)) {
    set.seed(as.numeric(seeds[b]))
    boot_pilot_df = pilot_df[sample(1:nrow(pilot_df),
                                    size = nrow(pilot_df),
                                    replace = T), ]
    boot_analysis_df = analysis_df[sample(1:nrow(analysis_df),
                                          size = nrow(analysis_df),
                                          replace = T), ]
    temp_measure = get_measures(
      object =
        create_jointVIP(
          object$treatment,
          object$outcome,
          names(analysis_df)[!names(analysis_df) %in% c(object$treatment,
                                                        object$outcome)],
          boot_pilot_df,
          boot_analysis_df
        ),
      smd = smd
    )[,c('outcome_cor', 'std_md')]
    result[, , b] = as.matrix(temp_measure)
  }
  dimnames(result) = list(row.names(og),
                          c('outcome_cor',
                            'std_md'),
                          seeds)

  if(use_abs){
    result = abs(result)
  }

  boot_sd = apply(
    result * is.finite(result),
    c(1, 2),
    stats::quantile,
    probs = c(0.025, 0.975),
    na.rm = TRUE
  )

  return(boot_sd)
}
