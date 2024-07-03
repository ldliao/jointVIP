#' Prepare data frame to plot standardized omitted variable bias
#' Marginal standardized mean differences and outcome correlation
#'
#' @param object jointVIP object
#' @param smd calculate standardized mean difference either using `OVB-based` or `standard`
#' @return measures needed for jointVIP
#' @export
#' @importFrom stats sd var cor complete.cases
get_measures = function(object, smd='OVB-based'){
  treated <- object$analysis_df[, object$treatment]
  covariates <- names(object$analysis_df)[!(names(object$analysis_df)
                                          %in% c(object$treatment,
                                                 object$outcome))]

  md <- apply(object$analysis_df[,covariates], 2,
              function(x){
                mean(x[treated == 1]) - mean(x[treated == 0])
              })

  ovb_denom <- apply(object$pilot_df[,covariates], 2, stats::sd)
  standard_denom <- apply(object$analysis_df[,covariates], 2,
                          function(x){
                            if(stats::var(x[treated == 1]) == 0 |
                               stats::var(x[treated == 1]) == 0){NA}
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

  if(!smd %in% c('OVB-based', 'standard')){
    stop("smd options only include `OVB-based` or `standard`")
  } else {
    smd_calc <- if(smd=='OVB-based'){md/ovb_denom}else{md/standard_denom}
  }

  measures = data.frame(
    outcome_cor = outcome_cor,
    std_md = smd_calc,
    bias = outcome_cor * smd_calc
  )

  if('post_jointVIP' %in% class(object)){
    denom = if(smd=='OVB-based'){ovb_denom}else{standard_denom}
    measures = data.frame(
      outcome_cor = outcome_cor,
      std_md = smd_calc,
      bias = outcome_cor * smd_calc,
      pre_sd = denom
    )
  }
  return(measures)
}



#' Check measures
#' Check to see if there is any missing values or variables
#' without any variation or identical rows (only unique rows will be used)
#'
#' @param measures measures needed for jointVIP
#' @return measures needed for jointVIP
check_measures = function(measures){

  if(nrow(measures[duplicated(measures) |
                   duplicated(measures, fromLast = TRUE),]) > 0){
    warning(paste0(c("Variables",
                     row.names(measures[duplicated(measures) |
                                          duplicated(measures, fromLast = TRUE),]),
                     "measures are duplicated (all multiples are shown).",
                     "\nOnly unique variables will be used."
    )," "))
  }

  clean_measures <- measures[!duplicated(measures),]

  if(any(rowSums(is.na(clean_measures))>0)){
    warning(paste0(c("Variable(s)",
                     rownames(clean_measures)[rowSums(is.na(clean_measures))>0],
                     "contain missing values.",
                     "\nThey are dropped when plotting."
    )," "))
  }

  if(any(clean_measures$std_md == 0)){
    warning(paste0(c("The standardized mean difference for variable(s)",
                     rownames(clean_measures)[sum(clean_measures$std_md == 0)>0],
                     "are 0.",
                     "\nTheir biases cannot be calculated."
    )," "))
  }

  clean_measures <- clean_measures[complete.cases(clean_measures),]
  if (all(dim(clean_measures) == c(0, 0))) {
    stop("measures is empty, please check for errors that may have occurred")
  }
  clean_measures
}

#' Post-measures data frame to plot post-standardized omitted variable bias
#'
#' @param object post_jointVIP object
#' @param smd calculate standardized mean difference either using `OVB-based` or `standard`
#' @return measures needed for jointVIP
#' @export
get_post_measures <- function(object, smd = 'OVB-based'){
  measures <- get_measures(object, smd = smd)

  treated <- object$post_analysis_df[, object$treatment]
  covariates <- names(object$post_analysis_df)[!(names(object$post_analysis_df)
                                                 %in% c(object$treatment,
                                                        object$outcome))]

  post_md <- apply(object$post_analysis_df[,covariates], 2,
              function(x){
                mean(x[treated == 1]) - mean(x[treated == 0])
              })

  post_measures = measures
  post_measures$post_std_md <- post_md/measures$pre_sd
  post_measures$post_bias <- post_measures$post_std_md*post_measures$outcome_cor
  post_measures[, c("outcome_cor",
                    "std_md",
                    "bias",
                    "post_std_md",
                    "post_bias")]
}

#' Calculate bootstrapped variation
#' additional tool to help calculate the uncertainty of each variable's bias
#'
#' @param object jointVIP object
#' @param smd calculate standardized mean difference either using `OVB-based` or `standard`
#' @param use_abs TRUE (default) for absolute measures
#' @param B 100 (default) for the number of times the bootstrap step wished to run
#' @return bootstrapped measures needed for bootstrap-jointVIP
#' @export
#' @importFrom stats sd var cor complete.cases
get_boot_measures = function(object,
                             smd = 'OVB-based',
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
