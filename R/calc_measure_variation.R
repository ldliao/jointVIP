#' @import dplyr tools
#' @include calc_measure.R
NULL

get_measures_variation <- function(seed, k=20, ...){
  og_measures = get_measures(df=df, covariates=covariates,
                          treatment=treatment, outcome=outcome,
                          pilot_prop=pilot_prop, seed = seed,
                          use_abs=use_abs)

  # repeat k random times with seeds
  set.seed(seed)
  seeds <- paste(sample(1e5:1e6, k, replace=F))

  # assigning row names background variables
  bg_vars=og_measures$measures$label

  # assigning column names measure
  msr=names(og_measures$measures[,names(og_measures$measures)!="label"])

  # assigning array names which seed was used # just seeds
  # proprotion used
  pilot_props = seq(0.1,0.5,0.05)

  # 4d array
  result <- array(0, dim = c(length(bg_vars),
                             length(msr),
                             length(seeds),
                             length(pilot_props))
                  )

  for(pp in 1:length(pilot_props)){
    for(i in 1:k){
      temp_measure = get_measures(df=df, covariates=covariates,
                                  treatment=treatment, outcome=outcome,
                                  pilot_prop=pilot_props[pp], seed = as.numeric(seeds[i]),
                                  use_abs=use_abs)
      result[,,i,pp] = as.matrix(temp_measure$measures[,names(temp_measure$measures)!="label"])
    }
  }
  dimnames(result) = list(bg_vars, msr,seeds, pilot_props)

  measure_variation = list(measure_mean = apply(result*is.finite(result), c(1,2,4), mean, na.rm = TRUE),
                           measure_variance = apply(result*is.finite(result), c(1,2,4), stats::var, na.rm = TRUE)
                           )
  return(measure_variation)
}


# msr_interest = 'standard_difference'
# # # measure_mean = apply(result*is.finite(result), c(1,2,4), mean, na.rm = TRUE)
# var_interest = "AVEDRNK2" #"propensity_score" #'prognostic_score'
# hp_mean = pheatmap::pheatmap(measure_variation$measure_mean[var_interest,,],
#                    cluster_cols = FALSE,
#                    treeheight_row = 0, treeheight_col = 0)
# hp_var = pheatmap::pheatmap(measure_variation$measure_variance[var_interest,,],
#                             cluster_rows = FALSE,
#                             cluster_cols = FALSE,
#                              treeheight_row = 0, treeheight_col = 0)

