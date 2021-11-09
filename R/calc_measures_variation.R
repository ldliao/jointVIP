#' @import dplyr tools
#' @include calc_measures.R
NULL

get_measures_variation <- function(og_seed, k=20, ...){
  set.seed(og_seed)

  pilot_sample_num = sample(which(df %>% pull(treatment) == 0),
                            length(which(df %>% pull(treatment) == 0)) *
                            pilot_prop)
  pilot_df = df[pilot_sample_num, ]
  analysis_df = df[-pilot_sample_num, ]
  og_measures = get_measures(pilot_df=pilot_df, analysis_df=analysis_df,
                          covariates=covariates, treatment=treatment,
                          outcome=outcome,
                          use_abs=F)

  # repeat k random times with seeds
  set.seed(og_seed)
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
      set.seed(as.numeric(seeds[i]))
      pilot_prop=pilot_props[pp]
      pilot_sample_num = sample(which(df %>% pull(treatment) == 0),
                                length(which(df %>% pull(treatment) == 0)) *
                                  pilot_prop)
      pilot_df = df[pilot_sample_num, ]
      analysis_df = df[-pilot_sample_num, ]

      temp_measure = get_measures(pilot_df=pilot_df, analysis_df=analysis_df,
                                  covariates=covariates, treatment=treatment,
                                  outcome=outcome,
                                  use_abs=use_abs)
      result[,,i,pp] = as.matrix(temp_measure$measures[,names(temp_measure$measures)!="label"])
    }
  }
  dimnames(result) = list(bg_vars, msr, seeds, pilot_props)

  measure_variation = apply(result*is.finite(result), c(1,2,4), stats::var, na.rm = TRUE)
  return(measure_variation)
}


