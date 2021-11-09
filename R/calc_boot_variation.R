#' @import dplyr tools ggplot2 ggrepel tidyr
#' @include calc_measures.R
NULL


#' @param B number of bootstrap to do
#' @param pilot_df dataframe of the pilot data
#' @param analysis_df dataframe of the analysis data
#' @param covariates vector of strings or list denoting column names of interest
#' @param treatment string denoting the name of the treatment variable
#' @param outcome string denoting the name of the outcome variable
#' @param use_abs boolean to denote whether absolute values are used to construct the measures
calc_bootstrap <- function(B, pilot_df, analysis_df,
                           covariates, treatment, outcome,
                           use_abs=F){

  set.seed(123456)
  seeds <- paste(sample(1e5:1e6, B, replace=F))

  og_measures = get_measures(pilot_df=pilot_df, analysis_df=analysis_df,
                             covariates=covariates, treatment=treatment,
                             outcome=outcome,
                             use_abs=F)

  # assigning row names background variables
  bg_vars=og_measures$measures$label

  # assigning column names measure
  msr=names(og_measures$measures[,names(og_measures$measures)!="label"])

  # 3d array
  result <- array(0, dim = c(length(bg_vars),
                             length(msr),
                             B))

  for (b in (1:B)){
    set.seed(as.numeric(seeds[b]))
    boot_pilot_df = pilot_df[sample(1:nrow(pilot_df), size=nrow(pilot_df), replace = T),]
    boot_analysis_df = analysis_df[sample(1:nrow(analysis_df), size=nrow(analysis_df), replace = T),]
    temp_measure = get_measures(pilot_df=boot_pilot_df,
                                analysis_df=boot_analysis_df,
                                covariates=covariates,
                                treatment=treatment,
                                outcome=outcome,
                                use_abs=use_abs)
    result[,,b] = as.matrix(temp_measure$measures[,names(temp_measure$measures)!="label"])
  }
  dimnames(result) = list(bg_vars, msr, seeds)

  boot_sd = apply(result*is.finite(result), c(1,2),
                  stats::quantile,
                  probs=c(0.025, 0.975),
                  na.rm = TRUE)

  return(list(og_measures = og_measures$measures, boot_sd = boot_sd))
}


#' @param og_measures original measures
#' @param boot_sd from bootstrap sd
#' @param use_denom which denom to use
#' @param joint_vip plot to add on to
plot_bootstrap <- function(og_measures, boot_sd, use_denom, joint_vip){
  if(use_denom == 'standard'){
    m = og_measures[,c('control_cor', 'label')]
    m$std_diff = og_measures$standard_difference
    m$lower_std_diff = boot_sd["2.5%",,"standard_difference"]
    m$upper_std_diff = boot_sd["97.5%",,"standard_difference"]
  }
  if(use_denom == 'pilot'){
    m = og_measures[,c('control_cor', 'label')]
    m$std_diff = og_measures$standard_difference_pilot
    m$lower_std_diff = boot_sd["2.5%",,"standard_difference_pilot"]
    m$upper_std_diff = boot_sd["97.5%",,"standard_difference_pilot"]
  }

  m$lower_cor = boot_sd["2.5%",,"control_cor"]
  m$upper_cor = boot_sd["97.5%",,"control_cor"]
  mm = tidyr::gather(m[,c('lower_std_diff','upper_std_diff','label')], 'key', 'values', -label)
  mm$cors = m$control_cor
  mm_cors = tidyr::gather(m[,c('lower_cor','upper_cor','label')], 'key', 'values', -label)
  mm_cors$std_diff = m$std_diff

  joint_vip = joint_vip +
    geom_line(data=mm, aes(x=values, y=cors, group=label), color="darkgreen",
              size=1, alpha=0.5) +
    geom_line(data=mm_cors, aes(y=values, x=std_diff, group=label), color="darkgreen",
              size=1, alpha=0.5)

  return(joint_vip)
}


