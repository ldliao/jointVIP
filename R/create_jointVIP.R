#' create jointVIP object
#'
#' This is creates the jointVIP object & check inputs
#' @param treatment string denoting the name of the binary treatment variable, containing numeric values: 0 denoting control and 1 denoting treated
#' @param outcome string denoting the name of a numeric outcome variable
#' @param covariates vector of strings or list denoting column names of interest
#' @param pilot_df data.frame of the pilot data; character and factor variables are automatically one-hot encoded
#' @param analysis_df data.frame of the analysis data; character and factor variables are automatically one-hot encoded
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
    }  else if ((!is.numeric(pilot_df[,outcome])) & (!is.numeric(analysis_df[,outcome]))) {
      stop("`outcome` must be denoting a numeric variable")
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

    ## one hot encoding
    pilot_df = one_hot(pilot_df)
    analysis_df = one_hot(analysis_df)

    if(!identical(names(pilot_df[, -c(1, 2)]),names(analysis_df[, -c(1, 2)]))){
      full_covs = c(names(pilot_df[, -c(1, 2)]),names(analysis_df[, -c(1, 2)]))
      in_dat = duplicated(full_covs) | duplicated(full_covs, fromLast = TRUE)
      covs = unique(full_covs[in_dat])
      cat("dropping some levels due to mismatch after one-hot encoding\nkeeping variables that exist in both dataframes")
      pilot_df = pilot_df[,c(treatment, outcome, covs)]
      analysis_df = analysis_df[,c(treatment, outcome, covs)]
      covariates = covs
    }

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
