% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{bootstrap.plot}
\alias{bootstrap.plot}
\title{plot the bootstrap version of the jointVIP object}
\usage{
bootstrap.plot(
  x,
  ...,
  smd = "cross-sample",
  use_abs = TRUE,
  plot_title = "Joint Variable Importance Plot",
  B = 100
)
}
\arguments{
\item{x}{a jointVIP object}

\item{...}{custom options: \code{bias_curve_cutoffs}, \code{text_size}, \code{max.overlaps}, \code{label_cut_std_md}, \code{label_cut_outcome_cor}, \code{label_cut_bias}, \code{bias_curves}, \code{add_var_labs}}

\item{smd}{specify the standardized mean difference is \code{cross-sample} or \code{pooled}}

\item{use_abs}{TRUE (default) for absolute measures}

\item{plot_title}{optional string for plot title}

\item{B}{100 (default) for the number of times the bootstrap step wished to run}
}
\value{
a joint variable importance plot of class \code{ggplot}
}
\description{
plot the bootstrap version of the jointVIP object
}
\examples{
data <- data.frame(year = rnorm(50, 200, 5),
                   pop = rnorm(50, 1000, 500),
                   gdpPercap = runif(50, 100, 1000),
                   trt = rbinom(50, 1, 0.5),
                   out = rnorm(50, 1, 0.2))
# random 20 percent of control as pilot data
pilot_sample_num = sample(which(data$trt == 0),
                          length(which(data$trt == 0)) *
                          0.2)
pilot_df = data[pilot_sample_num, ]
analysis_df = data[-pilot_sample_num, ]
treatment = "trt"
outcome = "out"
covariates = names(analysis_df)[!names(analysis_df)
                                \%in\% c(treatment, outcome)]
new_jointVIP = create_jointVIP(treatment = treatment,
                               outcome = outcome,
                               covariates = covariates,
                               pilot_df = pilot_df,
                               analysis_df = analysis_df)
# more bootstrap number B would be typically used in real settings
# this is just a small example
set.seed(1234567891)
bootstrap.plot(new_jointVIP, B = 15)
}
