
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Joint variable importance plot

<!-- badges: start -->
<!-- badges: end -->

Joint variable importance plot (jointVIP) visualizes each variable’s
outcome importance via Pearson’s correlation and treatment importance
via omitted variable bias-based standardized mean differences. Bias
curves enable comparisons to support prioritization.

## Installation

You can install the development version of jointVIP from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ldliao/jointVIP")
```

## BRFSS Example

To demonstrate, we use the 2015 Behavioral Risk Factor Surveillance
System (BRFSS) example to answer the causal question: Does smoking
increase the risk of chronic obstructive pulmonary disease (COPD)? The
data and background is inspired by [Clay Ford’s work from University of
Virginia
Library](https://data.library.virginia.edu/getting-started-with-matching-methods/).
First, the data is cleaned to only have numeric variables, i.e., all
factored variables are transformed via one-hot-encoding. Treatment
variable `smoke` only contains 0 (control) and 1 (treatment).

With the cleaned data, you can specify details in the function
`create_jointVIP()` like so:

``` r
library(jointVIP)
## basic example code

treatment = 'smoke'
outcome = 'COPD'
covariates = names(df)[!names(df) %in% c(treatment, outcome)]

## select the pilot sample from random portion
## pilot data here are considered as 'external controls'
set.seed(1234895)
pilot_prop = 0.15
pilot_sample_num = sample(which(df %>% pull(treatment) == 0),
                          length(which(df %>% pull(treatment) == 0)) *
                          pilot_prop)

## set up pilot and analysis data
## we want to make sure these two data are non-overlapping
pilot_df = df[pilot_sample_num, ]
analysis_df = df[-pilot_sample_num, ]

## minimal example
brfss_jointVIP = create_jointVIP(treatment = treatment,
                                 outcome = outcome,
                                 covariates = covariates,
                                 pilot_df = pilot_df,
                                 analysis_df = analysis_df)
```

Generic functions can be used for the `jointVIP` object to extract
information as a glance with `summary()` and `print()`.

``` r
summary(brfss_jointVIP)
#> Max absolute bias is 0.042
#> 3 variables are above the desired 0.01 absolute bias tolerance
#> 13 variables can be plotted
print(brfss_jointVIP)
#>                 bias
#> average_drinks 0.042
#> age_over65     0.024
#> age_25to34     0.016
```

``` r
plot(brfss_jointVIP)
```

<img src="man/figures/README-plot-1.png" width="80%" style="display: block; margin: auto;" />
