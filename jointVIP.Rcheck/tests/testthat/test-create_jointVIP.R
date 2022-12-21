library(testthat)
test_that("create_jointVIP() creates a valid JointVIP object", {
  set.seed(1234567891)
  data <- data.frame(year = rnorm(50, 200, 5),
                     pop = rnorm(50, 1000, 500),
                     gdpPercap = runif(50, 100, 1000),
                     lifeExp = rpois(50, 75),
                     trt = rbinom(50, 1, 0.5),
                     out = rnorm(50, 1, 0.2))
  pilot_sample_num = sample(which(data$trt == 0),
                            length(which(data$trt == 0)) *
                              0.2)
  pilot_df = data[pilot_sample_num, ]
  analysis_df = data[-pilot_sample_num, ]
  treatment = "trt"
  outcome = "out"
  covariates = names(analysis_df)[!names(analysis_df)
                                  %in% c(treatment, outcome)]
  new_jointVIP = create_jointVIP(treatment,
                                 outcome,
                                 covariates,
                                 pilot_df,
                                 analysis_df)
  expect_s3_class(new_jointVIP, "jointVIP")
  expect_equal(dim(new_jointVIP$pilot_df), dim(pilot_df))
  expect_error(create_jointVIP(treatment,
                               outcome,
                               covariates,
                               data.frame(),
                               analysis_df),
               "both `pilot_df` and `analysis_df` cannot be empty data.frames",
               fixed=TRUE)
  expect_error(create_jointVIP("pop",
                               outcome,
                               covariates,
                               pilot_df,
                               analysis_df),
               "`treatment` must be binary: 0 (control) and 1 (treated)",
               fixed=TRUE)
  expect_error(create_jointVIP(treatment,
                               outcome,
                               c("wonk"),
                               pilot_df,
                               analysis_df),
               "`covariates` must be in both pilot_df and analysis_df",
               fixed=TRUE)
})

