library(testthat)
test_that("summary.jointVIP() is able to show the desired summary", {
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

  expect_output(summary(new_jointVIP))
  expect_equal(capture_output(summary(new_jointVIP)),
                 paste0("Max absolute bias is 0.765\n4 variables",
                  " are above the desired 0.01 absolute bias tolerance\n4",
                  " variables can be plotted"))
  expect_warning(capture_output(summary(new_jointVIP, "standard")), fixed = TRUE,
                 "anything passed in ... must be named or it'll be ignored")
})
