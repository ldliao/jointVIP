test_that("bootstap related code works", {
  set.seed(1234567891)
  data <- data.frame(year = rnorm(50, 200, 5),
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
  expect_equal(length(bootstrap.plot(new_jointVIP)$layers),
               10)
  expect_error(bootstrap.plot(new_jointVIP, B = 1e7),fixed=TRUE,
               "B too large, please specify this number to be under 990000")

  expect_equal(length(bootstrap.plot(new_jointVIP, B = 15)$layers), 10)
  expect_error(bootstrap.plot(new_jointVIP, B = TRUE),fixed=TRUE,
               "B is the number of bootstrap step should run; please input a numeric\nThe ceiling of such number will be used.")

  expect_error(bootstrap.plot(new_jointVIP, B = 0),fixed=TRUE,
               "B is too small please make it a larger number")
})
