test_that("jointVIP summary check", {
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
  expect_output(summary(new_jointVIP, use_abs = FALSE))
  expect_equal(capture_output(summary(new_jointVIP)),
               paste0("Max absolute bias is 0.765\n4 variables",
                      " are above the desired 0.01 absolute bias tolerance\n4",
                      " variables can be plotted"))
  expect_warning(capture_output(summary(new_jointVIP, "standard")), fixed = TRUE,
                 "anything passed in ... must be named or it'll be ignored")
  expect_warning(capture_output(summary(new_jointVIP, bias_tol = -0.1)), fixed = TRUE,
                 "bias_tol` will be treated as positive")
})


test_that("post_jointVIP summary check", {
  set.seed(1234567891)
  data <- data.frame(
    year = rnorm(50, 200, 5),
    pop = rnorm(50, 1000, 500),
    gdpPercap = runif(50, 100, 1000),
    trt = rbinom(50, 1, 0.5),
    out = rnorm(50, 1, 0.2)
  )
  pilot_sample_num = sample(which(data$trt == 0),
                            length(which(data$trt == 0)) *
                              0.2)
  pilot_df = data[pilot_sample_num,]
  analysis_df = data[-pilot_sample_num,]
  treatment = "trt"
  outcome = "out"
  covariates = names(analysis_df)[!names(analysis_df)
                                  %in% c(treatment, outcome)]
  new_jointVIP <- create_jointVIP(treatment,
                                  outcome,
                                  covariates,
                                  pilot_df,
                                  analysis_df)
  # at this step typically you may wish to do matching or weighting
  # the results after can be stored as a post_data
  # the post_data here is not matched or weighted, only for illustrative purposes
  post_data <- data.frame(
    year = rnorm(50, 200, 5),
    pop = rnorm(50, 1000, 500),
    gdpPercap = runif(50, 100, 1000),
    trt = rbinom(50, 1, 0.5),
    out = rnorm(50, 1, 0.2)
  )
  post_jointVIP = create_post_jointVIP(new_jointVIP, post_data)
  expect_equal(
    capture_output(summary(post_jointVIP)),
    "Max absolute bias is 0.166\n2 variables are above the desired 0.01 absolute bias tolerance\n3 variables can be plotted\n\nMax absolute post-bias is 0.111\nPost-measure has 3 variable(s) above the desired 0.005 absolute bias tolerance"
  )
  expect_warning(capture_output(summary(post_jointVIP, "standard")), fixed = TRUE,
                 "anything passed in ... must be named or it'll be ignored")
  expect_output(summary(post_jointVIP))
  expect_output(summary(post_jointVIP, use_abs = FALSE))
})
