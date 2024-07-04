test_that("jointVIP print check", {
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

  expect_output(print(new_jointVIP))
  expect_output(print(new_jointVIP, use_abs = FALSE))
  expect_true(all(as.numeric(unlist(stringr::str_extract_all(capture.output(print(new_jointVIP, use_abs = TRUE)), "\\d+\\.\\d+"))) > 0))
  expect_warning(capture_output(print(new_jointVIP, "standard")), fixed = TRUE,
                 "anything passed in ... must be named or it'll be ignored")
  expect_equal(capture_output(print(new_jointVIP)),
               "           bias\npop       0.765\nyear      0.211\ngdpPercap 0.152\nlifeExp   0.032")
})

test_that("post_jointVIP print check", {
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
  expect_output(print(post_jointVIP))
  expect_output(print(post_jointVIP, use_abs = FALSE))
  expect_true(all(as.numeric(unlist(stringr::str_extract_all(capture.output(print(post_jointVIP, use_abs = TRUE)), "\\d+\\.\\d+"))) > 0))

  expect_warning(capture_output(print(post_jointVIP, "standard")), fixed = TRUE,
                 "anything passed in ... must be named or it'll be ignored")
  expect_equal(capture_output(print(post_jointVIP, smd = "pooled")),
               "           bias post_bias\npop       0.182     0.099\ngdpPercap 0.011     0.104")
  expect_equal(capture_output(print(post_jointVIP)),
               "           bias post_bias\npop       0.166     0.091\ngdpPercap 0.012     0.111")
})

