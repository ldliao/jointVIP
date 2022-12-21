test_that("post-related methods work!", {
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
  expect_equal(capture_output(print(post_jointVIP)),
               "           bias post_bias\npop       0.166     0.091\ngdpPercap 0.012     0.111")
  
  expect_equal(
    capture_output(summary(post_jointVIP)),
    "Max absolute bias is 0.166\n2 variables are above the desired 0.01 absolute bias tolerance\n3 variables can be plotted\n\nMax absolute post-bias is 0.111\nPost-measure has 3 variable(s) above the desired 0.005 absolute bias tolerance"
  )
  
  
  
  expect_equal(length(
    plot(
      post_jointVIP,
      plot_title = "Post-match jointVIP",
      smd = 'OVB-based',
      use_abs = FALSE,
      add_post_labs = TRUE,
      post_label_cut_bias = 0.001
    )$layers
  ),
  15)
  
  expect_equal(length(
    plot(
      post_jointVIP,
      plot_title = "Post-match jointVIP",
      smd = 'OVB-based',
      use_abs = FALSE,
      add_post_labs = FALSE,
      post_label_cut_bias = 0.001
    )$layers
  ),
  14)
  
  expect_equal(length(
    plot(
      post_jointVIP,
      plot_title = "Post-match jointVIP",
      smd = 'standard',
      use_abs = FALSE,
      add_post_labs = FALSE,
      post_label_cut_bias = 0.001
    )$layers
  ),
  3)
})
