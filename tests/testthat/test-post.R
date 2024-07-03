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
  
  # testing wts here
  expect_error(create_post_jointVIP(new_jointVIP, post_data, wts = 1),
               fixed = TRUE,
               "length of `wts` must be the same number of rows as `post_analysis_df`")
  
  expect_error(create_post_jointVIP(new_jointVIP, post_data, wts = c(1,NA)),
               fixed = TRUE,
               "length of `wts` must be the same number of rows as `post_analysis_df`")
  
  expect_error(create_post_jointVIP(new_jointVIP, post_data, wts = rep(NA, nrow(post_data))),
               fixed = TRUE,
               "`wts` must be numeric")
  
  expect_error(create_post_jointVIP(new_jointVIP, post_data, wts = c(1,rep(NA, nrow(post_data)-1))),
               fixed = TRUE,
               "`wts` cannot contain NA or all be 0")
  
  expect_error(create_post_jointVIP(new_jointVIP, post_data, wts = c(1, rep(NULL, nrow(post_data)-1))),
               fixed = TRUE,
               "length of `wts` must be the same number of rows as `post_analysis_df`")
  
  expect_error(create_post_jointVIP(new_jointVIP, post_data, wts = rep(0, nrow(post_data))),
               fixed = TRUE,
               "`wts` cannot contain NA or all be 0")
  
  expect_no_error(create_post_jointVIP(new_jointVIP, post_data, wts = rep(1,nrow(post_data))),
          )
  
  expect_no_error(create_post_jointVIP(new_jointVIP, 
                                       post_data, 
                                       wts = sample(1:10,nrow(post_data),replace = T)),
  )
  
  expect_equal(length(
    plot(
      post_jointVIP,
      plot_title = "Post-match jointVIP",
      smd = 'cross-sample',
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
      smd = 'cross-sample',
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
      smd = 'pooled',
      use_abs = FALSE,
      add_post_labs = FALSE,
      post_label_cut_bias = 0.001
    )$layers
  ),
  3)
  
  post_data[['hi']] <- sample(c('A','C'), 50, TRUE)
  expect_error(create_post_jointVIP(new_jointVIP, post_data, wts = rep(0, nrow(post_data))),
               fixed = TRUE,
               "`post_analysis_df` must have the same covariates, treatment, and outcome in `analysis_df`")
  
  
  pilot_df[['hi']] <- sample(c('A','C'), nrow(pilot_df), TRUE)
  analysis_df[['hi']] <- sample(c('A','C'), nrow(analysis_df), TRUE)
  treatment = "trt"
  outcome = "out"
  covariates = names(analysis_df)[!names(analysis_df)
                                  %in% c(treatment, outcome)]
  new_jointVIP <- create_jointVIP(treatment,
                                  outcome,
                                  covariates,
                                  pilot_df,
                                  analysis_df)
  expect_no_error(create_post_jointVIP(new_jointVIP, 
                                       post_data),
  )
})
