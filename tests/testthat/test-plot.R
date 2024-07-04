test_that("jointVIP plot basic input checks", {
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
  p1 <- plot(new_jointVIP,
             plot_title = paste0("jointVIP for ", outcome))
  p2 <- plot(new_jointVIP, use_abs = FALSE)
  p3 <- plot(new_jointVIP, smd = 'pooled')

  expect_error(plot(new_jointVIP, bias_curve = 2),
               fixed=TRUE,
               "custom plot options passed into ... must be one of the following:bias_curve_cutoffs text_size max.overlaps label_cut_std_md label_cut_outcome_cor label_cut_bias bias_curves add_var_labs expanded_y_curvelab")

  expect_error(plot(new_jointVIP, add_var_labs = 1), fixed=TRUE,
               "`add_var_labs` can only be set as TRUE or FALSE"
  )
  expect_no_error(plot(new_jointVIP, use_abs = TRUE))
})


test_that("jointVIP plot layer checks", {
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
  p1 <- plot(new_jointVIP,
             plot_title = paste0("jointVIP for ", outcome))
  p2 <- plot(new_jointVIP, use_abs = FALSE)
  p3 <- plot(new_jointVIP, smd = 'pooled')

  expect_equal(capture_output(print(p1$layers[[1]])),
               paste0("geom_point: na.rm = FALSE\n",
                      "stat_identity: na.rm = FALSE\n",
                      "position_identity "))
  expect_equal(capture_output(print(p2$layers[[1]])),
               paste0("geom_point: na.rm = FALSE\n",
                      "stat_identity: na.rm = FALSE\n",
                      "position_identity "))
  expect_equal(capture_output(print(p3$layers[[1]])),
               paste0("geom_point: na.rm = FALSE\n",
                      "stat_identity: na.rm = FALSE\n",
                      "position_identity "))

  expect_equal(length(p1$layers), 8)
  expect_equal(length(plot(new_jointVIP, smd = 'pooled')$layers), 2)
  expect_equal(length(plot(new_jointVIP,
                           smd = 'cross-sample',
                           bias_curve_cutoffs = c(0.05, 0.07))$layers), 5)
  expect_equal(length(plot(new_jointVIP, bias_curves = FALSE)$layers), 2)

  expect_equal(length(plot(new_jointVIP, add_var_labs = FALSE)$layers),
               length(plot(new_jointVIP, add_var_labs = TRUE)$layers)-1)
})

test_that("jointVIP label checks", {
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
  p1 <- plot(new_jointVIP,
             plot_title = paste0("jointVIP for ", outcome))
  p2 <- plot(new_jointVIP, use_abs = FALSE)
  p3 <- plot(new_jointVIP, smd = 'pooled')
})

test_that("jointVIP input expect errors", {
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
  p1 <- plot(new_jointVIP,
             plot_title = paste0("jointVIP for ", outcome))
  p2 <- plot(new_jointVIP, use_abs = FALSE)
  p3 <- plot(new_jointVIP, smd = 'pooled')

  expect_error(plot(new_jointVIP, smd = 'blah'), fixed=TRUE,
               "smd options only include `cross-sample` or `pooled`")
  expect_error(plot(new_jointVIP, smd = 2), fixed=TRUE,
               "smd options only include `cross-sample` or `pooled`")
  expect_error(plot(new_jointVIP, bias_curve_cutoffs = "a"), fixed=TRUE,
               "`bias_curve_cutoffs` must be numeric")
  expect_warning(plot(new_jointVIP, bias_curve_cutoffs = c(0,0.1,0.2)), fixed=TRUE,
                 "0 in the `bias_curve_cutoffs` will not be plotted")
  expect_error(plot(new_jointVIP, bias_curves = "a"), fixed=TRUE,
               "`bias_curves` can only be set as TRUE or FALSE")
  expect_error(plot(new_jointVIP, add_var_labs = "a"), fixed=TRUE,
               "`add_var_labs` can only be set as TRUE or FALSE")
  expect_error(plot(new_jointVIP, bias_curves = 1), fixed=TRUE,
               "`bias_curves` can only be set as TRUE or FALSE")
  expect_error(plot(new_jointVIP, add_var_labs = 1), fixed=TRUE,
               "`add_var_labs` can only be set as TRUE or FALSE")

  expect_error(plot(new_jointVIP, max.overlaps = 0), fixed=TRUE,
               "`max.overlaps` must be a positive numeric")
  expect_error(plot(new_jointVIP, max.overlaps = TRUE), fixed=TRUE,
               "`max.overlaps` must be a positive numeric")
  expect_error(plot(new_jointVIP, max.overlaps = -3), fixed=TRUE,
               "`max.overlaps` must be a positive numeric")

  expect_error(plot(new_jointVIP, label_cut_std_md = 0), fixed=TRUE,
               "`label_cut_std_md` must be a positive numeric")
  expect_error(plot(new_jointVIP, label_cut_std_md = TRUE), fixed=TRUE,
               "`label_cut_std_md` must be a positive numeric")
  expect_error(plot(new_jointVIP, label_cut_std_md = -3), fixed=TRUE,
               "`label_cut_std_md` must be a positive numeric")

  expect_error(plot(new_jointVIP, label_cut_outcome_cor = 0), fixed=TRUE,
               "`label_cut_outcome_cor` must be a positive numeric")
  expect_error(plot(new_jointVIP, label_cut_outcome_cor = TRUE), fixed=TRUE,
               "`label_cut_outcome_cor` must be a positive numeric")
  expect_error(plot(new_jointVIP, label_cut_outcome_cor = -3), fixed=TRUE,
               "`label_cut_outcome_cor` must be a positive numeric")

  expect_error(plot(new_jointVIP, label_cut_bias = 0), fixed=TRUE,
               "`label_cut_bias` must be a positive numeric")
  expect_error(plot(new_jointVIP, label_cut_bias = TRUE), fixed=TRUE,
               "`label_cut_bias` must be a positive numeric")
  expect_error(plot(new_jointVIP, label_cut_bias = -3), fixed=TRUE,
               "`label_cut_bias` must be a positive numeric")

  expect_warning(capture_output(plot(new_jointVIP, "standard")), fixed = TRUE,
                 "anything passed in ... must be named or it'll be ignored")
})


test_that("post_jointVIP basic checks", {
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

  post_data2 <- data.frame(
    year = rnorm(50, 200, 10),
    pop = rnorm(50, 1000, 1500),
    gdpPercap = runif(50, 100, 5000),
    trt = rbinom(50, 1, 0.5),
    out = rnorm(50, 1, 2)
  )
  post_jointVIP2 = create_post_jointVIP(new_jointVIP, post_data2)

  expect_warning(plot(post_jointVIP2), fixed=TRUE,
                 "Color not scaled to previous pre-bias plot since the post-bias is greater than pre-bias")

  expect_no_error(plot(post_jointVIP))
  expect_no_error(plot(post_jointVIP, smd = "pooled"))
  expect_no_error(plot(post_jointVIP, bias_curves=TRUE))
  expect_error(plot(post_jointVIP, add_var_labs = -1), fixed = TRUE,
               "`add_var_labs` can only be set as TRUE or FALSE")
  expect_error(plot(post_jointVIP, text_size = -0.1), fixed = TRUE,
               "`text_size` must be a positive numeric")
  expect_error(plot(post_jointVIP, max.overlaps = -0.1), fixed = TRUE,
               "`max.overlaps` must be a positive numeric")
  expect_error(plot(post_jointVIP, add_post_labs = 0), fixed = TRUE,
               "`add_post_labs` can only be set as TRUE or FALSE")
  expect_error(plot(post_jointVIP, text_size = -2), fixed = TRUE,
               "`text_size` must be a positive numeric")
  expect_error(plot(post_jointVIP, max.overlaps = -2), fixed = TRUE,
               "`max.overlaps` must be a positive numeric")
  expect_error(plot(post_jointVIP, post_label_cut_bias = -0.1),
               fixed = TRUE,
               "`post_label_cut_bias` must be a positive numeric")
  expect_error(plot(post_jointVIP, post_label_cut_bias = "c"),
               fixed = TRUE,
               "`post_label_cut_bias` must be a positive numeric")
  expect_error(plot(post_jointVIP, text_size = -0.1),
               fixed = TRUE,
               "`text_size` must be a positive numeric")
  expect_error(plot(post_jointVIP, text_size = "c"),
               fixed = TRUE,
               "`text_size` must be a positive numeric")
})

test_that("post_jointVIP plot layer checks", {
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
})

test_that("jointVIP bootstrap plot checks", {
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

  post_data <- data.frame(
    year = rnorm(50, 200, 4),
    gdpPercap = runif(50, 100, 800),
    lifeExp = rpois(50, 75),
    trt = rbinom(50, 1, 0.5),
    out = rnorm(50, 1, 0.2)
  )
  post_jointVIP = create_post_jointVIP(new_jointVIP, post_data)
  expect_error(bootstrap.plot(post_jointVIP), fixed = TRUE,
               "bootstrap_plot function only applicable to class jointVIP only!")

  expect_no_error(bootstrap.plot(new_jointVIP,use_abs = FALSE))

  set.seed(18283)
  expect_equal(length(bootstrap.plot(new_jointVIP, smd = "pooled")$layers),
               4)
  expect_equal(length(bootstrap.plot(new_jointVIP)$layers),
               10)
  expect_error(bootstrap.plot(new_jointVIP, bias_curves = 0.2), fixed=TRUE,
               "`bias_curves` can only be set as TRUE or FALSE")
  expect_error(bootstrap.plot(new_jointVIP, B = 1e7),fixed=TRUE,
               "B too large, please specify this number to be under 990000")

  expect_equal(length(bootstrap.plot(new_jointVIP, B = 15)$layers), 10)
  expect_error(bootstrap.plot(new_jointVIP, B = TRUE),fixed=TRUE,
               "B is the number of bootstrap step should run; please input a numeric\nThe ceiling of such number will be used.")

  expect_error(bootstrap.plot(new_jointVIP, B = 0),fixed=TRUE,
               "B is too small please make it a larger number")
})
