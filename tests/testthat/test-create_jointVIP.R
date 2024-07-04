test_that("create_jointVIP function creates a valid jointVIP S3 object", {
  set.seed(1234567891)
  data <- data.frame(year = rnorm(50, 200, 5),
                     pop = rnorm(50, 1000, 500),
                     gdpPercap = runif(50, 100, 1000),
                     lifeExp = rpois(50, 75),
                     trt = rbinom(50, 1, 0.5),
                     out = rnorm(50, 1, 0.2),
                     metro = ifelse(rbinom(50, 1, prob = 0.4) == 1, 'Y', 'N'),
                     three_lvl_fac = sample(c('A', 'B', 'C'), 50, replace = TRUE, prob = c(0.3, 0.25, 0.45)))
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
  expect_equal(dim(new_jointVIP$pilot_df), c(4,8))
})

test_that("error arise for invalid construction", {
  set.seed(1234567891)
  data <- data.frame(year = rnorm(50, 200, 5),
                     pop = rnorm(50, 1000, 500),
                     gdpPercap = runif(50, 100, 1000),
                     lifeExp = rpois(50, 75),
                     trt = rbinom(50, 1, 0.5),
                     out = rnorm(50, 1, 0.2),
                     metro = ifelse(rbinom(50, 1, prob = 0.4) == 1, 'Y', 'N'),
                     three_lvl_fac = sample(c('A', 'B', 'C'), 50, replace = TRUE, prob = c(0.3, 0.25, 0.45)))
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

  expect_error(create_jointVIP(treatment,
                               outcome,
                               covariates,
                               as.matrix(pilot_df),
                               as.matrix(analysis_df)),
               "`pilot_df` and `analysis_df` must both have data.frame classes",
               fixed=TRUE)

  expect_error(create_jointVIP(treatment,
                               outcome,
                               covariates,
                               pilot_df,
                               analysis_df[,c(covariates, "out")]),
               "`treatment` variable must be in analysis_df",
               fixed=TRUE)

  pilot_df3 = pilot_df2 = pilot_df
  pilot_df2$out = 3
  expect_error(create_jointVIP(treatment,
                               outcome,
                               covariates,
                               pilot_df2,
                               analysis_df),
               "`pilot_df` outcome must have some variation",
               fixed=TRUE)

  pilot_df3$trt = c(0, 1, 1, 0)
  expect_error(create_jointVIP(treatment,
                               outcome,
                               covariates,
                               pilot_df3,
                               analysis_df),
               "`pilot_df` should only be controls only",
               fixed=TRUE)
})


test_that("factored and numeric data are set correctly", {
  set.seed(1234567891)
  data <- data.frame(year = rnorm(50, 200, 5),
                     pop = rnorm(50, 1000, 500),
                     gdpPercap = runif(50, 100, 1000),
                     lifeExp = rpois(50, 75),
                     trt = rbinom(50, 1, 0.5),
                     out = rnorm(50, 1, 0.2),
                     metro = ifelse(rbinom(50, 1, prob = 0.4) == 1, 'Y', 'N'),
                     three_lvl_fac = sample(c('A', 'B', 'C'), 50, replace = TRUE, prob = c(0.3, 0.25, 0.45)))
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
  expect_equal(new_jointVIP$pilot_df$metro_N, as.numeric(pilot_df$metro == 'N'))
  expect_true(all(c("metro_N", "three_lvl_fac_A") %in% names(new_jointVIP$pilot_df)))

  data[['out']] = as.factor(data[['out']])
  pilot_sample_num = sample(which(data$trt == 0),
                            length(which(data$trt == 0)) *
                              0.2)
  pilot_df = data[pilot_sample_num, ]
  analysis_df = data[-pilot_sample_num, ]
  treatment = "trt"
  outcome = "out"
  covariates = names(analysis_df)[!names(analysis_df)
                                  %in% c(treatment, outcome)]
  expect_error(create_jointVIP(treatment,
                               outcome,
                               covariates,
                               pilot_df,
                               analysis_df),
               "`outcome` must be denoting a numeric variable",
               fixed=TRUE)
})
