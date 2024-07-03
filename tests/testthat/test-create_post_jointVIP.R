test_that("check create_post_jointVIP creation", {
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

  post_data2 <- data.frame(
    year = rnorm(50, 200, 5),
    pop = rnorm(50, 1000, 500),
    gdpPercap = runif(50, 100, 1000),
    trt = sample(c(1,2,0), 50, replace=TRUE),
    out = rnorm(50, 1, 0.2)
  )

  expect_no_error(create_post_jointVIP(new_jointVIP, post_data))

  expect_error(create_post_jointVIP(new_jointVIP, data.frame()), fixed = TRUE,
               "`post_analysis_df` cannot be an empty data.frame")

  expect_error(create_post_jointVIP(new_jointVIP, matrix(c(12,2,3,2,4,5), nrow = 3)), fixed = TRUE,
               "`post_analysis_df` must be a data.frame class")

  expect_error(create_post_jointVIP(new_jointVIP, post_data2), fixed = TRUE,
               "`treatment` must be binary: 0 (control) and 1 (treated)")
})


test_that("check create_post_jointVIP weights", {
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
})

test_that("error arise for invalid construction for post_jointVIP", {
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

  post_data[['hi']] <- sample(c('A','C'), 50, TRUE)
  expect_error(create_post_jointVIP(new_jointVIP, post_data, wts = rep(0, nrow(post_data))),
               fixed = TRUE,
               "`post_analysis_df` must have the same covariates, treatment, and outcome in `analysis_df`")

})

test_that("validity check for categorical/factor variable", {
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

  set.seed(12847)
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

  post_data <- data.frame(
    year = rnorm(50, 200, 5),
    pop = rnorm(50, 1000, 500),
    gdpPercap = runif(50, 100, 1000),
    trt = rbinom(50, 1, 0.5),
    out = rnorm(50, 1, 0.2)
  )
  post_data[['hi']] <- sample(c('A','C'), 50, TRUE)
  post_jointVIP = create_post_jointVIP(new_jointVIP, post_data)

  expect_no_error(create_post_jointVIP(new_jointVIP,
                                       post_data),
  )
})

