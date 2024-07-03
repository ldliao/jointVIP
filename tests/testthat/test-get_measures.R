test_that("get_measures runs smoothly", {
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

  expect_no_error(get_measures(new_jointVIP))
})

test_that("warnings and stop with errors in measures", {
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
  pilot_df3 = pilot_df2 = pilot_df1 = pilot_df
  analysis_df3 = analysis_df2 = analysis_df1 = analysis_df
  pilot_df2[['const']] = 5
  analysis_df2[['const']] = 4
  pilot_df3[['const']] = rnorm(nrow(pilot_df2), mean=4)
  analysis_df3[['const']] = 4

  pilot_df1[['lifeExp2']] = pilot_df1[['lifeExp']]
  analysis_df1[['lifeExp2']] = analysis_df1[['lifeExp']]

  treatment = "trt"
  outcome = "out"

  covariates1 = names(analysis_df1)[!names(analysis_df1)
                                  %in% c(treatment, outcome)]

  new_jointVIP1 = create_jointVIP(treatment,
                                 outcome,
                                 covariates1,
                                 pilot_df1,
                                 analysis_df1)

  expect_warning(get_measures(new_jointVIP1),
                 fixed = TRUE,
                 "Variables lifeExp lifeExp2 measures are duplicated (all multiples are shown). \nOnly unique variables will be used. ")


  covariates2 = names(analysis_df2)[!names(analysis_df2)
                                    %in% c(treatment, outcome)]

  new_jointVIP2 = create_jointVIP(treatment,
                                  outcome,
                                  covariates2,
                                  pilot_df2,
                                  analysis_df2)

  w <- capture_warnings(get_measures(new_jointVIP2))
  expect_equal(w[1], "the standard deviation is zero")
  expect_equal(w[2], "Variable(s) const measures contain missing values. \nThey are dropped when plotting. ")

  covariates3 = names(analysis_df3)[!names(analysis_df3)
                                    %in% c(treatment, outcome)]

  new_jointVIP3 = create_jointVIP(treatment,
                                  outcome,
                                  covariates3,
                                  pilot_df3,
                                  analysis_df3)

  expect_warning(get_measures(new_jointVIP3),
                 fixed = TRUE,
                 "The standardized mean difference for variable(s) const are 0. \nTheir biases are automatically 0. ")

  expect_error(check_measures(data.frame()), fixed=TRUE,
               "measures is empty, please check for errors that may have occurred")
})
