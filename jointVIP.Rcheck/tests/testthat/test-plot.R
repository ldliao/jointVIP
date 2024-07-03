library(testthat)
test_that("plot.jointVIP() is able to show the desired plot", {
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
  p3 <- plot(new_jointVIP, smd = 'standard')

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

  expect_equal("jointVIP for out", p1$labels$title)
  expect_equal("Joint Variable Importance Plot", p2$labels$title)
  expect_equal("OVB-based SMD", p1$labels$subtitle)
  expect_equal("Absolute Standardized Mean Difference", p1$labels$x)
  expect_equal("Absolute Outcome Correlation", p1$labels$y)
  expect_equal("standard SMD", p3$labels$subtitle)
  expect_equal("Standardized Mean Difference", p2$labels$x)
  expect_equal("Outcome Correlation", p2$labels$y)
  expect_equal(length(p1$layers), 8)
  expect_equal(length(plot(new_jointVIP, smd = 'standard')$layers), 2)
  expect_equal(length(plot(new_jointVIP,
                           smd = 'OVB-based',
                           bias_curve_cutoffs = c(0.05, 0.07))$layers), 5)

  expect_error(plot(new_jointVIP, smd = 'blah'), fixed=TRUE,
               "smd options only include `OVB-based` or `standard`")
  expect_error(plot(new_jointVIP, smd = 2), fixed=TRUE,
               "smd options only include `OVB-based` or `standard`")
  expect_error(plot(new_jointVIP, bias_curve_cutoffs = "a"), fixed=TRUE,
               "`bias_curve_cutoffs` must be numeric")
  expect_warning(plot(new_jointVIP, bias_curve_cutoffs = c(0,0.1,0.2)), fixed=TRUE,
                 "0 in the `bias_curve_cutoffs` will not be plotted")
  expect_equal(length(plot(new_jointVIP, OVB_curves = FALSE)$layers), 2)
  expect_error(plot(new_jointVIP, OVB_curves = "a"), fixed=TRUE,
               "`OVB_curves` can only be set as TRUE or FALSE")
  expect_error(plot(new_jointVIP, add_var_labs = "a"), fixed=TRUE,
               "`add_var_labs` can only be set as TRUE or FALSE")
  expect_error(plot(new_jointVIP, OVB_curves = 1), fixed=TRUE,
               "`OVB_curves` can only be set as TRUE or FALSE")
  expect_error(plot(new_jointVIP, add_var_labs = 1), fixed=TRUE,
               "`add_var_labs` can only be set as TRUE or FALSE")
  expect_equal(length(plot(new_jointVIP, add_var_labs = FALSE)$layers),
               length(plot(new_jointVIP, add_var_labs = TRUE)$layers)-1)

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
