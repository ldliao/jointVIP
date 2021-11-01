test_that("get_diff works", {
  analysis_sample_sm = data.frame(X1 = c(rnorm(5000, 3, 1),
                                         rnorm(5000, 10, 1)),
                                  X2 = rexp(10000, 2),
                                  treat = c(rep(0,5000),
                                            rep(1,5000)))
  X1diff = mean(analysis_sample_sm[analysis_sample_sm$treat==1,
                                 'X1']) - mean(analysis_sample_sm[analysis_sample_sm$treat==0,
                                                                  'X1'])
  expect_equal(round(X1diff), round(get_diff(covariate = analysis_sample_sm$X1,
                                     analysis_sample = analysis_sample_sm,
                                     treatment='treat')))
  expect_equal(0, round(get_diff(covariate = analysis_sample_sm$X2,
                                             analysis_sample = analysis_sample_sm,
                                             treatment='treat')))
})

