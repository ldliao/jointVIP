pkgname <- "jointVIP"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "jointVIP-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('jointVIP')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("bootstrap.plot")
### * bootstrap.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bootstrap.plot
### Title: plot the bootstrap version of the jointVIP object
### Aliases: bootstrap.plot

### ** Examples

data <- data.frame(year = rnorm(50, 200, 5),
                   pop = rnorm(50, 1000, 500),
                   gdpPercap = runif(50, 100, 1000),
                   trt = rbinom(50, 1, 0.5),
                   out = rnorm(50, 1, 0.2))
# random 20 percent of control as pilot data                  
pilot_sample_num = sample(which(data$trt == 0),
                          length(which(data$trt == 0)) *
                          0.2)
pilot_df = data[pilot_sample_num, ]
analysis_df = data[-pilot_sample_num, ]
treatment = "trt"
outcome = "out"
covariates = names(analysis_df)[!names(analysis_df)
                                %in% c(treatment, outcome)]
new_jointVIP = create_jointVIP(treatment = treatment,
                               outcome = outcome,
                               covariates = covariates,
                               pilot_df = pilot_df,
                               analysis_df = analysis_df)
# more bootstrap number B would be typically used in real settings
# this is just a small example
set.seed(1234567891)
bootstrap.plot(new_jointVIP, B = 15)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bootstrap.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_jointVIP")
### * create_jointVIP

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_jointVIP
### Title: create jointVIP object
### Aliases: create_jointVIP

### ** Examples


data <- data.frame(year = rnorm(50, 200, 5),
                   pop = rnorm(50, 1000, 500),
                   gdpPercap = runif(50, 100, 1000),
                   trt = rbinom(50, 1, 0.5),
                   out = rnorm(50, 1, 0.2))
# random 20 percent of control as pilot data                  
pilot_sample_num = sample(which(data$trt == 0),
                          length(which(data$trt == 0)) *
                          0.2)
pilot_df = data[pilot_sample_num, ]
analysis_df = data[-pilot_sample_num, ]
treatment = "trt"
outcome = "out"
covariates = names(analysis_df)[!names(analysis_df)
                                %in% c(treatment, outcome)]
new_jointVIP = create_jointVIP(treatment = treatment,
                               outcome = outcome,
                               covariates = covariates,
                               pilot_df = pilot_df,
                               analysis_df = analysis_df)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_jointVIP", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create_post_jointVIP")
### * create_post_jointVIP

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create_post_jointVIP
### Title: create jointVIP object
### Aliases: create_post_jointVIP

### ** Examples

data <- data.frame(year = rnorm(50, 200, 5),
                   pop = rnorm(50, 1000, 500),
                   gdpPercap = runif(50, 100, 1000),
                   trt = rbinom(50, 1, 0.5),
                   out = rnorm(50, 1, 0.2))
# random 20 percent of control as pilot data                  
pilot_sample_num = sample(which(data$trt == 0),
                          length(which(data$trt == 0)) *
                          0.2)
pilot_df = data[pilot_sample_num, ]
analysis_df = data[-pilot_sample_num, ]
treatment = "trt"
outcome = "out"
covariates = names(analysis_df)[!names(analysis_df)
                                %in% c(treatment, outcome)]
new_jointVIP = create_jointVIP(treatment = treatment,
                               outcome = outcome,
                               covariates = covariates,
                               pilot_df = pilot_df,
                               analysis_df = analysis_df)
                               
## at this step typically you may wish to do matching or weighting
## the results after can be stored as a post_data
## the post_data here is not matched or weighted, only for illustrative purposes
post_data <- data.frame(year = rnorm(50, 200, 5),
                        pop = rnorm(50, 1000, 500),
                        gdpPercap = runif(50, 100, 1000),
                        trt = rbinom(50, 1, 0.5),
                        out = rnorm(50, 1, 0.2))
post_dat_jointVIP =  create_post_jointVIP(new_jointVIP, post_data)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create_post_jointVIP", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.jointVIP")
### * plot.jointVIP

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.jointVIP
### Title: plot the jointVIP object
### Aliases: plot.jointVIP

### ** Examples

data <- data.frame(year = rnorm(50, 200, 5),
                   pop = rnorm(50, 1000, 500),
                   gdpPercap = runif(50, 100, 1000),
                   trt = rbinom(50, 1, 0.5),
                   out = rnorm(50, 1, 0.2))
# random 20 percent of control as pilot data                  
pilot_sample_num = sample(which(data$trt == 0),
                          length(which(data$trt == 0)) *
                          0.2)
pilot_df = data[pilot_sample_num, ]
analysis_df = data[-pilot_sample_num, ]
treatment = "trt"
outcome = "out"
covariates = names(analysis_df)[!names(analysis_df)
                                %in% c(treatment, outcome)]
new_jointVIP = create_jointVIP(treatment = treatment,
                               outcome = outcome,
                               covariates = covariates,
                               pilot_df = pilot_df,
                               analysis_df = analysis_df)
plot(new_jointVIP)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.jointVIP", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.post_jointVIP")
### * plot.post_jointVIP

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.post_jointVIP
### Title: plot the post_jointVIP object this plot uses the same custom
###   options as the jointVIP object
### Aliases: plot.post_jointVIP

### ** Examples

data <- data.frame(year = rnorm(50, 200, 5),
                   pop = rnorm(50, 1000, 500),
                   gdpPercap = runif(50, 100, 1000),
                   trt = rbinom(50, 1, 0.5),
                   out = rnorm(50, 1, 0.2))
# random 20 percent of control as pilot data                  
pilot_sample_num = sample(which(data$trt == 0),
                          length(which(data$trt == 0)) *
                          0.2)
pilot_df = data[pilot_sample_num, ]
analysis_df = data[-pilot_sample_num, ]
treatment = "trt"
outcome = "out"
covariates = names(analysis_df)[!names(analysis_df)
                                %in% c(treatment, outcome)]
new_jointVIP = create_jointVIP(treatment = treatment,
                               outcome = outcome,
                               covariates = covariates,
                               pilot_df = pilot_df,
                               analysis_df = analysis_df)
                               
## at this step typically you may wish to do matching or weighting
## the results after can be stored as a post_data
## the post_data here is not matched or weighted, only for illustrative purposes
post_data <- data.frame(year = rnorm(50, 200, 5),
                        pop = rnorm(50, 1000, 500),
                        gdpPercap = runif(50, 100, 1000),
                        trt = rbinom(50, 1, 0.5),
                        out = rnorm(50, 1, 0.2))
post_dat_jointVIP = create_post_jointVIP(new_jointVIP, post_data)
plot(post_dat_jointVIP)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.post_jointVIP", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print.jointVIP")
### * print.jointVIP

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print.jointVIP
### Title: Obtains a print for jointVIP object
### Aliases: print.jointVIP

### ** Examples

data <- data.frame(year = rnorm(50, 200, 5),
                   pop = rnorm(50, 1000, 500),
                   gdpPercap = runif(50, 100, 1000),
                   trt = rbinom(50, 1, 0.5),
                   out = rnorm(50, 1, 0.2))
# random 20 percent of control as pilot data                  
pilot_sample_num = sample(which(data$trt == 0),
                          length(which(data$trt == 0)) *
                          0.2)
pilot_df = data[pilot_sample_num, ]
analysis_df = data[-pilot_sample_num, ]
treatment = "trt"
outcome = "out"
covariates = names(analysis_df)[!names(analysis_df)
                                %in% c(treatment, outcome)]
new_jointVIP = create_jointVIP(treatment = treatment,
                               outcome = outcome,
                               covariates = covariates,
                               pilot_df = pilot_df,
                               analysis_df = analysis_df)
print(new_jointVIP)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print.jointVIP", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print.post_jointVIP")
### * print.post_jointVIP

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print.post_jointVIP
### Title: Obtains a print for post_jointVIP object
### Aliases: print.post_jointVIP

### ** Examples

data <- data.frame(year = rnorm(50, 200, 5),
                   pop = rnorm(50, 1000, 500),
                   gdpPercap = runif(50, 100, 1000),
                   trt = rbinom(50, 1, 0.5),
                   out = rnorm(50, 1, 0.2))
# random 20 percent of control as pilot data                  
pilot_sample_num = sample(which(data$trt == 0),
                          length(which(data$trt == 0)) *
                          0.2)
pilot_df = data[pilot_sample_num, ]
analysis_df = data[-pilot_sample_num, ]
treatment = "trt"
outcome = "out"
covariates = names(analysis_df)[!names(analysis_df)
                                %in% c(treatment, outcome)]
new_jointVIP = create_jointVIP(treatment = treatment,
                               outcome = outcome,
                               covariates = covariates,
                               pilot_df = pilot_df,
                               analysis_df = analysis_df)
                               
## at this step typically you may wish to do matching or weighting
## the results after can be stored as a post_data
## the post_data here is not matched or weighted, only for illustrative purposes
post_data <- data.frame(year = rnorm(50, 200, 5),
                        pop = rnorm(50, 1000, 500),
                        gdpPercap = runif(50, 100, 1000),
                        trt = rbinom(50, 1, 0.5),
                        out = rnorm(50, 1, 0.2))
post_dat_jointVIP = create_post_jointVIP(new_jointVIP, post_data)
print(post_dat_jointVIP)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print.post_jointVIP", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.jointVIP")
### * summary.jointVIP

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.jointVIP
### Title: Obtains a summary jointVIP object
### Aliases: summary.jointVIP

### ** Examples

data <- data.frame(year = rnorm(50, 200, 5),
                   pop = rnorm(50, 1000, 500),
                   gdpPercap = runif(50, 100, 1000),
                   trt = rbinom(50, 1, 0.5),
                   out = rnorm(50, 1, 0.2))
# random 20 percent of control as pilot data                  
pilot_sample_num = sample(which(data$trt == 0),
                          length(which(data$trt == 0)) *
                          0.2)
pilot_df = data[pilot_sample_num, ]
analysis_df = data[-pilot_sample_num, ]
treatment = "trt"
outcome = "out"
covariates = names(analysis_df)[!names(analysis_df)
                                %in% c(treatment, outcome)]
new_jointVIP = create_jointVIP(treatment = treatment,
                               outcome = outcome,
                               covariates = covariates,
                               pilot_df = pilot_df,
                               analysis_df = analysis_df)
summary(new_jointVIP)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.jointVIP", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.post_jointVIP")
### * summary.post_jointVIP

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.post_jointVIP
### Title: Obtains a summary post_jointVIP object
### Aliases: summary.post_jointVIP

### ** Examples

data <- data.frame(year = rnorm(50, 200, 5),
                   pop = rnorm(50, 1000, 500),
                   gdpPercap = runif(50, 100, 1000),
                   trt = rbinom(50, 1, 0.5),
                   out = rnorm(50, 1, 0.2))
# random 20 percent of control as pilot data                  
pilot_sample_num = sample(which(data$trt == 0),
                          length(which(data$trt == 0)) *
                          0.2)
pilot_df = data[pilot_sample_num, ]
analysis_df = data[-pilot_sample_num, ]
treatment = "trt"
outcome = "out"
covariates = names(analysis_df)[!names(analysis_df)
                                %in% c(treatment, outcome)]
new_jointVIP = create_jointVIP(treatment = treatment,
                               outcome = outcome,
                               covariates = covariates,
                               pilot_df = pilot_df,
                               analysis_df = analysis_df)
                               
## at this step typically you may wish to do matching or weighting
## the results after can be stored as a post_data
## the post_data here is not matched or weighted, only for illustrative purposes
post_data <- data.frame(year = rnorm(50, 200, 5),
                        pop = rnorm(50, 1000, 500),
                        gdpPercap = runif(50, 100, 1000),
                        trt = rbinom(50, 1, 0.5),
                        out = rnorm(50, 1, 0.2))
post_dat_jointVIP = create_post_jointVIP(new_jointVIP, post_data)
summary(post_dat_jointVIP)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.post_jointVIP", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
