---
title: 'jointVIP: Prioritizing variables in observational study design with joint
  variable importance plot in R'
tags:
- R
- observational study
- study design
- visualization
- causal inference
date: "3 February 2023"
output:
  html_document:
    df_print: paged
authors:
- name: Lauren D. Liao
  orcid: "0000-0003-4697-6909"
  corresponding: yes
  affiliation: 1
- name: Samuel D. Pimentel
  orcid: "0000-0002-0409-6586"
  affiliation: 2
bibliography: paper.bib
affiliations:
- name: Division of Biostatistics, University of California, Berkeley, USA
  index: 1
- name: Department of Statistics, University of California, Berkeley, USA
  index: 2
---

# Summary
 
Credible causal effect estimation requires treated subjects and controls to be otherwise similar. In observational settings, such as analysis of electronic health records, this is not guaranteed. Investigators must balance background variables so they are similar in treated and control groups. Common approaches include matching (grouping individuals into small homogeneous sets) or weighting (upweighting or downweighting individuals) to create similar profiles. However, creating identical distributions may be impossible if many variables are measured, and not all variables are of equal importance to the outcome. The joint variable importance plot (`jointVIP`) package to guides decisions about which variables to prioritize for adjustment by quantifying and visualizing each variable's relationship to both treatment and outcome.

# Statement of need

Consider an observational study to measure the effect of a binary treatment variable (treated/control) on an outcome, in which additional covariates (background variables) are measured. A covariate may be associated with outcomes, and it may also differ in distribution between treated and controls; if the covariate is associated with treatment and outcome, the covariate in question is a confounder. Ignored confounders introduce bias into treatment effect estimates. For instance, when testing a blood pressure drug, if older patients both take the drug more and have worse initial blood pressure, a simple difference in mean blood pressure between treated and control subjects will understate the drug's benefits. Confounding can be addressed by matching, under which blood pressure is compared only within pairs of patients with similar ages, or by weighting, in which older control subjects receive larger weights than younger control subjects when averaging blood pressure. When many potential confounders are measured, however, neither matching nor weighting can perfectly adjust for all differences, and researchers must select which variables to focus on balancing.  

Current practice for selecting variables for adjustment focuses primarily on understanding the treatment relationship, via tools such as balance tables and the Love plot [@ahmed2006; @greifer2021; @hansenbowers2008; @rosenbuam1985; @stuart2011]. A key metric is the standardized mean difference (SMD), or the difference in treated and control means over a covariate measure in standard deviations. Researchers commonly try to adjust so that all SMD values are moderately small, or focus on adjustments for variables with the largest initial SMD. However, these approaches neglect important information about the relationship of each covariate with the outcome variable, which substantially influences the degree of bias incurred by ignoring it.

To improve observational study design, we propose the joint variable importance plot (jointVIP) [@liao2023], implemented in the `jointVIP` package.  The jointVIP  represents both treatment and outcome relationships for each variable in a single image: each variable's SMD is plotted against an outcome correlation measure (computed in a pilot control sample to avoid bias from multiple use of outcome data). Bias curves based on the omitted variable bias (OVB) framework also are plotted to improve variable comparison. The jointVIP provides valuable insight into variable importance and can be used to specify key parameters in existing matching and weighting methods.

# Development

The `jointVIP` package was created in the R programming language [@cran]. The package uses the S3 object system and leverages system generic functions, `print()`, `summary()`, and `plot()`. Plotting the jointVIP object outputs a plot of the `ggplot2` class. An interactive R Shiny application, available online at https://ldliao.shinyapps.io/jointVIP/, showcases the package. 

# Usage
The `jointVIP` package is available from the Comprehensive R Archive Network [CRAN](https://CRAN.R-project.org/package=jointVIP) and [GitHub](https://github.com/ldliao/jointVIP). 

```r
# installation using CRAN:
# install.packages("jointVIP")

# installation using GitHub
# remotes::install_github('ldliao/jointVIP') 

library(jointVIP)
```

To create an object of the jointVIP class, the user needs to supply two datasets and specify the treatment, outcome, and background variable names. Two processed datasets,“pilot” and “analysis” samples, are in the form of data.frames. The analysis sample contains both treated and control groups. The pilot sample contains only control individuals, and they are excluded from the subsequent analysis stage. The treatment variable must be binary: 0 specified for the control group and 1 specified for the treated group. Background variables are measured before both treatment and outcome. The outcome of interest can be either binary or continuous.  

We demonstrate the utility of this package to investigate the effect of a job training program on earnings [@causaldata; @dehejia1999causal; @lalonde1986evaluating].  The treatment is whether the individual is selected for the job training program. The outcomeis earnings in 1978. Covariates are age, education, race/ethnicity, and previous earnings in 1974 and 1975.  After [preprocessing both dataset and log-transforming the earnings,](https://cran.rstudio.com/web/packages/jointVIP/vignettes/jointVIP.html), we use the `create_jointVIP()` functionto create a jointVIP object stored as *new_jointVIP*. 



```r
# first define and get pilot_df and analysis_df
# they should both be data.frame objects

treatment <- "treat"
outcome <- "log_re78"
covariates <- c("age", "educ", "black", 
                "hisp", "marr", "nodegree",
                "log_re74", "log_re75")

new_jointVIP = create_jointVIP(treatment = treatment,
                               outcome = outcome,
                               covariates = covariates,
                               pilot_df = pilot_df,
                               analysis_df = analysis_df)
```

The `plot()` function displays s a jointVIP (Figure 1). The x-axis describes treatment imbalance in SMD (computed with a denominator based on the pilot sample as in [@liao2023]). The y-axis describes outcome correlations in the pilot sample. The `summary()` function outputs the maximum absolute bias and the number of variables required for adjustment above the absolute bias tolerance, *bias_tol*. The *bias_tol* parameter can be used in the `print()` function to see which variables are above the desired tolerance. Additional tuning parameters can be specified in these functions, for details and examples, see [the additional options vignette](https://cran.rstudio.com/web/packages/jointVIP/vignettes/additional_options.html).

```r
plot(new_jointVIP)
```
![Joint variable importance plot example.\label{fig:pre}](figure1.png){ width=50% }

```r
summary(new_jointVIP, 
        smd = "OVB-based",
        use_abs = TRUE,
        bias_tol = 0.01)
# > Max absolute bias is 0.113
# > 2 variables are above the desired 0.01 absolute bias tolerance
# > 8 variables can be plotted

print(new_jointVIP,
      smd = "OVB-based",
      use_abs = TRUE,
      bias_tol = 0.01)
      
# >           bias
# > log_re75 0.113
# > log_re74 0.045

```
 
To interpret our working example, the most important variables are the previous earning variables in 1975 and 1974, `log_re75` and `log_re74` variables, respectively. Using the traditional visualization method, the Love plot, would only identify variables based on the SMD. The same information can be interpreted from the x-axis of the jointVIP. For example, the Love plot would indicate variables, `nodegree` and `hisp`, to be more important for adjustment than `log_re74`. In comparison, those variables, `nodegree` and `hisp`, show low bias using the jointVIP. 

After adjusting for variables, for example, using optimal matching [@optmatch; @stuart2011] to select pairs for analysis, a post-adjustment dataset, *post_analysis_df*, can be used to create a post adjustment object of class `post_jointVIP`. The `create_post_jointVIP()` function can be used to visualize and summarize the post adjustment results, as seen in Figure 2. The functions: `summary()`, `print()`, and `plot()` all can take in the post_jointVIP object and provide comparison between original and post adjusted jointVIPs. 


```r
post_optmatch_jointVIP <- create_post_jointVIP(new_jointVIP, 
                                               post_analysis_df = optmatch_df)
```
![Post match example showing balanced sample based on new mean differences.\label{fig:post}](figure2.png){ width=50% }

```r   
summary(post_optmatch_jointVIP)
> Max absolute bias is 0.113
> 2 variables are above the desired 0.01 absolute bias tolerance
> 8 variables can be plotted
>
> Max absolute post-bias is 0.005
> Post-measure has 0 variable(s) above the desired 0.005 absolute bias tolerance

print(post_optmatch_jointVIP)
>           bias post_bias
> log_re75 0.113     0.005
> log_re74 0.045     0.003
```

# Discussion

We have developed user-friendly software to prioritize variables for adjustment in observational studies. This package can help identify important variables related to both treatment and outcome. One limitation is that each background variable is individually evaluated for bias. Thus, conditional relationships, interactions, or higher moments of variables need to be carefully considered or preprocessed by the user.


# Acknowledgements

SDP is supported by Hellman Family Fellowship and by the National Science Foundation (grant 2142146). LDL is supported by National Science Foundation Graduate Research Fellowship (grant DGE 2146752). 

# References
