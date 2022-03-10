# library(devtools)
# devtools::install_github("ldliao/jointVIP", upgrade = F)
require(jointVIP)

library(shiny)
library(bslib)
library(shinythemes)
library(data.table)
library(ggplot2)
library(jointVIP)
library(cowplot)
library(dplyr)
# using symengine to bypass cmake error
library(symengine)

# library(shinyjs)


not_sel <- "Not Selected"

about_page <- tabPanel(title = "About",
                       titlePanel("About"),
                       fluidPage(# useShinyjs(),
                           mainPanel(
                               p(
                                   "Created with R Shiny; this work done by Lauren D. Liao and Samuel D. Pimentel.",
                                   style = "font-family: 'times'; font-si16pt"
                               ),
                               # p("To download please use", style = "font-family: 'times'; font-si16pt"),
                               # code("install.packages('jointVIP')"),
                               p(
                                   "Currently this is under active development, please email",
                                   span("ldliao [at] berkeley [dot] edu", style = "color:blue"),
                                   "if you have any questions or concerns.",
                                   style = "font-family: 'times'; font-si16pt"
                               ),
                           )),
                       br())

main_page <- tabPanel(title = "Analysis",
                      titlePanel("Analysis"),
                      sidebarLayout(
                          sidebarPanel(
                              width = 3,
                              title = "Inputs",
                              fileInput("csv_input", "Select CSV File to Import", accept = ".csv"),
                              helpText(
                                  "A sample data exists; please",
                                  "hit the run analysis button to view the joint VIP."
                              ),
                              selectInput("treatment", "Treatment", choices = c(not_sel)),
                              selectInput("outcome", "Outcome", choices = c(not_sel)),
                              selectInput(
                                  "variable_of_interest",
                                  "Variable of interest",
                                  choices = c(not_sel)
                              ),
                              selectInput("pilot_indi", "Pilot sample indicator", choices = c(not_sel)),

                              # br(),
                              actionButton("run_button", "Run Analysis", icon = icon("play"))
                          ),
                          mainPanel(tabsetPanel(
                              tabPanel(title = "joint VIP",
                                       fluidRow(
                                           align = "center",
                                           splitLayout(
                                               cellWidths = c("60%", "40%"),
                                               plotOutput("plot_1", height = "500px"),
                                               plotOutput("plot_2", height = "500px")
                                           )
                                       )),
                              tabPanel(
                                  title = "Statistics",
                                  fluidRow(
                                      column(width = 4, strong(textOutput("treatment_title"))),
                                      column(width = 4, strong(textOutput("outcome_title"))),
                                      column(width = 4, strong(textOutput("pilot_indi_title")))
                                  ),
                                  fluidRow(
                                      column(width = 4, tableOutput("treatment_summary_table")),
                                      column(width = 4, tableOutput("outcome_summary_table")),
                                      column(width = 4, tableOutput("pilot_indi_summary_table"))
                                  ),
                                  fluidRow(column(
                                      width = 12, tableOutput("combined_summary_table")
                                  ))
                              ),
                              tabPanel(
                                  title = "Advanced Options",
                                  fluidRow(
                                      column(
                                          4,
                                          "Note:",
                                          helpText(
                                              "Advanced options supports other forms of",
                                              "the joint VIP,",
                                              "please see documentation for more details.",
                                              "After updating please press the 'Run Analysis' botton and",
                                              "return to joint VIP for updated plot."
                                          )
                                      ),
                                      column(4,
                                             selectInput("use_denom", "Measure for standardized difference denominator calculation", choices = c(not_sel))
                                      ),
                                      column(
                                          4,
                                          radioButtons("use_abs",
                                                       "View plot with absolute measures?",
                                                       choices = list("Absolute standardized mean differences and correlation" = 'yes',
                                                                      "raw standardized mean differences and correlation" = 'no'),
                                                       selected = 'yes')
                                      )
                                  ),
                                  fluidRow(column(
                                      4,
                                      fileInput("post_input",
                                                "Select post-matched or post-weighted CSV File to Import",
                                                accept = ".csv"),
                                      radioButtons("use_post",
                                                   "View plot with pre-post both plotted?",
                                                   choices = list("Pre-post plot" = 'yes',
                                                                  "Original plot" = 'no'),
                                                   selected = 'no')
                                  ),
                                  column(
                                      4,
                                      radioButtons("run_boot",
                                                   "Use Bootstrap analysis?",
                                                   choices = list("Bootstrap plot" = 'yes',
                                                                  "Original plot" = 'no'),
                                                   selected = 'no'),
                                      helpText(
                                          "It will take a few minutes to run bootstrap,",
                                          "only standard or pilot choices are valid for standardizing mean difference."
                                      )
                                  ))
                              ),
                              tabPanel(title = "Plot Options",
                                       fluidRow(
                                           column(
                                               4,
                                               "Purpose:",
                                               helpText(
                                                   "To make the joint variable importance as desired,",
                                                   "please see documentation for more details.",
                                                   "After updating please press 'Run Analysis'",
                                                   "button.",
                                                   "Return to joint VIP for updated plot."
                                               )
                                           ),
                                           column(
                                               4,
                                               # plot_title
                                               textInput("plot_title",
                                                         "Plot title",
                                                         value = "Enter title..."),
                                               textInput("plot_subtitle",
                                                         "Plot subtitle",
                                                         value = "Enter subtitle...")
                                           ),
                                           column(
                                               4,
                                               # label_cutoff_control_cor
                                               sliderInput(
                                                   "max_overlap",
                                                   "maximum overlap labels",
                                                   min = 0,
                                                   max = 30,
                                                   step = 1,
                                                   value = 10
                                               ),
                                               sliderInput(
                                                   "point_text_size",
                                                   "text label size",
                                                   min = 2,
                                                   max = 10,
                                                   step = 1,
                                                   value = 4
                                               ),
                                               sliderInput(
                                                   "label_cutoff_control_cor",
                                                   "labeling points above outcome correlation",
                                                   min = 0,
                                                   max = 1,
                                                   step = 0.01,
                                                   value = 0.01
                                               ),
                                               sliderInput(
                                                   "label_cutoff_std_diff",
                                                   "labeling points above standardized mean difference",
                                                   min = 0,
                                                   max = 1,
                                                   step = 0.01,
                                                   value = 0.01
                                               ),
                                               # label_cutoff_bias
                                               sliderInput(
                                                   "label_cutoff_std_diff",
                                                   "labeling points above bias",
                                                   min = 0,
                                                   max = 1,
                                                   step = 0.005,
                                                   value = 0
                                               )

                                           )


                                       ))
                          ))
                      ))


draw_plot_1 <-
    function(data_input,
             treatment,
             outcome,
             pilot_indi,
             use_denom,
             use_abs,
             post_input,
             run_boot,
             label_cutoff_std_diff,
             label_cutoff_control_cor,
             label_cutoff_bias,
             use_post,
             max_overlap,
             point_text_size) {
        data_input = as.data.frame(data_input)
        use_abs = ifelse(use_abs == 'yes', yes = T, no =  F)
        run_boot = ifelse(run_boot == 'yes', yes = T, no =  F)
        use_post = ifelse(use_post == 'yes', yes = T, no =  F)
        # print(use_post)
        if (!use_post){
            post_input = NULL
        }
        if (pilot_indi == not_sel) {
            set.seed(123485)
            pilot_prop = 0.2
            pilot_sample_num = sample(which(data_input %>% pull(treatment) == 0),
                                      length(which(
                                          data_input %>% pull(treatment) == 0
                                      )) *
                                          pilot_prop)
            pilot_df = data_input[pilot_sample_num, ]
            analysis_df = data_input[-pilot_sample_num, ]
        } else {
            pilot_df = data_input[which(data_input %>% pull(pilot_indi) == 'pilot'), ]
            analysis_df = data_input[which(data_input %>% pull(pilot_indi) == 'analysis'), ]
        }

        if (sum(sapply(data_input, is.factor)) + sum(sapply(data_input, is.character)) > 0) {
            omit_vars = names(data_input)[which(sapply(data_input, is.factor) |
                                                    sapply(data_input, is.character))]
        } else {
            omit_vars = NULL
        }


        covariates = names(data_input)[!names(data_input) %in%
                                           c(treatment, outcome, pilot_indi, omit_vars)]

        if (!is.null(post_input)) {
            post_df = as.data.frame(post_input)
        } else {
            post_df = post_input
        }

        res_VIP <<- plot_jointVIP(
            pilot_df = pilot_df,
            analysis_df = analysis_df,
            treatment = treatment,
            covariates = covariates,
            outcome = outcome,
            use_denom = use_denom,
            use_abs = use_abs,
            post_analysis_df = post_df,
            label_cutoff_std_diff = label_cutoff_std_diff,
            label_cutoff_control_cor = label_cutoff_control_cor,
            label_cutoff_bias = label_cutoff_bias,
            max_overlap = max_overlap,
            point_text_size = point_text_size
        )


        if (run_boot) {
            if (!use_denom %in% c('standard', 'pilot')) {
                use_denom = 'standard'
                res_VIP <<- plot_jointVIP(
                    pilot_df = pilot_df,
                    analysis_df = analysis_df,
                    treatment = treatment,
                    covariates = covariates,
                    outcome = outcome,
                    use_denom = use_denom,
                    use_abs = use_abs,
                    post_analysis_df = NULL
                )

            }
            boot_vip = jointVIP:::calc_bootstrap(
                B = 1e3,
                pilot_df = pilot_df,
                analysis_df = analysis_df,
                covariates = covariates,
                treatment =  treatment,
                outcome = outcome,
                use_abs = use_abs,
                use_denom = use_denom
            )

            jointVIP:::plot_bootstrap(
                boot_vip$og_measures,
                boot_vip$boot_sd,
                use_denom = use_denom,
                joint_vip = res_VIP$VIP,
                use_abs = use_abs
            )
        } else {
            res_VIP$VIP
        }
    }

get_fact <-
    function(data_input,
             treatment,
             outcome,
             variable_of_interest) {
        df = data_input
        extract_underscore_names = names(df)[grepl('_', names(df), fixed = TRUE)]
        split_list = strsplit(extract_underscore_names, split = "_")
        first_values = c()
        for (i in 1:length(extract_underscore_names)) {
            first_values = c(first_values, split_list[[i]][1])
        }
        fac_start_strings <-
            unique(first_values[duplicated(first_values)])


        facts = c()
        for (i in 1:length(fac_start_strings)) {
            fac_names = unlist(gregexpr(paste(fac_start_strings[i], '_', sep = ""), names(df))) == 1
            facts = c(facts, list(names(df)[fac_names]))
        }

        in_fact = FALSE
        for (i in 1:length(facts)) {
            if (variable_of_interest %in% facts[[i]]) {
                var_to_plot <- strsplit(variable_of_interest, split = '_')[[1]][1]
                which_fact <- i
                in_fact = TRUE
            }
        }
        if (!in_fact) {
            return(NULL)
        }

        if (!is.null(var_to_plot)) {
            df = as.data.frame(df)
            new_df = df[, c(treatment, outcome)]
            id_cols = facts[[which_fact]]
            fact_gathered = names(df[, id_cols])[max.col(df[, id_cols])]
            fact_gathered = gsub("^.*?_", "", fact_gathered)
            new_df = data.frame(new_df, fact_gathered, stringsAsFactors = T)
            names(new_df)[ncol(new_df)] = fac_start_strings[which_fact]
            return(new_df[ncol(new_df)])
        } else {
            return(NULL)
        }
    }




draw_plot_2 <-
    function(data_input,
             treatment,
             outcome,
             variable_of_interest = "Not Selected") {
        if (variable_of_interest == not_sel) {
            # comparison with prognostic and propensity scores
            pp_plot = plot_grid(res_VIP$prognostic_comparison,
                                res_VIP$propensity_comparison,
                                ncol = 1)
            pp_plot
        } else {
            new_fact = get_fact(data_input, treatment, outcome, variable_of_interest)
            if (!is.null(new_fact)) {
                data = data.frame(data_input, new_fact)
                variable_of_interest = names(new_fact)
            } else {
                data = data_input
            }

            plot_treatment <- as.factor(data %>% pull(treatment))
            if (length(levels(as.factor(data %>% pull(outcome)))) == 2) {
                plot_outcome  = as.factor(data %>% pull(outcome))
                if (length(levels(as.factor(
                    data %>% pull(variable_of_interest)
                ))) == 2  | !is.null(new_fact)) {
                    plot_x  = as.factor(data %>% pull(variable_of_interest))
                    plot_df = data.frame(plot_x, plot_outcome, plot_treatment)
                    outcome_plot = ggplot(plot_df,
                                          aes(x = plot_x,
                                              fill = plot_outcome)) +
                        geom_bar(position = "dodge", alpha = 0.5) +
                        theme_minimal() +
                        ggtitle('Relation against outcome') +
                        theme(legend.position = "none") +
                        xlab(variable_of_interest) +
                        scale_fill_discrete(name = outcome)
                    smd_plot = ggplot(plot_df,
                                      aes(x = plot_x,
                                          fill = plot_treatment)) +
                        geom_bar(position = "dodge", alpha = 0.5) +
                        theme_minimal() +
                        ggtitle('Relation for each treatment group') +
                        theme(legend.position = "none") +
                        xlab(variable_of_interest) +
                        scale_fill_discrete(name = treatment)
                } else {
                    plot_x = data %>% pull(variable_of_interest)
                    plot_df = data.frame(plot_x, plot_outcome, plot_treatment)
                    outcome_plot = ggplot(plot_df,
                                          aes(x = plot_x,
                                              fill = plot_outcome)) +
                        geom_boxplot(alpha = 0.5) + theme_minimal() +
                        xlab(variable_of_interest) +
                        labs(fill = outcome) +
                        ggtitle('Relation against outcome') +
                        theme(
                            axis.title.y = element_blank(),
                            axis.text.y = element_blank(),
                            axis.ticks.y = element_blank()
                        ) +
                        scale_fill_discrete(name = outcome)
                    smd_plot = ggplot(plot_df,
                                      aes(x = plot_x,
                                          fill = plot_treatment)) +
                        geom_histogram(alpha = 0.5) + theme_minimal() +
                        xlab(variable_of_interest) +
                        labs(fill = treatment) +
                        ggtitle('Relation for each treatment group') +
                        scale_fill_discrete(name = treatment)
                }
            } else {
                plot_outcome  = data %>% pull(outcome)
                if (length(levels(as.factor(
                    data %>% pull(variable_of_interest)
                ))) == 2 | !is.null(new_fact)) {
                    plot_x = as.factor(data %>% pull(variable_of_interest))
                    plot_df = data.frame(plot_x, plot_outcome, plot_treatment)
                    outcome_plot <-
                        ggplot(plot_df, aes(x = plot_outcome, fill = plot_x)) +
                        geom_boxplot(alpha = 0.5) + theme_minimal() +
                        xlab(variable_of_interest) +
                        labs(fill = variable_of_interest) +
                        ggtitle('Relation against outcome') +
                        scale_fill_discrete(name = outcome)
                    smd_plot = ggplot(plot_df,
                                      aes(x = plot_x,
                                          fill = plot_treatment)) +
                        geom_bar(position = "dodge", alpha = 0.5) +
                        theme_minimal() +
                        ggtitle('Relation against treatment groups') +
                        theme(legend.position = "none") +
                        xlab(variable_of_interest) +
                        scale_fill_discrete(name = treatment)
                } else {
                    plot_x = data %>% pull(variable_of_interest)
                    plot_df = data.frame(plot_x, plot_outcome, plot_treatment)
                    outcome_plot = ggplot(plot_df, aes(y = plot_outcome, x =
                                                           plot_x)) +
                        geom_point() +
                        theme_minimal() +
                        ylab(outcome) +
                        xlab(variable_of_interest) +
                        ggtitle('Relation against outcome') +
                        scale_fill_discrete(name = outcome)
                    smd_plot = ggplot(plot_df,
                                      aes(x = plot_x,
                                          fill = plot_treatment)) +
                        geom_histogram(alpha = 0.5) + theme_minimal() +
                        xlab(variable_of_interest) +
                        labs(fill = treatment) +
                        ggtitle('Relation against treatment groups') +
                        scale_fill_discrete(name = treatment)
                }
            }
            plot_grid(
                outcome_plot + theme(axis.text.x = element_text(
                    angle = 90, hjust = 1
                )) + theme_minimal() +
                    theme(
                        axis.text.x = element_text(size = 10),
                        axis.text.y = element_text(size = 10),
                        axis.title.x = element_text(size = 12),
                        axis.title.y = element_text(size = 12),
                        plot.title = element_text(size = 14)
                    ) +
                    theme(
                        panel.background = element_rect(fill = "white"),
                        axis.text.x = element_text(color = "black"),
                        axis.text.y = element_text(color = "black"),
                        panel.border = element_rect(fill = NA, color = "black"),
                        plot.background = element_blank(),
                        legend.background = element_blank(),
                        legend.key = element_blank()
                    ),
                smd_plot + theme(axis.text.x = element_text(
                    angle = 90, hjust = 1
                )) + theme_minimal() +
                    theme(
                        axis.text.x = element_text(size = 10),
                        axis.text.y = element_text(size = 10),
                        axis.title.x = element_text(size = 12),
                        axis.title.y = element_text(size = 12),
                        plot.title = element_text(size = 14)
                    ) +
                    theme(
                        panel.background = element_rect(fill = "white"),
                        axis.text.x = element_text(color = "black"),
                        axis.text.y = element_text(color = "black"),
                        panel.border = element_rect(fill = NA, color = "black"),
                        plot.background = element_blank(),
                        legend.background = element_blank(),
                        legend.key = element_blank()
                    ),
                ncol = 1
            )
        }
    }

create_table <- function(data_input, var) {
    is.binary <- function(v) {
        x <- unique(v)
        length(x) - sum(is.na(x)) == 2L
    }

    if (var != not_sel) {
        if (is.binary(data_input[complete.cases(data_input),
                                 get(var)])) {
            freq_tbl <- data_input[, .N, by = get(var)]
            freq_tbl <- setnames(freq_tbl, c("level", "count"))
            freq_tbl$percentage <-
                as.vector(round(prop.table(table(
                    data_input[, get(var)]
                )), 3))
            return(freq_tbl)
        } else {
            col <- data_input[, get(var)]
            if (length(col) > 5000)
                col_norm <- sample(col, 5000)
            else
                col_norm <- col
            # norm_test <- shapiro.test(col_norm)
            statistic <-
                c(
                    "mean",
                    "median",
                    "5th percentile",
                    "95th percentile"
                    #"Shapiro statistic",
                    #"Shapiro p-value"
                )
            value <- c(
                round(mean(col), 2),
                round(median(col), 2),
                round(quantile(col, 0.05), 2),
                round(quantile(col, 0.95), 2)#,
                #norm_test$statistic,
                #norm_test$p.value
            )
            return(data.table(statistic, value))
        }
    }
}

create_pilot_table <-
    function(data_input,
             pilot_indi,
             treatment,
             outcome) {
        is.binary <- function(v) {
            x <- unique(v)
            length(x) - sum(is.na(x)) == 2L
        }

        if (pilot_indi != not_sel) {
            if (is.binary(data_input[complete.cases(data_input),
                                     get(pilot_indi)])) {
                freq_tbl <- data_input[, .N, by = get(pilot_indi)]
                freq_tbl <- setnames(freq_tbl, c("level", "count"))
                freq_tbl$percentage <-
                    as.vector(prop.table(table(data_input[, get(pilot_indi)])))
                return(freq_tbl)
            }
        } else if (treatment != not_sel & outcome != not_sel) {
            set.seed(123485)
            pilot_prop = 0.2
            pilot_sample_num = sample(which(data_input %>% pull(treatment) == 0),
                                      length(which(
                                          data_input %>% pull(treatment) == 0
                                      )) *
                                          pilot_prop)
            random_pilot = rep(0, nrow(data_input))
            random_pilot[pilot_sample_num] = 1
            freq_tbl <-
                as.data.frame(rbind(c(
                    "pilot", sum(random_pilot), round(mean(random_pilot), 2)
                ),
                c(
                    "analysis", sum(random_pilot == 0), round(1 - mean(random_pilot), 2)
                )))
            freq_tbl <-
                setnames(freq_tbl, c("sample", "count", "percentage"))
            return(freq_tbl)
        }
    }

create_combined_table <- function(treatment, outcome, use_abs, use_post) {
    use_abs = ifelse(use_abs == 'yes', yes = T, no =  F)
    use_post = ifelse(use_post == 'yes', yes = T, no =  F)

    if (treatment != not_sel & outcome != not_sel) {
        ranked_df = res_VIP$measures[order(abs(res_VIP$VIP$data %>% pull('bias_std_diff_pilot')),
                                           decreasing = T),]
        if(!use_post){
            rank_vip = data.frame(
                rownames(ranked_df),
                abs(res_VIP$VIP$data[order(abs(res_VIP$VIP$data %>% pull('bias_std_diff_pilot')),
                                       decreasing = T),] %>% pull('bias_std_diff_pilot')),
                ranked_df$tol_suggest
            )
            colnames(rank_vip) <-
                    c('absolute variable importance (hi-lo)',
                      'bias',
                      'tolerance suggestion')

            rank_vip = rank_vip[abs(round(rank_vip %>% pull('bias'), 2)) > 0,]

        } else {
            rank_vip = data.frame(
                rownames(ranked_df),
                abs(res_VIP$measures[order(abs(res_VIP$measures %>% pull('pre_bias')),
                                       decreasing = T),] %>% pull('pre_bias')),
                abs(res_VIP$measures[order(abs(res_VIP$measures %>% pull('pre_bias')),
                                       decreasing = T),] %>% pull('bias_std_diff_post_pilot'))
            )

            colnames(rank_vip) <-
                c('Absolute variable importance (hi-lo)',
                  'pre-bias',
                  'post-bias')

            rank_vip = rank_vip[abs(round(rank_vip %>% pull('pre-bias'), 2)) > 0,]

        }
        return(head(rank_vip, 10))
    }
}

ui <- navbarPage(# shinythemes::themeSelector(),
    title = "Run Joint [treatment-outcome] Variable Importance Plot",
    theme = bs_theme(bootswatch = "united"),
    main_page,
    about_page)

server <- function(input, output, session) {
    options(shiny.maxRequestSize = 10 * 1024 ^ 2)

    data_input <- reactive({
        if (is.null(input$csv_input)) {
            ##### using matching dataset online as example #####
            brfss <-
                read.csv("http://static.lib.virginia.edu/statlab/materials/data/brfss_2015_sample.csv")
            # COPD: Ever told you have chronic obstructive pulmonary disease (COPD)?
            # SMOKE: Adults who are current smokers (0 = no, 1 = yes)
            # RACE: Race group
            # AGE: age group
            # SEX: gender
            # WTLBS: weight in lbs
            # AVEDRNK2: During the past 30 days, when you drank, how many drinks did you drink on average?
            #
            # We wish to investigate the effect of smoking on COPD. Does smoking increase the chance of contracting COPD?

            # data cleaning
            brfss$COPD = ifelse(factor(brfss$COPD) == 'No', 0, 1) # reference is no
            brfss$RACE = factor(brfss$RACE)
            brfss$RACE <-
                relevel(brfss$RACE, ref = 'White') # reference is majority in data
            brfss$AGE = factor(brfss$AGE)
            brfss$SEX = ifelse(factor(brfss$SEX) == 'Female', 0, 1) # reference is majority in data
            brfss = fastDummies::dummy_cols(brfss)
            df = (brfss[, !(names(brfss) %in% c("RACE", "AGE"))])

            ## cleaned data
            df = (brfss[, !(names(brfss) %in% c("RACE", "AGE"))])
            names(df) <- c("COPD", "smoke", "sex", "weight",
                           "average_drinks", "race_white", "race_black",
                           "race_hispanic", "race_other", "age_18to24",
                           "age_25to34", "age_35to44", "age_45to54",
                           "age_55to64", "age_over65")
            class(df) <- c("data.table", "data.frame")
            df
        } else {
            if (all(fread(input$csv_input$datapath)[, 1] == 1:nrow(fread(input$csv_input$datapath)))) {
                fread(input$csv_input$datapath)[,-c(1)]
            }
            else {
                fread(input$csv_input$datapath)
            }
        }
        # req(input$csv_input)
    })


    post_input <- reactive({
        inFile <- input$post_input
        if (is.null(inFile)) {
            return(NULL)
        } # same here
        # fread(input$post_input$datapath)
        else if (all(fread(input$post_input$datapath)[, 1] ==
                     1:nrow(fread(input$post_input$datapath)))) {
            fread(input$post_input$datapath)[,-c(1)]
        }
        else {
            fread(input$post_input$datapath)
        }
    })


    observeEvent(data_input(), {
        choices <- c(not_sel, names(data_input()))
        if (is.null(input$csv_input)){
            updateSelectInput(inputId = "treatment", choices = choices,
                              selected = 'smoke')
            updateSelectInput(inputId = "outcome", choices = choices,
                              selected = 'COPD')
        } else {
            updateSelectInput(inputId = "treatment", choices = choices)
            updateSelectInput(inputId = "outcome", choices = choices)
        }
        updateSelectInput(inputId = "pilot_indi", choices = choices)
        updateSelectInput(inputId = "use_denom",
                          choices = c("both", "standard", "pilot"))
        # updateSelectInput(inputId = "use_abs",
        #                   choices = c('yes', 'no'))
        updateSelectInput(inputId = "variable_of_interest",
                          choices = choices)
        # updateSelectInput(inputId = "run_boot",
        #                   choices = c('no', 'yes'))
    })

    observeEvent(post_input(), {
        updateSelectInput(inputId = "use_denom",
                          choices = c("standard", "pilot"))
    })

    treatment <<- eventReactive(input$run_button, input$treatment)
    outcome <<- eventReactive(input$run_button, input$outcome)
    pilot_indi <<- eventReactive(input$run_button, input$pilot_indi)
    use_denom <<- eventReactive(input$run_button, input$use_denom)
    use_abs <<- eventReactive(input$run_button, input$use_abs)
    use_post <<- eventReactive(input$run_button, input$use_post)
    max_overlap <<- eventReactive(input$run_button, input$max_overlap)
    point_text_size <<- eventReactive(input$run_button, input$point_text_size)

    variable_of_interest <<- eventReactive(input$run_button,
                                           input$variable_of_interest)
    run_boot <<- eventReactive(input$run_button,
                               input$run_boot)
    label_cutoff_std_diff <<- eventReactive(input$run_button,
                                            input$label_cutoff_std_diff)
    label_cutoff_control_cor <<- eventReactive(input$run_button,
                                               input$label_cutoff_control_cor)
    label_cutoff_bias <<- eventReactive(input$run_button,
                                        input$label_cutoff_bias)
    plot_title <<- eventReactive(input$run_button,
                                 input$plot_title)
    plot_subtitle <<- eventReactive(input$run_button,
                                    input$plot_subtitle)

    treatment_check <- reactive({
        input$run_button
        validate(need(treatment != not_sel, "Treatment must be selected."))
        validate(need(
            all(input$treatment %in% c(0, 1)),
            "Treatment must be binary and complete."
        ))
    })

    outcome_check <- reactive({
        input$run_button
        validate(need(outcome != not_sel, "Outcome must be selected."))
        validate(need(all(!is.na(
            input$outcome
        )), "Outcome must be complete."))
    })

    # plot
    plot_1 <- eventReactive(input$run_button, {
        p = draw_plot_1(
            data_input = data_input(),
            treatment = treatment(),
            outcome = outcome(),
            pilot_indi = pilot_indi(),
            use_denom = use_denom(),
            use_abs = use_abs(),
            post_input = post_input(),
            run_boot = run_boot(),
            label_cutoff_std_diff = label_cutoff_std_diff(),
            label_cutoff_control_cor = label_cutoff_control_cor(),
            label_cutoff_bias = label_cutoff_bias(),
            use_post = use_post(),
            max_overlap = max_overlap(),
            point_text_size = point_text_size()
        )
        if (plot_title() == 'Enter title...' &
            plot_subtitle() == 'Enter subtitle...') {
            p
        }
        else if (plot_title() != 'Enter title...' &
                 plot_subtitle() == 'Enter subtitle...') {
            p <- p + labs(title = plot_title())
            p
        } else if (plot_title() == 'Enter title...' &
                   plot_subtitle() != 'Enter subtitle...') {
            p <- p + labs(subtitle = plot_subtitle())
            p
        } else{
            p <- p + labs(title = plot_title(),
                          subtitle = plot_subtitle())
            p
        }

    })
    plot_2 <- eventReactive(input$run_button, {
        draw_plot_2(data_input(),
                    treatment(),
                    outcome(),
                    variable_of_interest())
    })

    output$plot_1 <- renderPlot({
        input$run_button
        withProgress({
            for (i in seq_len(15)) {
                return(plot_1())
            }
        }, message = "Plotting in progress...")

    })
    output$plot_2 <- renderPlot(plot_2())

    # 1-d summary tables

    output$treatment_title <-
        renderText(paste("Treatment:", treatment()))

    treatment_summary_table <- eventReactive(input$run_button, {
        create_table(data_input(), treatment())
    })

    output$treatment_summary_table <-
        renderTable(treatment_summary_table(),
                    colnames = T)

    output$outcome_title <- renderText(paste("Outcome:", outcome()))

    outcome_summary_table <- eventReactive(input$run_button, {
        create_table(data_input(), outcome())
    })

    output$outcome_summary_table <-
        renderTable(outcome_summary_table(),
                    colnames = T)

    output$pilot_indi_title <-
        renderText(paste("Pilot sample indicator:",
                         pilot_indi()))

    pilot_indi_summary_table <- eventReactive(input$run_button, {
        create_pilot_table(data_input(), pilot_indi(), treatment(), outcome())
    })

    output$pilot_indi_summary_table <-
        renderTable(pilot_indi_summary_table(),
                    colnames = T)

    # multi-d summary table

    combined_summary_table <- eventReactive(input$run_button, {
        create_combined_table(treatment(), outcome(), use_abs(), use_post())
    })

    output$combined_summary_table <-
        renderTable(combined_summary_table())

}

# library(shiny.semantic)
shinyApp(ui = ui, server = server)

# library(rsconnect)
# deployApp(appName="jointVIP")
# runApp("/Users/ldliao/Research/Projects/jointVIP/man/shiny/jointVIP_example/app.R", display.mode = "showcase")
