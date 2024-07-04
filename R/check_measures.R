#' Check measures
#' Check to see if there is any missing values or variables
#' without any variation or identical rows (only unique rows will be used)
#'
#' @param measures measures needed for jointVIP
#' @return measures needed for jointVIP
check_measures = function(measures){

  if(nrow(measures[duplicated(measures) |
                   duplicated(measures, fromLast = TRUE),]) > 0){
    warning(paste0(c("Variables",
                     row.names(measures[duplicated(measures) |
                                          duplicated(measures, fromLast = TRUE),]),
                     "measures are duplicated (all multiples are shown).",
                     "\nOnly unique variables will be used."
    )," "))
  }

  clean_measures <- measures[!duplicated(measures),]

  if(any(rowSums(is.na(clean_measures))>0)){
    warning(paste0(c("Variable(s)",
                     rownames(clean_measures)[rowSums(is.na(clean_measures))>0],
                     "measures contain missing values.",
                     "\nThey are dropped when plotting."
    )," "))
  }

  clean_measures <- measures[rownames(clean_measures)[rowSums(!is.na(clean_measures))>0],]

  if(any(clean_measures$std_md == 0)){
    warning(paste0(c("The standardized mean difference for variable(s)",
                     rownames(clean_measures)[clean_measures$std_md == 0],
                     "are 0.",
                     "\nTheir biases are automatically 0."
    )," "))
  }

  clean_measures <- clean_measures[complete.cases(clean_measures),]
  if (all(dim(clean_measures) == c(0, 0))) {
    stop("measures is empty, please check for errors that may have occurred")
  }
  clean_measures
}
