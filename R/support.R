#' support function for one-hot encoding
#'
#' @param df data.frame object for performing one-hot encoding
#' @return data.frame object with factor variables one-hot encoded for each level
one_hot <-
  function(df) {
    char_vars <- unlist(lapply(df[, , drop = FALSE], is.character))

    if(sum(char_vars) > 0){
      # cat("character variables are converted into factor variables\n")
      df[sapply(df, is.character)] <-
        lapply(df[sapply(df, is.character)],
               as.factor)
    }
    fac_vars <- unlist(lapply(df[, , drop = FALSE], is.factor))

    if (sum(fac_vars) > 0) {
      # cat("all factor variables are one-hot encoded\n")
      lvls <- lapply(df[, fac_vars, drop = FALSE], nlevels)

      # binary
      for (bin_var in names(lvls)[lvls == 2]) {
        col_name_first_fac_lvl <-
          paste0(c(bin_var, levels(df[[bin_var]])[1]), collapse = "_")
        df[[col_name_first_fac_lvl]] <-
          ifelse(df[[bin_var]] == levels(df[[bin_var]])[1],
                 1, 0)
      }

      # categorical with multiple levels
      for (multi_var in names(lvls)[lvls > 2]) {
        col_names_lvls <- paste(multi_var, levels(df[[multi_var]]), sep = "_")
        one_hot_mtx <- stats::model.matrix( ~ 0 + df[[multi_var]], df)
        colnames(one_hot_mtx) <- col_names_lvls
        df <- cbind(df, one_hot_mtx)
      }
    }

    return(Filter(function(x)
      ! is.factor(x), df))
  }

#' support function to plot bias curves
#'
#' @param p plot made with jointVIP object
#' @param ... encompasses other variables needed
#' @return a joint variable importance plot of class `ggplot` with curves
#' @import ggplot2
add_bias_curves <- function(p, ...) {
  use_abs = list(...)[['use_abs']]
  measures = list(...)[['measures']]
  bias_curve_cutoffs = list(...)[['bias_curve_cutoffs']]
  expanded_y = list(...)[['expanded_y_curvelab']]

  if (is.null(expanded_y)) {
    expanded_y = 0
  }
  if (is.null(bias_curve_cutoffs)) {
    if (use_abs) {
      bias_curve_cutoffs = c(0.005)
      bias_curve_cutoffs = c(bias_curve_cutoffs,
                             floor_dec(seq(0.011,
                                           max(
                                             abs(measures$bias)
                                           ),
                                           length.out = 4), 2))
      bias_curve_cutoffs = bias_curve_cutoffs[abs(bias_curve_cutoffs) >= 0.01 |
                                                abs(bias_curve_cutoffs) == 0.005]
    } else {
      bias_curve_cutoffs = c(0.005)

      bias_curve_cutoffs = c(bias_curve_cutoffs,
                             floor_dec(seq(0.011,
                                           max(
                                             abs(measures$bias)
                                           ),
                                           length.out = 4), 2))
      bias_curve_cutoffs = c(bias_curve_cutoffs, -1 * bias_curve_cutoffs)

      bias_curve_cutoffs = bias_curve_cutoffs[abs(bias_curve_cutoffs) >= 0.01 |
                                                abs(bias_curve_cutoffs) == 0.005]
    }
  } else if (!is.numeric(bias_curve_cutoffs)) {
    stop("`bias_curve_cutoffs` must be numeric")
  } else  if (0 %in% bias_curve_cutoffs) {
    warning("0 in the `bias_curve_cutoffs` will not be plotted")
  }

  bias_curve_cutoffs = bias_curve_cutoffs[!duplicated(bias_curve_cutoffs)]
  bias_curve_cutoffs = bias_curve_cutoffs[bias_curve_cutoffs != 0]

  bias_func = function(i) {
    i = force(i)
    f = function(x) {
      i / x
    }
    return(f)
  }

  for (b in bias_curve_cutoffs) {
    loop_input = paste(
      "geom_function(fun = bias_func(",
      b,
      "), colour = 'grey5', linetype = 'dotted', alpha = 0.5, na.rm=TRUE)",
      sep = ""
    )
    p <- p + eval(parse(text = loop_input))
  }

  if (use_abs) {
    text_bias_lab = data.frame(
      x = c(bias_curve_cutoffs / (expanded_y + ceiling_dec(max(
        abs(measures$outcome_cor)
      ), 2))),
      y = ceiling_dec(max(abs(
        measures$outcome_cor
      )), 2) - 0.002 + expanded_y,
      label = as.character(bias_curve_cutoffs)
    )
    text_bias_lab[text_bias_lab$label == 0.005, 'y'] =
      (ceiling_dec(max(abs(
        measures$outcome_cor
      )), 2) -
        min(c(max(
          bias_curve_cutoffs
        ), 0.02))) + expanded_y
    p <- p + geom_text(
      data = text_bias_lab,
      mapping = aes(
        x = as.numeric(.data$x),
        y = as.numeric(.data$y),
        label = .data$label
      ),
      color = 'grey3',
      alpha = 0.7,
      size = 3.5,
      check_overlap = FALSE
    )
  } else {
    text_bias_lab = data.frame(
      x = c(
        bias_curve_cutoffs /
          (expanded_y + ceiling_dec(max(
            abs(measures$outcome_cor)
          ), 2)),
        bias_curve_cutoffs /
          (expanded_y + ceiling_dec(max(
            abs(measures$outcome_cor)
          ), 2))
      ),
      y = c(
        rep(
          ceiling_dec(max(abs(
            measures$outcome_cor
          )), 2) - 0.002 + expanded_y,
          length(bias_curve_cutoffs)
        ),
        rep(
          -(ceiling_dec(max(
            abs(measures$outcome_cor)
          ), 2) - 0.002) - expanded_y,
          length(bias_curve_cutoffs)
        )
      ),
      label = c(bias_curve_cutoffs, -(bias_curve_cutoffs))
    )

    text_bias_lab[abs(as.numeric(text_bias_lab$label)) == 0.005,
                  'y'] =
      (text_bias_lab[abs(as.numeric(text_bias_lab$label)) == 0.005, 'y'] -
         min(c(max(
           bias_curve_cutoffs
         ), 0.1)) *
         sign(text_bias_lab[abs(as.numeric(text_bias_lab$label)) == 0.005, 'y']))

    text_bias_lab[abs(as.numeric(text_bias_lab$label)) == 0.005 &
                    text_bias_lab$x < 0,
                  'y'] =
      text_bias_lab[abs(as.numeric(text_bias_lab$label)) == 0.005 &
                      text_bias_lab$x < 0,
                    'y'] + 0.04

    p <- p + geom_text(
      data = text_bias_lab,
      mapping = aes(
        x = as.numeric(.data$x),
        y = as.numeric(.data$y),
        label = .data$label
      ),
      color = 'grey3',
      alpha = 0.7,
      size = 3,
      check_overlap = FALSE
    )
  }
  return(p)
}

#' support function to plot variable text labels
#'
#' @param p plot made with jointVIP object
#' @param ... encompasses other variables needed
#' @return a joint variable importance plot of class `ggplot` with curves
#' @importFrom ggrepel geom_text_repel
add_variable_labels <- function(p,
                                ...) {
  measures = list(...)[['measures']]
  arguments <- list(...)
  label_cut_std_md <- arguments$label_cut_std_md
  label_cut_outcome_cor <- arguments$label_cut_outcome_cor
  label_cut_bias <- arguments$label_cut_bias
  text_size <- arguments$text_size
  max.overlaps <- arguments$max.overlaps

  if (is.null(label_cut_std_md)) {
    label_cut_std_md = 0
  } else {
    if (!((is.numeric(label_cut_std_md)) &
          (label_cut_std_md > 0))) {
      stop("`label_cut_std_md` must be a positive numeric")
    }
  }
  if (is.null(label_cut_outcome_cor)) {
    label_cut_outcome_cor = 0
  } else {
    if (!(is.numeric(label_cut_outcome_cor) &
          (label_cut_outcome_cor > 0))) {
      stop("`label_cut_outcome_cor` must be a positive numeric")
    }
  }
  if (is.null(label_cut_bias)) {
    label_cut_bias = 0
  } else {
    if (!((is.numeric(label_cut_bias)) &
          (label_cut_bias > 0))) {
      stop("`label_cut_bias` must be a positive numeric")
    }
  }
  if (is.null(text_size)) {
    text_size = 3.5
  } else {
    if (!((is.numeric(text_size)) &
          (text_size > 0))) {
      stop("`text_size` must be a positive numeric")
    }
  }
  if (is.null(max.overlaps)) {
    max.overlaps = 10
  } else {
    if (!((is.numeric(max.overlaps)) &
          (max.overlaps > 0))) {
      stop("`max.overlaps` must be a positive numeric")
    }
  }
  measures$text_label <- row.names(measures)
  if (!(label_cut_std_md == 0 &
        label_cut_outcome_cor == 0 & label_cut_bias == 0)) {
    measures[!(((
      abs(measures$std_md) >= label_cut_std_md
    ) & (
      abs(measures$outcome_cor) >= label_cut_outcome_cor
    )) &
      (abs(measures$bias) >= label_cut_bias)), 'text_label'] = ""
  }

  p + geom_text_repel(
    data = measures,
    mapping = aes(label = .data$text_label),
    size = text_size,
    max.overlaps = max.overlaps
  )
}

#' support function for ceiling function with decimals
#'
#' @param num numeric
#' @param dec_place decimal place that is desired ceiling for
#' @return numeric number desired
ceiling_dec <-
  function(num, dec_place = 1) {
    round(num + 5 * 10 ^ (-dec_place - 1), dec_place)
  }


#' support function for floor function with decimals
#'
#' @param num numeric
#' @param dec_place decimal place that is desired floor for
#' @return numeric number desired
floor_dec <- function(num, dec_place = 1) {
  round(num - 5 * 10 ^ (-dec_place - 1), dec_place)
}
