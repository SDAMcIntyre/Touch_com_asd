library(dplyr)
library(boot)

# label: string, the particular class we are currently investigating 
# true_class: string vector, the true class of the items
# predicted_class: string vector, the responses indicating how the items were labelled

present <- function(label, true_class) {
  sum(true_class == label)
}

selected <- function(label, predicted_class) {
  sum(predicted_class == label)
}

hits <- function(label, true_class, predicted_class) {
  sum(true_class == predicted_class & true_class == label)
}

misses <- function(label, true_class, predicted_class) {
  present(label, true_class) - hits(label, true_class, predicted_class)
}

false_alarms <- function(label, true_class, predicted_class) {
  selected(label, predicted_class) - hits(label, true_class, predicted_class)
}

correct_rejections <- function(label, true_class, predicted_class) {
  length(true_class) - present(label, true_class) - false_alarms(label, true_class, predicted_class)
}

recall <- function(label, true_class, predicted_class) {
  # p correct when this label is the true class
  hits(label, true_class, predicted_class) / present(label, true_class)
}

precision <- function(label, true_class, predicted_class) {
  # p correct when predicting this label as the class
  precision <- hits(label, true_class, predicted_class) / selected(label, predicted_class) 
  if ( is.na(precision) ) {
    return(0)
  } else {
    return(precision)
  }
}

f1  <- function(label, true_class, predicted_class) {
  f1 <- 2 * (
    ( precision(label, true_class, predicted_class) * recall(label, true_class, predicted_class) ) / 
      (precision(label, true_class, predicted_class) + recall(label, true_class, predicted_class) )
    )
  if ( is.na(f1) ) {
    return(0) 
  } else {
      return(f1)
  }
}

f1_chance <- function(label, true_class, predicted_class) {
  # predicted f1 score if just guessing
  2 * ( 
    ( present(label, true_class) / length(true_class) ) / 
      ( (present(label, true_class) / length(true_class)) + 1 ) 
    )
}

# bootstrapping
# https://bookdown.org/compfinezbook/introcompfinr/The-Nonparametric-Bootstrap.html
# https://search.r-project.org/CRAN/refmans/boot/html/boot.html

# f1 "statistic" function to pass to boot
f1_for_boot <- function(xdata, idx, label) {
  f1(label, xdata[["true_class"]][idx], xdata[["predicted_class"]][idx])
}


f1_bootstrap_ci <- function(df, true_class, predicted_class, label, R) {
  xdata <- tibble(
    true_class = df[[true_class]],
    predicted_class = df[[predicted_class]]
  ) 
  
  # do bootstrapping on f1
  f1_boot <- boot(data = xdata, statistic = f1_for_boot, R = R, parallel = "multicore", label = label)
  # get CIs
  f1_boot_ci <- boot.ci(f1_boot, conf = 0.95, type="perc")
  ci_lower <- f1_boot_ci$percent[4]
  ci_upper <- f1_boot_ci$percent[5]
  
  # output
  tibble(
    Label = label,
    F1 = f1_boot$t0,
    # set CI to 1,1 if error is generated from boot.ci() call: 
    #   "All values of t are equal to  1 \n Cannot calculate confidence intervals"
    F1_bCI_lower = if_else(is.null(ci_lower), 1, ci_lower),
    F1_bCI_upper = if_else(is.null(ci_upper), 1, ci_upper)
  )
}

calculate_metrics <- function(df, labels, true_class, predicted_class, R, ...) {
  out <- tibble()
  for (label in labels) {
    
    f1_bci <- df %>% 
      group_by(...) %>% 
      group_modify( ~ f1_bootstrap_ci(.x, true_class, predicted_class, label = label, R = R))
    
    other_metrics <- df %>%
      group_by(...) %>%
      summarise(
        Label = label,
        F1_chance = f1_chance(label, get(!!true_class), get(!!predicted_class)),
        Present = present(label, get(!!true_class)),
        Selected = selected(label, get(!!predicted_class)),
        Hits = hits(label, get(!!true_class), get(!!predicted_class)),
        Misses = misses(label, get(!!true_class), get(!!predicted_class)),
        `False Alarms` = false_alarms(label, get(!!true_class), get(!!predicted_class)),
        `Correct Rejections` = correct_rejections(label, get(!!true_class), get(!!predicted_class)),
        Recall = recall(label, get(!!true_class), get(!!predicted_class)),
        Precision = precision(label, get(!!true_class), get(!!predicted_class))
      )
    
    combined <- full_join(f1_bci, other_metrics)
    
    out <- rbind(
      out, 
      combined
    )
  }
  out %>% select(..., everything()) %>% arrange(...)
}
