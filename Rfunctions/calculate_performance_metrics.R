library(dplyr)
library(boot)
library(tidyr)
library(broom)
library(purrr)

# functions for metrics ####

# some functions here based on MLmetrics (F1 averaging functions)
# but I don't actually load this package because of a bunch of small bugs, had to rewrite a lot anyway
# https://github.com/renatopanda/MLmetrics
# devtools::install_github("renatopanda/MLmetrics") # this version has F1 averaging options


# label: string, the particular class we are currently investigating 
# true_class: string vector, the true class of the items
# predicted_class: string vector, the responses indicating how the items were labelled

confusion_matrix <- function(true_class, predicted_class) { 
  table(true_class, predicted_class)
}

confusion_df <- function(true_class, predicted_class) { 
  as_tibble(confusion_matrix(predicted_class, true_class))
}

present <- function(true_class, label) {
  sum(true_class == label)
}

selected <- function(predicted_class, label) {
  sum(predicted_class == label)
}

hits <- function(true_class, predicted_class, label) {
  sum(true_class == predicted_class & true_class == label)
}

misses <- function(true_class, predicted_class, label) {
  present(true_class, label) - hits(true_class, predicted_class, label)
}

false_alarms <- function(true_class, predicted_class, label) {
  selected(predicted_class, label) - hits(true_class, predicted_class, label)
}

correct_rejections <- function(true_class, predicted_class, label) {
  length(true_class) - present(true_class, label) - false_alarms(true_class, predicted_class, label)
}

recall <- function(true_class, predicted_class, label) {
  # p correct when this label is the true class
  hits(true_class, predicted_class, label) / present(true_class, label)
}

precision <- function(true_class, predicted_class, label) {
  # p correct when predicting this label as the class
  precision <- hits(true_class, predicted_class, label) / selected(predicted_class, label) 
  if ( is.na(precision) ) {
    return(0)
  } else {
    return(precision)
  }
}

specificity <- function(true_class, predicted_class, label) {
  correct_rejections(true_class, predicted_class, label) / 
    (length(predicted_class) - selected(predicted_class, label))
}

f1  <- function(true_class, predicted_class, label) {
  f1 <- 2 * (
    ( precision(true_class, predicted_class, label) * recall(true_class, predicted_class, label) ) / 
      (precision(true_class, predicted_class, label) + recall(true_class, predicted_class, label) )
  )
  if ( is.na(f1) ) {
    return(0) 
  } else {
    return(f1)
  }
}

f1_chance <- function(true_class, predicted_class, label) {
  # predicted f1 score if just guessing
  2 * ( 
    ( present(true_class, label) / length(true_class) ) / 
      ( (present(true_class, label) / length(true_class)) + 1 ) 
  )
}

precision_micro <- function(true_class, predicted_class, labels = NULL) {

  if (is.null(labels) == TRUE) {
      labels <- unique(c(true_class, predicted_class))
      }
  
  Hits <- c()
  False_alarms <- c()
  
  for (lbl in labels) {
    Hits <- cbind(Hits, hits(true_class, predicted_class, lbl))
    False_alarms <- cbind(False_alarms, false_alarms(true_class, predicted_class, lbl))
  }
  
  sum(Hits)/(sum(Hits) + sum(False_alarms))
}


recall_micro <- function (true_class, predicted_class, labels = NULL) {
  
  if (is.null(labels) == TRUE) {
    labels <- unique(c(true_class, predicted_class))
  }
  
  Hits <- c()
  Misses <- c()

  for (lbl in labels) {
    Hits <- c(Hits, hits(true_class, predicted_class, lbl))
    Misses <- c(Misses, misses(true_class, predicted_class, lbl))
  }
  
  sum(Hits)/(sum(Hits) + sum(Misses))
}

f1_ave_micro <- function (true_class, predicted_class, labels = NULL) {
  if (is.null(labels) == TRUE) 
    labels <- unique(c(true_class, predicted_class))
  Precision <- precision_micro(true_class, predicted_class, labels)
  Recall <- recall_micro(true_class, predicted_class, labels)
  f1_micro <- 2 * (Precision * Recall)/(Precision + Recall)
  if ( is.na(f1_micro) ) {
    return(0) 
  } else {
    return(f1_micro)
  }
}

# bootstrapping ####
# https://bookdown.org/compfinezbook/introcompfinr/The-Nonparametric-Bootstrap.html
# https://search.r-project.org/CRAN/refmans/boot/html/boot.html
# https://stackoverflow.com/questions/31813228/bootstrap-confidence-intervals-for-more-than-one-statistics-through-boot-ci-func


# bootstrap functions
f1_micro_boot <- function(df, true_class, predicted_class, labels = NULL, R) {
  if (is.null(labels)) {(labels <- unique(df[["true_class"]])); print(labels)}
  
  data_for_boot <- tibble(
    true_class = df[[true_class]],
    predicted_class = df[[predicted_class]]
  ) 
  
  # f1 micro fn to pass to boot()
  stat_fn_for_boot <- function(data_for_boot, idx, labels) {
    f1_ave_micro(data_for_boot[["true_class"]][idx], data_for_boot[["predicted_class"]][idx], labels)
  }
  
  # # do bootstrapping with above function
  boot_out <- boot(data = data_for_boot, statistic = stat_fn_for_boot, R = R, parallel = "multicore", labels = labels)
  
  # get CIS, tidy
  if (n_distinct(boot_out$t) == 1) {
    # if all values are the same (e.g. participant got 100% correct), fill CIs with NA
    tidy_cis <- boot_out %>% 
      tidy(conf.int=FALSE, conf.method="perc") %>% 
      mutate( conf.low = NA_real_, conf.high = NA_real_)
  } else {
    tidy_cis <- boot_out %>% 
      tidy(conf.int=TRUE, conf.method="perc") 
  }
  
  tidy_cis %>% rename(F1_micro = statistic)
  
}

metrics_boot <- function(df, true_class, predicted_class, labels = NULL, R) {
  if (is.null(labels)) {(labels <- unique(df[["true_class"]])); print(labels)}
  
  # so we can label the output later
  metric_list <- c()
  for (lbl in labels){
    metric_list <- c(metric_list, paste(lbl, c("Recall", "Precision", "Specificity", "F1", "F1chance"), sep = "_"))
  }
  
  # put data in the format that boot() wants
  data_for_boot <- tibble(
    true_class = df[[true_class]],
    predicted_class = df[[predicted_class]]
  ) 
  
  # metrics fn to pass to boot()
  stat_fn_for_boot <- function(data_for_boot, idx, labels) {
    out <- c()
    # by label
    for (lbl in labels) {
      # Recall: Hits / positive cases [Hits + Misses]; prop. correct, positive predictive value (PPV), Sensitivity
      Recall <- recall(data_for_boot[["true_class"]][idx], data_for_boot[["predicted_class"]][idx], lbl) 
      # Precision: Hits / positive predictions [Hits + False Alarms]
      Precision <- precision(data_for_boot[["true_class"]][idx], data_for_boot[["predicted_class"]][idx], lbl)
      # Specificity: Correct Rejections / negative cases [CR + misses]
      Specificity <- specificity(data_for_boot[["true_class"]][idx], data_for_boot[["predicted_class"]][idx], lbl)
      # F1: 2 * (Precision * Recall) / (Precision + Recall); harmonic mean of Recall and Precision
      F1 <- f1(data_for_boot[["true_class"]][idx], data_for_boot[["predicted_class"]][idx], lbl)
      # F1_Chance: predicted f1 score if just guessing
      F1chance <- f1_chance(data_for_boot[["true_class"]][idx], data_for_boot[["predicted_class"]][idx], lbl)
      
      metrics <- c(Recall, Precision, Specificity, F1, F1chance)
      out <- c(out, metrics)
    }
    out
  }
  
  
  # # do bootstrapping with above function
  boot_out <- boot(data = data_for_boot, statistic = stat_fn_for_boot, R = R, parallel = "multicore", labels = labels)
  
  # tidy up output and CIs
  tidy_cis <- tibble()
  for (m in seq_along(metric_list)) {
    
    # basic bootstrap output
    tidy_m <- tibble(
      statistic = boot_out$t0[m],
      bias = mean(boot_out$t[,m], na.rm = TRUE)-boot_out$t0[m], 
      std.error = sd(boot_out$t[,m], na.rm = TRUE)
    )
    
    # 95% CIs, percentile
    if (n_distinct(boot_out$t[,m], na.rm = TRUE) == 1) {
      # if all values are the same (e.g. participant got 100% correct), fill CIs with NA
      tidy_m <- tidy_m %>% 
        mutate( conf.low = NA_real_, conf.high = NA_real_)
    } else {
      m_ci <- boot.ci(boot_out, index = c(m), conf = 0.95, type = "perc")
      tidy_m <- tidy_m %>% 
        mutate(conf.low = m_ci$percent[4], conf.high = m_ci$percent[5])
    }
    
    tidy_cis <- rbind(tidy_cis, tidy_m %>% mutate(metric = metric_list[m])) 
  }
  
  tidy_cis %>% 
    separate(metric, c("Label", "Metric")) %>% 
    select(Label, Metric, everything())
}

# flexible dplyr functions ####
# https://tidyr.tidyverse.org/articles/nest.html

f1_micro_boot_dataset <- function(df, true_class, predicted_class, labels = NULL, R, ...) {
  if (is.null(labels)) {(labels <- unique(df[["true_class"]])); print(labels)}

  df %>% 
    group_by(...) %>% 
    nest() %>% 
    mutate(boot_out = map(data, \(x) f1_micro_boot(x, true_class, predicted_class, labels, R = R))) %>% 
    unnest(c(boot_out))
}

metrics_boot_dataset <- function(df, true_class, predicted_class, labels = NULL, R, ...) {
  if (is.null(labels)) {(labels <- unique(df[["true_class"]])); print(labels)}
  
  df %>% 
    group_by(...) %>% 
    nest() %>% 
    mutate(boot_out = map(data, \(x) metrics_boot(x, true_class, predicted_class, labels, R = R))) %>% 
    unnest(c(boot_out))
}


