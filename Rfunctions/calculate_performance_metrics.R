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


#. "statistic" functions to pass to boot ####

# for testing:
library(readr)
PROCESSED_DATA_FOLDER <- "Data/processed/"
comm_fc_data <- read_csv(
  paste0(PROCESSED_DATA_FOLDER,'communication-data.csv'), 
  col_types = "cccccicc"
) %>% 
  filter(task == 'forced choice') %>% 
  select(-task)

source("Rfunctions/plot_appearance.R")
gp1 <- comm_fc_data %>% filter(experiment == "felt touch" & group == "ASD" & PID == "asd01")
gp65 <- comm_fc_data %>% filter(experiment == "felt touch" & group == "Control" & PID == "sub32")

# working, f1 micro only
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

# flexible dplyr functions ####
# https://tidyr.tidyverse.org/articles/nest.html

f1_micro_boot_dataset <- function(df, true_class, predicted_class, labels, R, ...) {
  df %>% 
    group_by(...) %>% 
    nest() %>% 
    mutate(boot_out = map(data, \(x) f1_micro_boot(x, true_class, predicted_class, labels, R = R))) %>% 
    unnest(c(boot_out))
}

# working
f1_micro_boot_dataset(comm_fc_data, "cued", "response", ORDERED_CUES, R = 100, experiment, group, PID) 

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
      bias = mean(boot_out$t[,m])-boot_out$t0[m], 
      std.error = sd(boot_out$t[,m])
    )
    
    # 95% CIs, percentile
    if (n_distinct(boot_out$t[,m]) == 1) {
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
  
  tidy_cis
}

# working
metrics_boot(gp1, "cued", "response", ORDERED_CUES, R = 100) %>% 
  separate(metric, c("Label", "Metric"))

# old

f1_for_boot <- function(xdata, idx, label) {
  f1(xdata[["true_class"]][idx], xdata[["predicted_class"]][idx], label)
}

metrics_combined_for_boot <- function(xdata, idx, labels = NULL) {
  if (is.null(labels)) {(labels <- unique(xdata[["true_class"]])); print(labels)}
  
  # overall
  F1_micro <- f1_ave_micro(xdata[["true_class"]][idx], xdata[["predicted_class"]][idx], labels)
  out <- c(F1_micro) 
  
  # by label
  for (lbl in labels) {
    # Recall: Hits / positive cases [Hits + Misses]; prop. correct, positive predictive value (PPV), Sensitivity
    Recall <- recall(xdata[["true_class"]][idx], xdata[["predicted_class"]][idx], lbl) 
    # Precision: Hits / positive predictions [Hits + False Alarms]
    Precision <- precision(xdata[["true_class"]][idx], xdata[["predicted_class"]][idx], lbl)
    # Specificity: Correct Rejections / negative cases [CR + misses]
    Specificity <- specificity(xdata[["true_class"]][idx], xdata[["predicted_class"]][idx], lbl)
    # F1: 2 * (Precision * Recall) / (Precision + Recall); harmonic mean of Recall and Precision
    F1 <- f1(xdata[["true_class"]][idx], xdata[["predicted_class"]][idx], lbl)
    # F1_Chance: predicted f1 score if just guessing
    F1_Chance <- f1_chance(xdata[["true_class"]][idx], xdata[["predicted_class"]][idx], lbl)
    
    metrics <- c(Recall, Precision, Specificity, F1, F1_Chance)
    out <- c(out, metrics)
  }
  out
}

boot_metrics <- function(df, true_class, predicted_class, labels = NULL, R) {
  if (is.null(labels)) {(labels <- unique(xdata[["true_class"]])); print(labels)}

  xdata <- tibble(
    true_class = df[[true_class]],
    predicted_class = df[[predicted_class]]
  )
  # xdata <- tibble(
  #   true_class = gp1$cued,
  #   predicted_class = gp1$response
  # )

  # # do bootstrapping on f1 and other metrics
  f1_boot <- boot(data = xdata, statistic = f1_metrics_for_boot, R = R, parallel = "multicore", labels = labels)
  # f1_boot <- boot(data = xdata, statistic = f1_metrics_for_boot, R = 100, parallel = "multicore", labels = ORDERED_CUES)

  # apply nice labels
  f1_boot_ci <- f1_boot %>%
    tidy(conf.int=TRUE, conf.method="perc") %>%
    mutate(
      label = c("overall", rep(labels,5))
      # label = c("overall", rep(ORDERED_CUES,5))
    )

  # split into separate dfs for overall stats and stats by label

  overall <- f1_boot_ci %>%
    filter(label == "overall") %>%
    rename(F1_micro = statistic) %>%
    select(-label)

  by_label <- f1_boot_ci %>%
    filter(label != "overall") %>%
    mutate(
      metric = rep(c("Recall", "Precision", "Specificity", "F1", "F1_Chance"), each = length(labels))
    ) %>%
    rename(value = statistic) %>%
    pivot_wider(
      names_from = metric,
      names_glue = "{metric}_{.value}",
      values_from = c(value, bias, std.error, conf.low, conf.high)
    )

  list(overall, by_label)
}



# old stuff not working ####
calculate_metrics <- function(df, true_class, predicted_class, labels, R, ...) {
  boot_result <- df %>% 
    group_by(...) %>% 
    nest() %>% 
    mutate(boot_out = map(data, \(x) boot_metrics(x, true_class, predicted_class, labels, R = R))) %>% 
    mutate(
      overall = map(boot_out, \(x) x[[1]]),
      by_label = map(boot_out, \(x) x[[2]])
    )
  
  boot_result
  # overall <- boot_result %>%
  #   select(c(..., overall_out)) %>%
  #   unnest(c(overall_out))
  
  # by_label <- boot_result %>% 
  #   select(c(..., by_label)) %>% 
  #   unnest(c(by_label))
  
  # list(overall = overall, by_label = by_label, boot_result = boot_result)
}


boot_result <- calculate_metrics(comm_fc_data, "cued", "response", ORDERED_CUES, R = 100, experiment, group, PID)

calculate_metrics(gp1, "cued", "response", ORDERED_CUES, R = 70)

overall <- boot_result %>%
  select(c(..., overall_out)) %>%
  unnest(c(overall_out))

by_label <- boot_result %>%
  select(c(..., by_label)) %>%
  unnest(c(by_label))
