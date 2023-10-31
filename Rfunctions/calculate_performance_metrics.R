library(dplyr)
library(boot)
library(tidyr)
library(broom)
# MLmetrics
# https://github.com/renatopanda/MLmetrics
# devtools::install_github("renatopanda/MLmetrics") # this version has F1 averaging options
library(MLmetrics)
library(purrr)

# bootstrapping
# https://bookdown.org/compfinezbook/introcompfinr/The-Nonparametric-Bootstrap.html
# https://search.r-project.org/CRAN/refmans/boot/html/boot.html
# https://stackoverflow.com/questions/31813228/bootstrap-confidence-intervals-for-more-than-one-statistics-through-boot-ci-func


# overwrite MLmetrics function for specificity which is missing sum() for the true negative calculation
Specificity <- function(y_true, y_pred, positive = NULL) {
  Confusion_DF <- ConfusionDF(y_pred, y_true)
  if (is.null(positive) == TRUE)
    positive <- as.character(Confusion_DF[1, 1])
  TN <- as.integer(sum(
    Confusion_DF[
      which(Confusion_DF$y_true != positive &
              Confusion_DF$y_pred != positive),
      "Freq"]
  ))
  FP <- as.integer(sum(
    Confusion_DF[
      which(Confusion_DF$y_true != positive &
              Confusion_DF$y_pred == positive),
      "Freq"]
  ))
  Specificity <- TN/(TN + FP)
  return(Specificity)
}

F1_Chance <- function(y_true, y_pred, positive = NULL) {
  Confusion_DF <- ConfusionDF(y_pred, y_true)
  if (is.null(positive) == TRUE)
    positive <- as.character(Confusion_DF[1, 1])
  
  Present <- as.integer(sum(
    Confusion_DF[
    which(Confusion_DF$y_true == positive),
    "Freq"
  ]
  ))
  
  Total <- as.integer(sum(
    Confusion_DF$Freq
    ))
  
  # predicted f1 score if just guessing
  2 * (
    ( Present / Total ) /
      ((Present / Total) + 1)
  )

}

ConfusionDF(comm_fc_data$response, comm_fc_data$cued)
ConfusionMatrix(comm_fc_data$response, comm_fc_data$cued)
# individual
F1_Score(comm_fc_data$cued, comm_fc_data$response) 
F1_Score(comm_fc_data$cued, comm_fc_data$response, "attention") # takes the first one (attention) if no default
F1_Score(comm_fc_data$cued, comm_fc_data$response, c("happiness"))
# f1("happiness", comm_fc_data$cued, comm_fc_data$response)

Precision(comm_fc_data$cued, comm_fc_data$response, "happiness")
Recall(comm_fc_data$cued, comm_fc_data$response, "happiness")
#Sensitivity(comm_fc_data$cued, comm_fc_data$response, "happiness") # same as Recall
Specificity(comm_fc_data$cued, comm_fc_data$response, "happiness")

# "statistic" function to pass to boot
f1_metrics_for_boot <- function(xdata, idx, labels = NULL) {
  if (is.null(labels)) {(labels <- unique(xdata[["true_class"]])); print(labels)}
  
  # overall
  F1_micro <- F1_Score_micro(xdata[["true_class"]][idx], xdata[["predicted_class"]][idx], labels)
  out <- c(F1_micro)
  
  # by label
  for (lbl in labels) {
    # Recall: Hits / positive cases [Hits + Misses]; prop. correct, positive predictive value (PPV), Sensitivity
    Recall <- Recall(xdata[["true_class"]][idx], xdata[["predicted_class"]][idx], lbl) 
    # Precision: Hits / positive predictions [Hits + False Alarms]
    Precision <- Precision(xdata[["true_class"]][idx], xdata[["predicted_class"]][idx], lbl)
    # Specificity: Correct Rejections / negative cases [CR + misses]
    Specificity <- Specificity(xdata[["true_class"]][idx], xdata[["predicted_class"]][idx], lbl)
    # F1: 2 * (Precision * Recall) / (Precision + Recall); harmonic mean of Recall and Precision
    F1 <- F1_Score(xdata[["true_class"]][idx], xdata[["predicted_class"]][idx], lbl)
    # F1_Chance: predicted f1 score if just guessing
    F1_Chance <- F1_Chance(xdata[["true_class"]][idx], xdata[["predicted_class"]][idx], lbl)
    
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


# https://tidyr.tidyverse.org/articles/nest.html

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

boot_result <- calculate_metrics(comm_fc_data, "cued", "response", ORDERED_CUES, R = 70, experiment, group, PID)

gp1 <- comm_fc_data %>% filter(experiment == "felt touch" & group == "ASD" & PID == "asd01")
calculate_metrics(gp1, "cued", "response", ORDERED_CUES, R = 70)
boot_metrics(gp1, "cued", "response", ORDERED_CUES, R = 70)

overall <- boot_result %>%
  select(c(..., overall_out)) %>%
  unnest(c(overall_out))

by_label <- boot_result %>%
  select(c(..., by_label)) %>%
  unnest(c(by_label))
