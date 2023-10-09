library(readr)
library(dplyr)

# source all .R files in the Rfunctions directory ####
sapply(list.files("Rfunctions", full.names = TRUE), source)

# folder paths ####
PROCESSED_DATA_FOLDER <- "Data/processed/"

# read in the data ####
comm_fc_data <- read_csv(
  paste0(PROCESSED_DATA_FOLDER,'communication-data.csv'), 
  col_types = "cccccicc"
  ) %>% 
  filter(task == 'forced choice') %>% 
  select(-task)

# calculate performance metrics ####

# bootstrapping
# https://bookdown.org/compfinezbook/introcompfinr/The-Nonparametric-Bootstrap.html
# https://search.r-project.org/CRAN/refmans/boot/html/boot.html

x <- comm_fc_data %>% 
  filter(experiment == "felt touch")

f1(label = "attention", x$cued, x$response)
f1_chance("attention", x$cued, x$response)

xdata <- tibble(
  item = x$cued,
  resp = x$response
) 

library(boot)

f1_for_boot <- function(xdata, idx, label) {
  f1(label, xdata[["item"]][idx], xdata[["resp"]][idx])
}

f1_for_boot(xdata, 1:4081, label = "happiness")

f1_boot <- boot(xdata, f1_for_boot, R = 1000, label = "happiness")
plot(f1_boot)
boot.ci(f1_boot, conf = 0.95, type="perc")

comm_fc_data %>% 
  filter(experiment == "felt touch") %>% 
  group_by(group) %>% # just to preserve the variable label
  do(calculate_performance_metrics(., cued, response, PID))

## by individual, felt only ####
comm_fc_data %>% 
  filter(experiment == "felt touch") %>% 
  group_by(group) %>% # just to preserve the variable label
  do(calculate_performance_metrics(., cued, response, PID)) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "comm-felt-touch_performance-indiv.csv")

## by group ####

comm_fc_data %>% 
  group_by(experiment) %>% 
  do(calculate_performance_metrics(., cued, response, group)) %>%  
  write_path_csv(PROCESSED_DATA_FOLDER, "comm_performance-group.csv")

## combined live and online by trial 1 -6 (first presentation of each touch) ####

comm_fc_data %>% 
  filter(trial <=6) %>% 
  group_by(experiment) %>% 
  do(calculate_performance_metrics(., cued, response, group)) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "comm_performance-group-first6.csv")
