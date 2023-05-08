library(readr)
library(dplyr)

# source all .R files in the Rfunctions directory ####
sapply(list.files("Rfunctions", full.names = TRUE), source)

# folder paths ####
PROCESSED_DATA_FOLDER <- "Data/processed/"

# read in the data ####
comm_fc_data <- read_csv(paste0(PROCESSED_DATA_FOLDER,'communication-data.csv'), col_types = "cccccccccc") %>% 
  filter(task == 'forced choice') %>% 
  select(-task)

# calculate performance metrics ####

## by individual ####
comm_fc_data %>% 
  group_by(experiment, group) %>% # just to preserve the variable labels
  do(calculate_performance_metrics(., cued, response, PID)) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "comm_performance-indiv.csv")

## by group ####

comm_fc_data %>% 
  do(calculate_performance_metrics(., cued, response, experiment, group)) %>%  
  write_path_csv(PROCESSED_DATA_FOLDER, "comm_performance-group.csv")

## combined live and online by trial 1 -6 ####

comm_fc_data %>% 
  filter(trial <=6) %>% 
  do(calculate_performance_metrics(., cued, response, experiment, group)) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "comm_performance-group-first6.csv")
