library(readr)
library(dplyr)

# source all .R files in the Rfunctions directory ####
sapply(list.files("Rfunctions", full.names = TRUE), source)

# read in the data ####
live_comm_data <- read_csv('Data/primary/live-comm_collated.csv', col_types = cols()) 

online_comm_data <- read_csv("Data/primary/online_comm_recoded.csv") %>% 
  filter(task == 'forced choice') %>% 
  select(-task)

# calculate performance metrics ####

## by individual in the live experiment ####
live_perf_data_indiv <- live_comm_data %>% 
  group_by(group) %>% # just to preserve the group variable label
  do(calculate_performance_metrics(., cued, response, PID))

live_perf_data_indiv %>% 
  write_path_csv("Data/processed/", "live-comm_performance-indiv.csv")

## by group in the live experiment ####
live_perf_data_group <- live_comm_data %>% 
  do(calculate_performance_metrics(., cued, response, group)) 

live_perf_data_group %>% 
  write_path_csv("Data/processed/", "live-comm_performance-group.csv")

## by group in the online experiment ####
online_perf_data_group <- online_comm_data %>% 
  do(calculate_performance_metrics(., cued, response, group)) 

online_perf_data_group %>% 
  write_path_csv("Data/processed/", "online-comm_performance-group.csv")

## combined live and online by trial 1 -6 ####

comm_data_first6 <- full_join(
  live_comm_data %>% 
    mutate(experiment = "felt touch"), 
  online_comm_data %>% 
    mutate(experiment = "viewed touch")
  ) %>% 
  filter(trial <=6)

perf_data_first6 <- comm_data_first6 %>% 
  do(calculate_performance_metrics(., cued, response, experiment, group)) 

perf_data_first6 %>% 
  write_path_csv("Data/processed/", "comm-first6_performance-group.csv")
