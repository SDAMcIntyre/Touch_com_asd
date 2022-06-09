library(readr)
library(dplyr)

# source all .R files in the Rfunctions directory
sapply(list.files("Rfunctions", full.names = TRUE), source)

# read in the data
live_comm_data <- read_csv('Data/primary/live-comm_collated.csv', col_types = cols()) 

online_comm_data <- read_csv("Data/primary/online_comm_recoded.csv") %>% 
  filter(task == 'forced choice')

# calculate performance metrics
# by individual in the live experiment
live_comm_data %>% 
  group_by(group) %>% 
  do(calculate_performance_metrics(., cued, response, PID)) %>% 
  write_path_csv("Data/processed/", "live-comm_performance-indiv.csv")

# by group in the live experiment
live_comm_data %>% 
  group_by(group) %>% 
  do(calculate_performance_metrics(., cued, response)) %>% 
  write_path_csv("Data/processed/", "live-comm_performance-group.csv")

# by group in the online experiment
online_comm_data %>% 
  group_by(group) %>% 
  do(calculate_performance_metrics(., cued, response)) %>% 
  write_path_csv("Data/processed/", "online-comm_performance-group.csv")
