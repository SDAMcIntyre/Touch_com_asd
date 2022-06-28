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

# combined live and online by trial 1 -6
live_comm_data <- read_csv('Data/primary/live-comm_collated.csv', col_types = cols()) %>% 
  mutate(experiment = "felt touch")

online_comm_data <- read_csv('Data/primary/online_comm_recoded.csv') %>% 
  filter(task == 'forced choice') %>% 
  select(-task) %>% 
  mutate(experiment = "viewed touch")

comm_data <- full_join(live_comm_data, online_comm_data)

trial.1data <- comm_data %>% filter(trial <=6)