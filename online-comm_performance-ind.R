library(readr)
library(dplyr)

# source all .R files in the Rfunctions directory
sapply(list.files("Rfunctions", full.names = TRUE), source)

# read in the data
online_comm_data <- read_csv("Data/primary/online_comm_recoded.csv") %>% 
  filter(task == 'forced choice')


# calculate performance metrics by group in the online experiment
online_comm_data %>% 
  group_by(group, PID) %>% 
  do(calculate_performance_metrics(., cued, response)) %>% 
  write_path_csv("Data/processed/online-comm_performance.csv")
