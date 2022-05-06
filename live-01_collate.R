library(readr)
library(dplyr)
library(stringr)

# source all .R files in the Rfunctions directory
sapply(list.files("Rfunctions", full.names = TRUE), source)

raw_data_folder <- '~/Library/CloudStorage/OneDrive-Linköpingsuniversitet/projects - in progress/Touch Comm ASD/Data'
save_folder <- 'Data/primary/'

collated_comm_data <- raw_data_folder %>% 
  collate_live_data('comm.*data\\.csv') 

# expect 60 trials per participant, check for discrepancies
collated_comm_data %>% 
  group_by(path) %>% 
  tally %>% 
  filter(n != 60)

collated_comm_data %>% 
  group_by(PID) %>% 
  tally %>% 
  filter(n != 60)

collated_comm_data %>% 
  select(-path) %>%
  write_path_csv(save_folder,'live-comm_collated.csv')

collated_pleas_data <- raw_data_folder %>% 
  collate_live_data('pleas.*data\\.csv') 

# expect 6 trials per participant, check for discrepancies
collated_pleas_data %>% 
  group_by(path) %>% 
  tally %>% 
  filter(n != 6)

collated_pleas_data %>% 
  group_by(PID) %>% 
  tally %>% 
  filter(n != 6)

collated_pleas_data %>% 
  select(-path) %>%
  write_path_csv(save_folder,'live-pleas_collated.csv')
