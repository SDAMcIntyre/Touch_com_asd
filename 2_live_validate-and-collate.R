library(readr)
library(dplyr)
library(stringr)

# source all .R files in the Rfunctions directory
sapply(list.files("Rfunctions", full.names = TRUE), source)

RAW_DATA_FOLDER <- "~/Library/CloudStorage/OneDrive-Linköpingsuniversitet/projects - in progress/Touch Comm ASD/Data/"

save_folder <- 'Data/primary/'

#
collated_comm_data <- collate_live_data(RAW_DATA_FOLDER, 'comm.*data\\.csv') 

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

collated_pleas_data <- RAW_DATA_FOLDER %>% 
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

# read data manually entered from paper surveys

live_individual_data <- read_csv(paste0(RAW_DATA_FOLDER, "demographic-data_entered-and-checked.csv")) %>% 
  full_join(read_csv(paste0(RAW_DATA_FOLDER, "AQ_entered-and-checked.csv"))) %>% 
  full_join(read_csv(paste0(RAW_DATA_FOLDER, "BAPQ_entered-and-checked.csv"))) %>% 
  full_join(read_csv(paste0(RAW_DATA_FOLDER, "STQ_entered-and-checked.csv"))) %>% 
  full_join(read_csv(paste0(RAW_DATA_FOLDER, "TAS_entered-and-checked.csv"))) %>% 
  full_join(read_csv(paste0(RAW_DATA_FOLDER, "medication-family_entered-and-checked.csv"))) %>% 
  rename("group" = Group)

## ADD VALIDATION HERE ##

private_data_folder <- "Data/private"
if (!dir.exists(private_data_folder)) {dir.create(private_data_folder)}
write_csv(live_individual_data, paste0(private_data_folder,"/live_valid-individual-data.csv"))
