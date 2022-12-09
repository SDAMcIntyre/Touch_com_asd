# load libraries  ####
library(readr)
library(dplyr)
library(stringr)

# source all .R files in the Rfunctions directory ####
sapply(list.files("Rfunctions", full.names = TRUE), source)

# input path ####
RAW_DATA_FOLDER <- "~/Library/CloudStorage/OneDrive-Linköpingsuniversitet/projects - in progress/Touch Comm ASD/Data/"

# output paths ####
PRIMARY_DATA_FOLDER <- 'Data/primary/'

# participant exclusions ####
EXCLUDED_PIDS <- c(
  "asd19", # exclusion criterion missed in screening
  "sub15" # no data for either touch task
)

# ===== MAIN ===== 

# collate communication task data ####
comm_data <- collate_live_data(RAW_DATA_FOLDER, 'comm.*data\\.csv') %>% 
  #. remove excluded participants ####
  filter(!(PID %in% EXCLUDED_PIDS)) 

# expect 60 trials per participant, check for discrepancies
comm_data %>% 
  group_by(path) %>% 
  tally %>% 
  filter(n != 60)

comm_data %>% 
  group_by(PID) %>% 
  tally %>% 
  filter(n != 60)

# asd 13 - the task was stopped part-way through and re-started, so there are two files, and one extra trial was run. 

# collate pleasantness rating data ####
pleas_data <- collate_live_data(RAW_DATA_FOLDER, 'pleas.*data\\.csv')  %>% 
  #. remove excluded participants ####
  filter(!(PID %in% EXCLUDED_PIDS)) 

# expect 6 trials per participant, check for discrepancies
pleas_data %>% 
  group_by(path) %>% 
  tally %>% 
  filter(n != 6)

pleas_data %>% 
  group_by(PID) %>% 
  tally %>% 
  filter(n != 6)

# read data manually entered from paper surveys ####

indiv_data <- read_csv(paste0(RAW_DATA_FOLDER, "demographic-data_entered-and-checked.csv")) %>% 
  full_join(read_csv(paste0(RAW_DATA_FOLDER, "AQ_entered-and-checked.csv"))) %>% 
  full_join(read_csv(paste0(RAW_DATA_FOLDER, "BAPQ_entered-and-checked.csv"))) %>% 
  full_join(read_csv(paste0(RAW_DATA_FOLDER, "STQ_entered-and-checked.csv"))) %>% 
  full_join(read_csv(paste0(RAW_DATA_FOLDER, "TAS_entered-and-checked.csv"))) %>% 
  rename("group" = Group)

# read in separately, as it contains lots of potentially identifying info
med_fam_data <- read_csv(paste0(RAW_DATA_FOLDER, "medication-family_entered-and-checked.csv"))

anon_indiv_data <- indiv_data %>% 
  mutate(
    # anonymise experimenter ####
    Experimenter = Experimenter %>% as.factor() %>% as.numeric(),
    
    # collapse age into bins ####
    `Age Group` = case_when(
      between(Age, 16, 20) ~ '16 - 20',
      between(Age, 21, 25) ~ '21 - 25',
      between(Age, 26, 30) ~ '26 - 30',
      between(Age, 31, 35) ~ '31 - 35',
      between(Age, 36, 40) ~ '36 - 40'
    ),
    `Age Cohort` = case_when(
      between(Age, 16, 17) ~ "Youth (16 - 17)",
      between(Age, 18, 40) ~ "Adult (18 - 40)",
      PID == "asd01" ~ "Adult (18 - 40)" # age data missing for asd01 but files were saved in the "adult" folder
    )
  ) %>% 
  
  # remove exact age for privacy ####
  select(-Age)

# quick look at the data
anon_indiv_data %>% 
  select(
    -(starts_with("AQ") | starts_with("BAPQ") | starts_with("STQ") | starts_with("TAS") )
    ) %>% View
  
# save data files ####
comm_data %>% 
  #. remove source file name with date information for privacy ####
select(-path) %>%
  write_path_csv(PRIMARY_DATA_FOLDER,'live_comm-data.csv')

pleas_data %>% 
  #. remove source file name with date information for privacy ####
select(-path) %>%
  write_path_csv(PRIMARY_DATA_FOLDER,'live_pleas-data.csv')

anon_indiv_data %>% 
  write_path_csv(PRIMARY_DATA_FOLDER,"live_valid-anon-indiv-data.csv")

