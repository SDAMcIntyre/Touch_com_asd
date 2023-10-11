library(readr)
library(dplyr)
library(boot)

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

strict_subsets <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, "PIDs-included-in-strict-subsets.csv"),
  col_types = cols()
)

# calculate performance metrics ####

## by individual, felt only ####
comm_fc_data %>% 
  filter(experiment == "felt touch") %>% 
  calculate_metrics(ORDERED_CUES, "cued", "response", R = 1000, group, PID) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "comm-felt-touch_performance-indiv.csv")

# strict subset
comm_fc_data %>% 
  filter(
    experiment == "felt touch" &
      PID %in% filter(strict_subsets, felt_touch)[["PID"]]
      ) %>% 
  calculate_metrics(ORDERED_CUES, "cued", "response", R = 1000, group, PID) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "comm-felt-touch_performance-indiv_strict.csv")

## by group ####

comm_fc_data %>% 
  calculate_metrics(ORDERED_CUES, "cued", "response", R = 1000, experiment, group) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "comm_performance-group.csv")

# strict subset (for separate felt and viewed analyses)
felt_strict <- comm_fc_data %>% 
  filter(
    experiment == "felt touch" &
    PID %in% filter(strict_subsets, felt_touch)[["PID"]]
  ) %>% 
  calculate_metrics(ORDERED_CUES, "cued", "response", R = 1000, experiment, group) 

viewed_strict <- comm_fc_data %>% 
  filter(
    experiment == "viewed touch" &
      PID %in% filter(strict_subsets, viewed_touch)[["PID"]]
  ) %>% 
  calculate_metrics(ORDERED_CUES, "cued", "response", R = 1000, experiment, group) 

rbind(felt_strict, viewed_strict) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "comm_performance-group_strict-each.csv")

# strict subset (for comparision of felt vs viewed)
comm_fc_data %>% 
  filter(
      PID %in% filter(strict_subsets, felt_vs_viewed)[["PID"]]
  ) %>% 
  calculate_metrics(ORDERED_CUES, "cued", "response", R = 1000, experiment, group) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "comm_performance-group_strict-compare.csv")


## combined live and online by trial 1 -6 (first presentation of each touch) ####

comm_fc_data %>% 
  filter(trial <=6) %>% 
  calculate_metrics(ORDERED_CUES, "cued", "response", R = 1000, experiment, group) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "comm_performance-group-first6.csv")

# strict subset
comm_fc_data %>% 
  filter(
    trial <=6 &
      PID %in% filter(strict_subsets, felt_vs_viewed)[["PID"]]
  ) %>% 
  calculate_metrics(ORDERED_CUES, "cued", "response", R = 1000, experiment, group) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "comm_performance-group-first6_strict-compare.csv")
