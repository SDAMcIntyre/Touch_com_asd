library(readr)
library(dplyr)

# from source files:
# ggplot2
# stringr
# library(dplyr)
# library(boot)
# library(tidyr)
# library(broom)
# library(purrr)

# source all .R files in the Rfunctions directory ####
source_files <- list.files("Rfunctions", full.names = TRUE)
sapply(source_files[grepl(
  "(plot_appearance)|(performance_metrics)|(write_path_csv)", 
  source_files
  )], source)

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

Nreps <- 1000 # number of replicate samples for bootstrapping
set.seed(01112023)

## f1 average micro by individual ####

comm_F1micro_indiv <- comm_fc_data %>% 
  f1_micro_boot_dataset("cued", "response", ORDERED_CUES, R = Nreps, experiment, group, PID) 

# save to file
write_path_csv(comm_F1micro_indiv, PROCESSED_DATA_FOLDER, "comm_F1micro-indiv.csv")
save(comm_F1micro_indiv, file = paste0(PROCESSED_DATA_FOLDER, "comm_F1micro-indiv.RData"))


# strict subset (for comparison of felt vs viewed)
comm_F1micro_indiv_strict <- comm_fc_data %>% 
  filter(
    PID %in% filter(strict_subsets, felt_vs_viewed)[["PID"]]
  ) %>% 
  f1_micro_boot_dataset("cued", "response", ORDERED_CUES, R = Nreps, experiment, group, PID)

# save to files
write_path_csv(comm_F1micro_indiv_strict, PROCESSED_DATA_FOLDER, "comm_F1micro-indiv_strict.csv")
save(comm_F1micro_indiv_strict, file = paste0(PROCESSED_DATA_FOLDER, "comm_F1micro-indiv_strict.RData"))


# first 6 trials only (for comparison of felt vs viewed)
comm_F1micro_indiv_first6 <- comm_fc_data %>% 
  filter(trial <=6) %>% 
  f1_micro_boot_dataset("cued", "response", ORDERED_CUES, R = Nreps, experiment, group, PID)

# save to files
write_path_csv(comm_F1micro_indiv_first6, PROCESSED_DATA_FOLDER, "comm_F1micro-indiv_first6.csv")
save(comm_F1micro_indiv_first6, file = paste0(PROCESSED_DATA_FOLDER, "comm_F1micro-indiv_first6.RData"))


# strict subset with first 6 only (for comparison of felt vs viewed)
comm_F1micro_indiv_first6_strict <- comm_fc_data %>% 
  filter(
    trial <=6 &
      PID %in% filter(strict_subsets, felt_vs_viewed)[["PID"]]
  ) %>% 
  f1_micro_boot_dataset("cued", "response", ORDERED_CUES, R = Nreps, experiment, group, PID)

# save to files
write_path_csv(comm_F1micro_indiv_first6_strict, PROCESSED_DATA_FOLDER, "comm_F1micro-indiv_first6_strict.csv")
save(comm_F1micro_indiv_first6_strict, file = paste0(PROCESSED_DATA_FOLDER, "comm_F1micro-indiv_first6_strict.RData"))


## metrics by label (touch) and group ####

comm_metrics <- comm_fc_data %>% 
  metrics_boot_dataset("cued", "response", ORDERED_CUES, R = Nreps, experiment, group) 

# save to files
write_path_csv(comm_metrics, PROCESSED_DATA_FOLDER, "comm_metrics-group.csv")
save(comm_metrics, file = paste0(PROCESSED_DATA_FOLDER, "comm_metrics-group.RData"))

# strict subset (for separate felt and viewed analyses)
felt_strict <- comm_fc_data %>% 
  filter(
    experiment == "felt touch" &
      PID %in% filter(strict_subsets, felt_touch)[["PID"]]
  ) 

viewed_strict <- comm_fc_data %>% 
  filter(
    experiment == "viewed touch" &
      PID %in% filter(strict_subsets, viewed_touch)[["PID"]]
  ) 

comm_metrics_group_strict_each <- rbind(felt_strict, viewed_strict) %>% 
  metrics_boot_dataset("cued", "response", ORDERED_CUES, R = Nreps, experiment, group) 

# save to files
write_path_csv(comm_metrics_group_strict_each, PROCESSED_DATA_FOLDER, "comm_metrics-group_strict-each.csv")
save(comm_metrics_group_strict_each, file = paste0(PROCESSED_DATA_FOLDER, "comm_metrics-group_strict-each.RData"))

# strict subset (for comparison of felt vs viewed)
comm_metrics_group_strict_compare <- comm_fc_data %>% 
  filter(
    PID %in% filter(strict_subsets, felt_vs_viewed)[["PID"]]
  ) %>% 
  metrics_boot_dataset("cued", "response", ORDERED_CUES, R = Nreps, experiment, group) 

# save to files
write_path_csv(comm_metrics_group_strict_compare, PROCESSED_DATA_FOLDER, "comm_metrics-group_strict-compare.csv")
save(comm_metrics_group_strict_compare, file = paste0(PROCESSED_DATA_FOLDER, "comm_metrics-group_strict-compare.RData"))

# strict subset with first 6 only (for comparison of felt vs viewed)
comm_metrics_group_first6_strict <- comm_fc_data %>% 
  filter(
    trial <=6 &
      PID %in% filter(strict_subsets, felt_vs_viewed)[["PID"]]
  ) %>% 
  metrics_boot_dataset("cued", "response", ORDERED_CUES, R = Nreps, experiment, group)

# save to files
write_path_csv(comm_metrics_group_first6_strict, PROCESSED_DATA_FOLDER, "comm_metrics-group_first6_strict.csv")
save(comm_metrics_group_first6_strict, file = paste0(PROCESSED_DATA_FOLDER, "comm_metrics-group_first6_strict.RData"))


## metrics by label and individual, felt only ####

felt_metrics_indiv <- comm_fc_data %>% 
  filter(experiment == "felt touch") %>% 
  metrics_boot_dataset("cued", "response", ORDERED_CUES, R = Nreps, experiment, group, PID)

# save to file
write_path_csv(felt_metrics_indiv, PROCESSED_DATA_FOLDER, "comm-felt_metrics-indiv.csv")
save(felt_metrics_indiv, file = paste0(PROCESSED_DATA_FOLDER, "comm-felt_metrics-indiv.RData"))

# strict subset
felt_metrics_indiv_strict <- comm_fc_data %>% 
  filter(
    experiment == "felt touch" &
      PID %in% filter(strict_subsets, felt_touch)[["PID"]]
  ) %>% 
  metrics_boot_dataset("cued", "response", ORDERED_CUES, R = Nreps, experiment, group, PID)

# save to file 
write_path_csv(felt_metrics_indiv_strict, PROCESSED_DATA_FOLDER, "comm-felt_metrics-indiv_strict.csv")
save(felt_metrics_indiv_strict, file = paste0(PROCESSED_DATA_FOLDER, "comm-felt_metrics-indiv_strict.RData"))

