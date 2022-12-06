library(tidyverse)

# source all .R files in the Rfunctions directory
sapply(list.files("Rfunctions", full.names = TRUE), source)


#### read in data ####

valid_data <- read_csv("Data/private/live_valid-individual-data.csv") 

save_folder <- "Data/primary/"

# language from task info files #


####

data_demog <- valid_data %>% 
  
  # collapse age into bins
  mutate(
    `Age Group` = case_when(
      between(Age, 16, 20) ~ '16 - 20',
      between(Age, 21, 25) ~ '21 - 25',
      between(Age, 26, 30) ~ '26 - 30',
      between(Age, 31, 35) ~ '31 - 35',
      between(Age, 36, 40) ~ '36 - 40'
    ),
    `Age Cohort` = case_when(
      between(Age, 16, 17) ~ "Youth (16 - 17)",
      between(Age, 18, 40) ~ "Adult (18 - 40)"
    )
  ) %>% 
  
  # keep demographics variables
  select(
    PID,
    group,
    #Language,
    Gender,
    `Age Group`,
    `Age Cohort`
  )


data_demog %>% 
  write_path_csv(save_folder, "live_individual_recoded.csv")


