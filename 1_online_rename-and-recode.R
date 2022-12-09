# load libraries  ####
library(readr)
library(dplyr)
library(summarytools)
library(stringr)
library(tidyr)

# source all .R files in the Rfunctions directory ####
sapply(list.files("Rfunctions", full.names = TRUE), source)

# input path ####
PRIMARY_DATA_FOLDER <- "Data/primary/"

# output paths ####
REPORTS_FOLDER <- "Data/reports/"
PROCESSED_DATA_FOLDER <- "Data/processed/"

# ===== MAIN ===== 

# read in the primary online data file ####
online_data <- read_csv(paste0(PRIMARY_DATA_FOLDER, "online_valid-anon-data.csv"))

# rename online data #### 

#. get the names from qualtrics ####
old_var_names <- names(online_data)

#. define and apply new variable names ####
new_var_names <- get_nice_var_names(online_data, q_idx = 36:161)
names(online_data) <- new_var_names

#. update the variable name report to show how names were changed ####
read_csv(paste0(REPORTS_FOLDER, "online_qualtrics-variable-names.csv")) %>% 
  mutate(nice_names = new_var_names) %>% 
  write_path_csv(REPORTS_FOLDER, "online_qualtrics-variable-names.csv")

#. summary table ####
online_data %>% 
  select(-matches(" DO")) %>% # don't show display order variables
  dfSummary() %>% view


# recode online data #### 

####. independent variables   #### 
data_indep <- online_data %>% 
  
  # make group variable from ASD question
  mutate(
    group = case_when(
      ASD == "Yes" ~ "ASD",
      ASD == "No" ~ "Control"
    ) 
  ) %>% 
  
  # make task variable for which comm task they were assigned
  mutate(
    task = coalesce(
      `Communication Condition ASD`,
      `Communication Condition Control`
    )
  ) %>% 
  select(-starts_with("Communication Condition")) %>% 
  mutate(
    task = case_when(
      str_detect(task, "FL_(40|29)") ~ "forced choice", 
      str_detect(task, "FL_(36|30)") ~ "free text"
    )
  ) %>% 
  
  # keep independent variables
  select(c(
    PID,
    group,
    task
  ))


####. communication data ####

data_comm_trial_order <- online_data %>% 
  # make single variable for display order (qualtrics gives separate variables for group and Task)
  mutate(
    `Comm Display Order` = coalesce(
      `CommunicationFC DO ASD`,
      `CommunicationFT DO ASD`,
      `CommunicationFC DO Control`,
      `CommunicationFT DO Control`
    ) 
  )  %>% extract_trial_numbers_from_order("Comm Display Order")


data_comm <- online_data %>% 
  
  # re-name FC responses
  mutate(across(
    .cols = matches("CommunicationFC .+") & !contains('DO'),
    .fns = extract_touches
  )) %>% 
  
  # keep comm variables
  select(
    PID, 
    matches("CommunicationF. .+") & !contains('DO')
  )  %>% 
  
  # put into tidy format like live study
  pivot_longer(
    cols = contains("F"),
    names_to = "cued",
    names_prefix = "CommunicationF. ",
    names_transform = list(cued = tolower),
    values_to = "response"
  ) %>% 
  
  # remove NAs due to not being in the condition (FC/FT)
  na.omit()

####. pleasantness data   #### 

data_pleas_trial_order <- online_data %>% 
  extract_trial_numbers_from_order("Pleasantness DO")

data_pleas <- online_data %>% 
  
  # keep pleasantness variables
  select(
    PID, 
    matches("Pleasantness ") & !contains('DO')
  )  %>% 
  
  # put into tidy format like live study
  pivot_longer(
    cols = matches("Pleasantness "),
    names_to = "cued",
    names_prefix = "Pleasantness ",
    names_transform = list(cued = tolower),
    values_to = "response"
  ) %>% 
  
  #re-scale VAS ratings to match live experiment
  mutate(
    response = response/5 - 10
  ) 

####. qualtrics variables   #### 
data_qualtrics <- online_data %>% 
  
  # add minutes/hours duration data 
  mutate(
    `Duration (minutes)` = `Duration (in seconds)`/60,
    `Duration (hours)` = `Duration (in seconds)`/(60*60)
  ) %>% 
  
  # keep qualtrics variables
  select(c(
    PID,
    `Start Date`,
    `End Date`,
    `Recorded Date`,
    `Duration (minutes)`,
    `Duration (hours)`
  ))

####. demographics variables   #### 
data_demog <- online_data %>% 
  
  # nice language response labels
  mutate(
    Language = case_when(
      `User Language` == "EN" ~ "English",
      `User Language` == "SV" ~ "Swedish"
    )
  ) %>% 
  
  # nice country of residence names
  mutate(
    `Country of Residence` = coalesce(
      `Country of Residence (EN)`, 
      `Country of Residence (SV)`
    )
  ) %>% 
  mutate(
    `Country of Residence` = case_when(
      `Country of Residence` == "Sverige" ~ "Sweden",
      `Country of Residence` == "United Kingdom of Great Britain and Northern Ireland" ~ "UK",
      `Country of Residence` == "United States of America" ~ "USA",
      TRUE ~ `Country of Residence`
    )
  ) %>% 
  
  # keep demographics variables
  select(
    PID,
    Language,
    `Country of Residence`,
    Gender,
    `Age Group`,
    `Age Cohort`
  )

####. AQ ####
data_aq <- online_data %>% 
  recode_AQ() %>% 
  select(
    PID, 
    AQ_total,
    AQ_n_missing
  )

####. BAPQ ####

data_bapq <- online_data %>% 
  recode_BAPQ() %>% 
  select(
    PID,
    BAPQ_total,
    BAPQ_sub_Aloof,
    BAPQ_sub_PragLang,
    BAPQ_sub_Rigid,
    BAPQ_n_missing
  )

####. STQ ####

data_stq <- online_data %>% 
  recode_STQ() %>% 
  select(
    PID,
    STQ_total,
    STQ_n_missing
  )

####. TAS ####

data_tas <- online_data %>% 
  recode_TAS() %>% 
  select(
    PID,
    TAS_total,
    TAS_sub_IdFeelings,
    TAS_sub_DescFeelings,
    TAS_sub_ExtThinking,
    TAS_n_missing
  ) 

#### save data ####

####. communication data ####

data_indep %>% 
  full_join(data_comm_trial_order) %>% 
  full_join(data_comm) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "online_comm-data.csv")

####. pleasantness data ####

data_indep %>% 
  full_join(data_pleas_trial_order) %>% 
  full_join(data_pleas) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "online_pleas-data.csv")

####. individual data (demographics and questionnaires) ####

data_indep %>% 
  full_join(data_qualtrics) %>% 
  full_join(data_demog) %>% 
  full_join(data_aq) %>% 
  full_join(data_bapq) %>% 
  full_join(data_stq) %>% 
  full_join(data_tas) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "online_indiv-data.csv")

