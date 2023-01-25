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

# read in  primary data files ####
online_data <- read_csv(paste0(PRIMARY_DATA_FOLDER, "online_valid-anon-data.csv")) 

live_data_indiv <- read_csv(paste0(PRIMARY_DATA_FOLDER, "live_valid-anon-indiv-data.csv")) %>% 
  mutate(
    experiment = "felt touch",
    task = "forced choice"
  )

live_data_comm <- read_csv(paste0(PRIMARY_DATA_FOLDER, "live_comm-data.csv")) %>% 
  mutate(
    experiment = "felt touch",
    PID = str_replace(PID,"[0-9]+", str_pad(str_extract(PID,"[0-9]+"), 2, pad = "0"))
    )

live_data_pleas <- read_csv(paste0(PRIMARY_DATA_FOLDER, "live_pleas-data.csv")) %>% 
  mutate(
    experiment = "felt touch",
    PID = str_replace(PID,"[0-9]+", str_pad(str_extract(PID,"[0-9]+"), 2, pad = "0"))
    )

# RENAME ONLINE DATA #### 

#. define and apply new variable names ####
new_var_names <- get_nice_var_names(online_data, q_idx = 36:161)
names(online_data) <- new_var_names

#. update the variable name report to show how names were changed ####
read_csv(paste0(REPORTS_FOLDER, "online_qualtrics-variable-names.csv")) %>% 
  mutate(nice_name = new_var_names) %>% 
  write_path_csv(REPORTS_FOLDER, "online_qualtrics-variable-names.csv")

online_data <- online_data %>% 
  mutate(experiment = "viewed touch")

#. summary table ####
online_data %>% 
  select(-matches(" DO")) %>% # don't show display order variables
  dfSummary() %>% view

# RECODE ONLINE DATA #### 

####. independent variables   #### 

online_data_indep <- online_data %>% 
  
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
    experiment,
    PID,
    group,
    task
  ))


####. communication data ####

online_data_comm_trial_order <- online_data %>% 
  # make single variable for display order (qualtrics gives separate variables for group and Task)
  mutate(
    `Comm Display Order` = coalesce(
      `CommunicationFC DO ASD`,
      `CommunicationFT DO ASD`,
      `CommunicationFC DO Control`,
      `CommunicationFT DO Control`
    ) 
  )  %>% extract_trial_numbers_from_order("Comm Display Order")


online_data_comm <- online_data %>% 
  
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
  
  # add Experimenter code
  mutate(
    Experimenter = 3
  ) %>% 
  
  # remove NAs due to not being in the condition (FC/FT)
  na.omit() %>% 
  
  # combine with trial order
  full_join(online_data_comm_trial_order)

####. pleasantness data   #### 

online_data_pleas_trial_order <- online_data %>% 
  extract_trial_numbers_from_order("Pleasantness DO")

online_data_pleas <- online_data %>% 
  
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
  ) %>% 
  
  # combine with trial order
  full_join(online_data_pleas_trial_order)

####. qualtrics variables   #### 
online_data_qualtrics <- online_data %>% 
  
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
online_data_demog <- online_data %>% 
  
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

# SEPARATE LIVE DATA ####

#. independent variables ####
live_data_indep <- live_data_indiv %>% 
  select(
    experiment,
    PID,
    group,
    Experimenter,
    task
    )

#. demographics variables ####
live_data_demog <- live_data_indiv %>% 
  mutate(`Country of Residence` = "Sweden") %>% 
  select(
  PID,
  Language,
  `Country of Residence`,
  Gender,
  `Age Group`,
  `Age Cohort`
)

# BOTH - RECODE QUESTIONNAIRE DATA ####

####. AQ ####
online_data_aq <- online_data %>% 
  recode_AQ() %>% 
  select(
    PID, 
    AQ_total,
    AQ_n_missing
  )

live_data_aq <- live_data_indiv %>% 
  recode_AQ() %>% 
  select(
    PID, 
    AQ_total,
    AQ_n_missing
  ) 

####. BAPQ ####

online_data_bapq <- online_data %>% 
  recode_BAPQ() %>% 
  select(
    PID,
    BAPQ_total,
    BAPQ_sub_Aloof,
    BAPQ_sub_PragLang,
    BAPQ_sub_Rigid,
    BAPQ_n_missing
  )

live_data_bapq <- live_data_indiv %>% 
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

online_data_stq <- online_data %>% 
  recode_STQ() %>% 
  select(
    PID,
    STQ_total,
    STQ_n_missing
  )

live_data_stq <- live_data_indiv %>% 
  recode_STQ() %>% 
  select(
    PID,
    STQ_total,
    STQ_n_missing
  )

####. TAS ####

online_data_tas <- online_data %>% 
  recode_TAS() %>% 
  select(
    PID,
    TAS_total,
    TAS_sub_IdFeelings,
    TAS_sub_DescFeelings,
    TAS_sub_ExtThinking,
    TAS_n_missing
  ) 

live_data_tas <- live_data_indiv %>% 
  recode_TAS() %>% 
  select(
    PID,
    TAS_total,
    TAS_sub_IdFeelings,
    TAS_sub_DescFeelings,
    TAS_sub_ExtThinking,
    TAS_n_missing
  ) 

#### COMBINE AND SAVE DATA ####

####. communication data ####

full_join(
  right_join(live_data_indep, live_data_comm), 
  full_join(online_data_indep, online_data_comm)
  ) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "comm-data.csv")

####. pleasantness data ####

full_join(right_join(live_data_indep, live_data_pleas), 
  full_join(online_data_indep, online_data_pleas)
 ) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "pleas-data.csv")

####. individual data (demographics and questionnaires) ####

full_join(live_data_indep, online_data_indep)
full_join(live_data_demog, online_data_demog)

full_join(online_data_aq, live_data_aq)


data_indep %>% 
  full_join(data_qualtrics) %>% 
  full_join(data_demog) %>% 
  full_join(data_aq) %>% 
  full_join(data_bapq) %>% 
  full_join(data_stq) %>% 
  full_join(data_tas) %>% 
  write_path_csv(PROCESSED_DATA_FOLDER, "online_indiv-data.csv")

