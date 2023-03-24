library(tidyverse)
library(summarytools)

# source all .R files in the Rfunctions directory
sapply(list.files("Rfunctions", full.names = TRUE), source)

# read data ####
demog <- read_csv("Data/processed/demographics-data.csv", col_types = "cccccccccc") 

# output paths ####
TABLES_FOLDER <- "Tables/"
FIGURES_FOLDER <- "Figures/"
REPORTS_FOLDER <- "Data/reports/"


# check that qualtrics assigned even numbers to different tasks ####

print_prop_xtab(xtabs(~ group + task, filter(demog, experiment == "viewed touch")))
# a bit uneven, perhaps due to dropouts for the free text task


# forced choice task ####

demog_fc <- filter(demog, task == "forced choice")

print_prop_xtab(xtabs(~ group + Language + experiment, demog_fc))

print_prop_xtab(xtabs(~ group + `Country of Residence` + experiment, demog_fc))

print_prop_xtab(xtabs(~ group + Gender + experiment, demog_fc))

print_prop_xtab(xtabs(~ group + `Age Group` + experiment, demog_fc))

#. summary table ####
demog_fc %>% 
  group_by(experiment, group) %>%
  select(-c(PID, task, `Age Cohort`)) %>% 
  freq_table() %>% 
  view(file = paste0(TABLES_FOLDER,"table1_demographics_forced-choice.html"))

#. subset for comparable demographics ####
  
  # should do separate subsets, 
    # one for comparing ASD vs. control
    # another for comparing felt vs. seen touch

#.. felt touch subset ####
  
# Swedish-speaking, only experimenters 1 & 3
demog_ft_strict <- filter(
  demog_fc,
  experiment == "felt touch" &
    Language == "Swedish" & 
    Experimenter != "2"
  )

demog_ft_strict %>% 
  group_by(group) %>%
  select(-c(PID, task, experiment, `Age Cohort`)) %>% 
  freq_table() %>% 
  view(file = paste0(TABLES_FOLDER,"tableS1_demographics_felt-touch_strict.html"))


#.. viewed touch subset ####

demog_vt_reduced <- filter(
  demog_fc, 
  experiment == "viewed touch" &
    `Country of Residence` == "Sweden" &
    Language == "Swedish",
  Gender == "Female"
)

set.seed(09032023)

# viewed touch select control group
# 1 from 21-25
# 4 from 26-30
# 4 from 31-35
# 5 from 36-40
# https://stackoverflow.com/questions/66476142/sample-from-a-data-frame-using-group-specific-sample-sizes

demog_vt_strict_control <- demog_vt_reduced %>% 
  filter(group == "Control" & `Age Group` != "16 - 20") %>% 
  group_split(`Age Group`) %>% 
  map2_dfr(c(1,4,4,5), ~ slice_sample(.x, n = .y))

demog_vt_strict <- full_join(
  filter(demog_vt_reduced, group == "ASD"),
  demog_vt_strict_control
)

print_prop_xtab(xtabs(~ group + `Age Group` + experiment, demog_vt_strict))

demog_vt_strict %>% 
  group_by(group) %>%
  select(-c(PID, task, experiment, Experimenter, `Age Cohort`)) %>% 
  freq_table() %>% view
view(file = paste0(TABLES_FOLDER,"tableS2_demographics_viewed-touch_strict.html"))


#. subset for comparing felt vs. seen touch BOOKMARK


#### missing questionnaire responses ####

demog %>% 
  group_by(group,AQ_n_missing) %>% tally()

demog %>% 
  group_by(group,BAPQ_n_missing) %>% tally()

demog %>% 
  group_by(group,STQ_n_missing) %>% tally()

demog %>% 
  group_by(group, TAS_n_missing) %>% tally()

demog %>% 
  select(PID, group,Language, `Country of Residence`, contains("n_missing")) %>% 
  rowwise() %>% 
  mutate(total_n_missing = sum(c_across(contains("n_missing")))) %>% 
  filter(total_n_missing > 0) %>%
  select(-total_n_missing) %>% 
  arrange(group, -across(contains("n_missing"))) %>% 
  write_path_csv("Data/reports/", "online_missing_questionnaire_responses.csv")


