library(tidyverse)
library(summarytools)

# source all .R files in the Rfunctions directory
sapply(list.files("Rfunctions", full.names = TRUE), source)

# folder paths ####
PROCESSED_DATA_FOLDER <- "Data/processed/"
TABLES_FOLDER <- "Tables/"
FIGURES_FOLDER <- "Figures/"
REPORTS_FOLDER <- "Data/reports/"

# read data ####
demog <- read_csv(paste0(PROCESSED_DATA_FOLDER, "demographics-data.csv"), col_types = "cccccccccc") 

# check that qualtrics assigned even numbers to different tasks ####

print_prop_xtab(xtabs(~ group + task, filter(demog, experiment == "viewed touch")))
# a bit uneven, perhaps due to dropouts for the free text task


# forced choice task ####

demog_forced <- filter(demog, task == "forced choice")

print_prop_xtab(xtabs(~ group + Language + experiment, demog_forced))

print_prop_xtab(xtabs(~ group + `Country of Residence` + experiment, demog_forced))

print_prop_xtab(xtabs(~ group + Gender + experiment, demog_forced))

print_prop_xtab(xtabs(~ group + `Age Group` + experiment, demog_forced))

#. summary table ####
demog_forced %>% 
  group_by(experiment, group) %>%
  select(-c(PID, task, `Age Cohort`)) %>% 
  freq_table() %>% 
  view(file = paste0(TABLES_FOLDER,"table1_demographics_forced-choice.html"))

#. subset for comparable demographics ####

#.. felt touch subset ####
  
# Swedish-speaking, only experimenters 1 & 3
demog_felt_strict <- filter(
  demog_forced,
  experiment == "felt touch" &
    Language == "Swedish" & 
    Experimenter != "2"
  )

demog_felt_strict %>% 
  group_by(group) %>%
  select(-c(PID, task, experiment, `Age Cohort`)) %>% 
  freq_table() %>% 
  view(file = paste0(TABLES_FOLDER,"tableS1_demographics_felt-touch_strict.html"))

#.. viewed touch subset ####

demog_viewed_reduced <- filter(
  demog_forced, 
  experiment == "viewed touch" &
    `Country of Residence` == "Sweden" &
    Language == "Swedish",
  Gender == "Female"
)

# viewed touch select control group
# numbers matched to ASD group
# 1 from 21-25
# 4 from 26-30
# 4 from 31-35
# 5 from 36-40
# https://stackoverflow.com/questions/66476142/sample-from-a-data-frame-using-group-specific-sample-sizes

set.seed(09032023)
demog_viewed_strict_control <- demog_viewed_reduced %>% 
  filter(group == "Control" & `Age Group` != "16 - 20") %>% 
  group_split(`Age Group`) %>% 
  map2_dfr(c(1,4,4,5), ~ slice_sample(.x, n = .y))

demog_viewed_strict <- full_join(
  filter(demog_viewed_reduced, group == "ASD"),
  demog_viewed_strict_control
)

print_prop_xtab(xtabs(~ group + `Age Group` + experiment, demog_viewed_strict))

demog_viewed_strict %>% 
  group_by(group) %>%
  select(-c(PID, task, experiment, Experimenter, `Age Cohort`)) %>% 
  freq_table() %>% 
  view(file = paste0(TABLES_FOLDER,"tableS2_demographics_viewed-touch_strict.html"))


#.. felt vs. seen touch subset ####

demog_forced_reduced <- filter(
  demog_forced,
  Gender == "Female"
)

print_prop_xtab(xtabs(~ experiment + `Age Group` + group, demog_forced_reduced))


# forced choice select ASD felt touch group
# reduce 16-20 to more closely resemble ASD viewed touch group
# 7 from 16-20
# 5 from 21-25
# 3 from 26-30
# 1 from 31-35
# 0 from 36-40

set.seed(24032023)
demog_forced_strict_felt_asd <- demog_forced_reduced %>% 
  filter(experiment == "felt touch", group == "ASD" & `Age Group` != "36 - 40") %>% 
  group_split(`Age Group`) %>% 
  map2_dfr(c(7,5,3,1), ~ slice_sample(.x, n = .y))

# forced choice select control viewed touch group
# select to match age deomgraphics of felt touch control group
# 3 from 16-20
# 13 from 21-25
# 2 from 26-30
# 1 from 31-35
# 2 from 36-40

set.seed(24032023)
demog_forced_strict_viewed_control <- demog_forced_reduced %>% 
  filter(experiment == "viewed touch", group == "Control") %>% 
  group_split(`Age Group`) %>% 
  map2_dfr(c(3,13,2,1,2), ~ slice_sample(.x, n = .y))

demog_forced_strict <- full_join(
  filter(demog_forced_reduced, experiment == "felt touch" & group == "Control"),
  filter(demog_forced_reduced, experiment == "viewed touch" & group == "ASD")
  ) %>%
  full_join(demog_forced_strict_felt_asd) %>% 
  full_join(demog_forced_strict_viewed_control)


print_prop_xtab(xtabs(~ experiment + `Age Group` + group, demog_forced_strict))

demog_forced_strict %>% 
  group_by(group, experiment) %>%
  select(-c(PID, task, Experimenter, `Age Cohort`)) %>% 
  freq_table() %>% 
  view(file = paste0(TABLES_FOLDER,"tableS3_demographics_felt-vs-viewed_strict.html"))


#.. save PIDs for strict subsets ####

demog %>% 
  select(PID) %>% 
  mutate(
    felt_touch = PID %in% demog_felt_strict$PID,
    viewed_touch = PID %in% demog_viewed_strict$PID,
    felt_vs_viewed = PID %in% demog_forced_strict$PID
  ) %>% 
  write_csv(paste0(PROCESSED_DATA_FOLDER, "PIDs-included-in-strict-subsets.csv"))

# free text task ####

demog_free <- filter(demog, task == "free text")

print_prop_xtab(xtabs(~ group + Language, demog_free))

print_prop_xtab(xtabs(~ group + `Country of Residence`, demog_free))

print_prop_xtab(xtabs(~ group + Gender, demog_free))

print_prop_xtab(xtabs(~ group + `Age Group`, demog_free))

#. summary table ####
demog_free %>% 
  group_by(group) %>%
  select(-c(PID, Experimenter, experiment, task, `Age Cohort`)) %>% 
  freq_table() %>% 
  view(file = paste0(TABLES_FOLDER,"tableS4_demographics_free-text.html"))


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


