library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
# source all .R files in the Rfunctions directory
sapply(list.files("Rfunctions", full.names = TRUE), source)

# input path ####
PROCESSED_DATA_FOLDER <- "Data/processed/"

# output paths ####
FIGURES_FOLDER <- "Figures/"
if ( !dir.exists(FIGURES_FOLDER) ) { dir.create(FIGURES_FOLDER) }

# read individual performance metrics ####

comm_F1micro <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'comm_F1micro-indiv.csv'), 
  col_types = cols()
) 

# read questionnaire data ####

questionnaires <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'questionnaire-data.csv'),
  col_types = cols() 
) 

# read group performance metrics by touch label ####

comm_metrics <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'comm_metrics-group.csv'), 
  col_types = cols()
) 


# correlations performance vs. questionnaires ####

indiv_data <- full_join(comm_F1micro, questionnaires)

indiv_data %>% 
  ggplot(mapping = aes(y = F1_micro, x = BAPQ_total)) +
  facet_grid(experiment ~ .) +
  geom_point(mapping = aes(colour = group)) +
  geom_smooth(method = "lm")


# sensitivity analyses:

#. read subsets for sensitivity analyses ####

comm_F1micro_first6 <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'comm_F1micro-indiv_first6.csv'), 
  col_types = cols()
) 

comm_F1micro_strict <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'comm_F1micro-indiv_strict.csv'), 
  col_types = cols()
)

comm_F1micro_first6_strict <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'comm_F1micro-indiv_first6_strict.csv'), 
  col_types = cols()
)

#. first 6 trials only ####

indiv_data_first6 <- full_join(comm_F1micro_first6, questionnaires)

indiv_data_first6 %>% 
  ggplot(mapping = aes(y = F1_micro, x = BAPQ_total)) +
  facet_grid(experiment ~ .) +
  geom_point(mapping = aes(colour = group)) +
  geom_smooth(method = "lm")

#. strict dataset (similar sex and age demographics, N) ####

indiv_data_strict <- full_join(comm_F1micro_strict, questionnaires)

indiv_data_strict %>% 
  ggplot(mapping = aes(y = F1_micro, x = BAPQ_total)) +
  facet_grid(experiment ~ .) +
  geom_point(mapping = aes(colour = group)) +
  geom_smooth(method = "lm")

#. first 6 trials in strict dataset ####

indiv_data_first6_strict <- full_join(comm_F1micro_first6_strict, questionnaires)

indiv_data_first6_strict %>% 
  ggplot(mapping = aes(y = F1_micro, x = BAPQ_total)) +
  facet_grid(experiment ~ .) +
  geom_point(mapping = aes(colour = group)) +
  geom_smooth(method = "lm")
