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

# read individual pleasantness metrics ####

pleasantness <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'pleasantness-data.csv'), 
  col_types = cols()
) 

# read questionnaire data ####

questionnaires <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'questionnaire-data.csv'),
  col_types = cols() 
) 


# correlations pleasantness vs. questionnaires ####

pleasantness_ave <- pleasantness %>% 
  group_by(experiment, group, PID) %>% 
  summarise(
    pleasantness_median = median(response, na.rm = TRUE)
  )

indiv_data <- full_join(pleasantness_ave, questionnaires)

indiv_data %>% 
  ggplot(mapping = aes(y = pleasantness_median, x = BAPQ_total)) +
  facet_grid(experiment ~ .) +
  geom_point(mapping = aes(colour = group)) +
  geom_smooth(method = "lm")


# sensitivity analyses:

#. read subsets for sensitivity analyses ####

strict_subsets <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, "PIDs-included-in-strict-subsets.csv"),
  col_types = cols()
)

#. strict dataset (similar sex and age demographics, N) ####

indiv_data_strict <- indiv_data %>% 
  filter(
    PID %in% filter(strict_subsets, felt_vs_viewed)[["PID"]]
  )

indiv_data_strict %>% 
  ggplot(mapping = aes(y = pleasantness_median, x = BAPQ_total)) +
  facet_grid(experiment ~ .) +
  geom_point(mapping = aes(colour = group)) +
  geom_smooth(method = "lm")

