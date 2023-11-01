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

# read group performance metrics by touch label ####

comm_metrics <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'comm_metrics-group.csv'), 
  col_types = cols()
) 

# overall performance
comm_data %>% 
  f1_micro_boot_dataset("cued", "response", ORDERED_CUES, R = 1000, experiment, group)

# performance by label 
f1_data <- comm_metrics %>% 
  filter(Metric == "F1") %>% 
  rename(F1 = statistic) %>% 
  select(-Metric) 

f1_data %>% 
  ggplot(mapping = aes(x = Label, y = F1, colour = group)) +
  facet_grid(experiment ~ .) +
  geom_point() +
  geom_errorbar(mapping = aes(ymin = conf.low, ymax = conf.high))

# sensitivity analyses:
# comm_metrics-group_strict-each
# comm_metrics-group_strict-compare
# comm_metrics-group_first6_strict