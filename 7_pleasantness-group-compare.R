library(readr)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(stringr)

# source all .R files in the Rfunctions directory ####
sapply(list.files("Rfunctions", full.names = TRUE), source)

# input path ####
PROCESSED_DATA_FOLDER <- "Data/processed/"

# output path ####
FIGURES_FOLDER <- "Figures/"
if ( !dir.exists(FIGURES_FOLDER) ) { dir.create(FIGURES_FOLDER) }

# read in the data ####
pleas_data <- read_csv(paste0(PROCESSED_DATA_FOLDER, "pleasantness-data.csv"), col_types = "cccccccd")

# Ns ####

Ns <- pleas_data %>% 
  group_by(experiment, group,PID) %>% 
  tally() %>% tally()

N_felt_ASD <- Ns %>% filter(experiment == "felt touch" & group == 'ASD') %>% pull(n)
N_felt_Control <- Ns %>% filter(experiment == "felt touch" & group == 'Control') %>% pull(n)
N_viewed_ASD <- Ns %>% filter(experiment == "viewed touch" & group == 'ASD') %>% pull(n)
N_viewed_Control <- Ns %>% filter(experiment == "viewed touch" & group == 'Control') %>% pull(n)

# Figures ####

open_plot_window(width = 9, height = 6.5); plot(1:10)

pleas_data %>%
  filter(experiment == "felt touch") %>% 
  mutate(group = recode(group, 
                        ASD = paste0('ASD (n = ',N_felt_ASD,')'), 
                        Control = paste0('Control (n = ',N_felt_Control,')') ) ) %>% 
  mutate(group = as.factor(group),
         cued = factor(cued, levels = ORDERED_CUES)) %>% 
  pleasantness_plot()

ggsave(paste0(FIGURES_FOLDER, 'felt_touch_pleasantness.svg'))
ggsave(paste0(FIGURES_FOLDER, 'felt_touch_pleasantness.pdf'))

open_plot_window(width = 9, height = 6.5); plot(1:10)

pleas_data %>%
  filter(experiment == "viewed touch") %>% 
  mutate(group = recode(group, 
                        ASD = paste0('ASD (n = ',N_viewed_ASD,')'), 
                        Control = paste0('Control (n = ',N_viewed_Control,')') ) ) %>% 
  mutate(group = as.factor(group),
         cued = factor(cued, levels = ORDERED_CUES)) %>% 
  pleasantness_plot()

ggsave(paste0(FIGURES_FOLDER, 'viewed_touch_pleasantness.svg'))
ggsave(paste0(FIGURES_FOLDER, 'viewed_touch_pleasantness.pdf'))
  