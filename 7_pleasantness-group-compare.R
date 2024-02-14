library(readr)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(stringr)
library(patchwork)

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



# 
# group_mode_levels <- c(
#   paste0('ASD, felt (n = ',N_felt_ASD,')'),
#   paste0('Control, felt (n = ',N_felt_Control,')'),
#   paste0('ASD, viewed (n = ',N_viewed_ASD,')'),
#   paste0('Control, viewed (n = ',N_viewed_Control,')')
#          )

plot_pleas_felt <- pleas_data %>%
  filter(experiment == "felt touch") %>%
  mutate(
    # group_mode = case_when(
    #   group == "ASD" & experiment == "felt touch" ~ group_mode_levels[1],
    #   group == "Control" & experiment == "felt touch" ~ group_mode_levels[2],
    #   group == "ASD" & experiment == "viewed touch" ~ group_mode_levels[3],
    #   group == "Control" & experiment == "viewed touch" ~ group_mode_levels[4]
    # )
    group = recode(
      group,
      ASD = paste0('ASD (n = ',N_felt_ASD,')'),
      Control = paste0('Control (n = ',N_felt_Control,')')
    )
  ) %>% 
  mutate(
    group = as.factor(group),
    # group_mode = factor(group_mode, levels = group_mode_levels),
    cued = factor(cued, levels = ORDERED_CUES)
    ) %>% 
  ggplot(aes(y = response, x = cued, colour = group, fill = group)) +
  geom_hline(yintercept = 0, 
             colour = HLINE_COLOUR, linetype = HLINE_LINETYPE, size = HLINE_SIZE) +
  stat_summary(geom = 'errorbar', fun.data = 'mean_cl_boot', 
               width = ERRORBAR_WIDTH, size = ERRORBAR_SIZE,
               position = DODGE_WIDTH) +
  stat_summary(geom = "point", fun= "mean",
               size = POINT_SIZE, 
               position = DODGE_WIDTH) +
  theme_light(base_size = 14) +
  felt_only_scales + x_touch_labels + theme_x45deg +
  theme_insidelegend(0.7, 0.15) +
  scale_y_continuous(
    breaks = c(-10,0,10), 
    limits = c(-10,10), 
    labels = c('unp','','ple')
  ) +
  labs(title = "Felt touch", y = 'Pleasantness rating (VAS)', x = NULL) 

plot_pleas_viewed <- pleas_data %>%
  filter(experiment == "viewed touch") %>%
  mutate(
    group = recode(
      group,
      ASD = paste0('ASD (n = ',N_viewed_ASD,')'),
      Control = paste0('Control (n = ',N_viewed_Control,')')
    )
  ) %>% 
  mutate(
    group = as.factor(group),
    # group_mode = factor(group_mode, levels = group_mode_levels),
    cued = factor(cued, levels = ORDERED_CUES)
  ) %>% 
  ggplot(aes(y = response, x = cued, colour = group, fill = group)) +
  geom_hline(yintercept = 0, 
             colour = HLINE_COLOUR, linetype = HLINE_LINETYPE, size = HLINE_SIZE) +
  stat_summary(geom = 'errorbar', fun.data = 'mean_cl_boot', 
               width = ERRORBAR_WIDTH, size = ERRORBAR_SIZE,
               position = DODGE_WIDTH) +
  stat_summary(geom = "point", fun= "mean",
               size = POINT_SIZE, 
               position = DODGE_WIDTH) +
  theme_light(base_size = 14) +
  viewed_only_scales + x_touch_labels + theme_x45deg +
  theme_insidelegend(0.7, 0.15) +
  scale_y_continuous(
    breaks = c(-10,0,10), 
    limits = c(-10,10), 
    labels = c('unp','','ple')
  ) +
  labs(title = "Viewed touch", y = NULL, x = NULL) 

open_plot_window()

plot_pleas_felt + plot_pleas_viewed

ggsave(paste0(FIGURES_FOLDER, 'Pleasantness_by_Label.svg'), width = 7.9, height = 3.6)


  