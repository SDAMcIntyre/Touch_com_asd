library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(stringr)
# source all .R files in the Rfunctions directory
sapply(list.files("Rfunctions", full.names = TRUE), source)

# input path ####
PROCESSED_DATA_FOLDER <- "Data/processed/"

# output paths ####
FIGURES_FOLDER <- "Figures/"
if ( !dir.exists(FIGURES_FOLDER) ) { dir.create(FIGURES_FOLDER) }

# read group performance metrics by touch label ####

load(paste0(PROCESSED_DATA_FOLDER, 'comm_metrics-group.RData'))

comm_data <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'communication-data.csv'),
  col_types = cols()
)

f1_data <- comm_metrics %>% 
  filter(Metric == "F1") %>% 
  rename(F1 = statistic) %>% 
  select(-Metric) %>% 
  mutate(Label = factor(Label, levels = ORDERED_CUES))

# felt touch ####

plot_f1_felt <- f1_data %>% 
  filter(experiment == "felt touch") %>% 
  mutate(group = recode(
    group,
    ASD = paste0('ASD (n = 34)'),
    Control = paste0('Control (n = 34)') 
  )) %>% 
  ggplot(mapping = aes(x = Label, y = F1, colour = group, fill = group)) +
  geom_hline(
    yintercept = f1_data$F1_chance[1], 
    colour = HLINE_COLOUR, linetype = HLINE_LINETYPE, size = HLINE_SIZE
  ) +
  geom_point(position = position_dodge(0.5), size = 2) +
  geom_errorbar(
    mapping = aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(0.5), width = 0, size = 1
    ) +
  scale_y_continuous(limits = c(0,1)) +
  theme_light(base_size = 14) +
  felt_only_scales + x_touch_labels + theme_x45deg +
  theme_insidelegend(0.8, 0.9) +
  labs(x = NULL, y = "Message agreement (F1)") +
  annotate("text", x = 1.5, y = 0.25, label = 'italic(chance)', parse = TRUE, colour = 'darkgrey')

conf_asd_felt <- comm_data %>%
  filter(experiment == "felt touch" & group == 'ASD') %>% 
  confusion_matrix_data() %>%
  confusion_matrix_plot(COLOUR_ASD_FELT, ylabels = c(ORDERED_CUES,'other')) 

conf_ctl_felt <- comm_data %>%
  filter(experiment == "felt touch" & group == 'Control') %>% 
  confusion_matrix_data() %>%
  confusion_matrix_plot(COLOUR_CONTROL_FELT, ylabels = c(ORDERED_CUES,'other')) 


open_plot_window()

design.compare = '
AAAB
AAAC
'

plot_f1_felt + conf_ctl_felt + conf_asd_felt +
  plot_annotation(tag_levels = 'A') +
  plot_layout(design = design.compare) 

ggsave(paste0(FIGURES_FOLDER,"Agreement_by_Label_felt.svg"), width = 8.4, height = 4.7)


# viewed touch ####

plot_f1_viewed <- f1_data %>% 
  filter(experiment == "viewed touch") %>% 
  mutate(group = recode(
    group,
    ASD = paste0('ASD (n = 19)'),
    Control = paste0('Control (n = 73)') 
  )) %>% 
  ggplot(mapping = aes(x = Label, y = F1, colour = group, fill = group)) +
  geom_hline(
    yintercept = f1_data$F1_chance[1], 
    colour = HLINE_COLOUR, linetype = HLINE_LINETYPE, size = HLINE_SIZE
  ) +
  geom_point(position = position_dodge(0.5), size = 2) +
  geom_errorbar(
    mapping = aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(0.5), width = 0, size = 1
  ) +
  scale_y_continuous(limits = c(0,1)) +
  theme_light(base_size = 14) +
  viewed_only_scales + x_touch_labels + theme_x45deg +
  theme_insidelegend(0.8, 0.9) +
  labs(x = NULL, y = "Message agreement (F1)") +
  annotate("text", x = 1.5, y = 0.25, label = 'italic(chance)', parse = TRUE, colour = 'darkgrey')

conf_asd_viewed <- comm_data %>%
  filter(task == "forced choice" & experiment == "viewed touch" & group == 'ASD') %>% 
  confusion_matrix_data() %>%
  confusion_matrix_plot(COLOUR_ASD_VIEWED, ylabels = c(ORDERED_CUES,'other')) 

conf_ctl_viewed <- comm_data %>%
  filter(task == "forced choice" & experiment == "viewed touch" & group == 'Control') %>% 
  confusion_matrix_data() %>%
  confusion_matrix_plot(COLOUR_CONTROL_VIEWED, ylabels = c(ORDERED_CUES,'other')) 


open_plot_window()

design.compare = '
AAAB
AAAC
'

plot_f1_viewed + conf_ctl_viewed + conf_asd_viewed +
  plot_annotation(tag_levels = 'A') +
  plot_layout(design = design.compare) 

ggsave(paste0(FIGURES_FOLDER,"Agreement_by_Label_viewed.svg"), width = 8.4, height = 4.7)


# sensitivity analyses:
# comm_metrics-group_strict-each
# comm_metrics-group_strict-compare
# comm_metrics-group_first6_strict