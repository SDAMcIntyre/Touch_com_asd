library(tidyverse)
library(summarytools)

# source all .R files in the Rfunctions directory
sapply(list.files("Rfunctions", full.names = TRUE), source)

#### read data ####
ind_data <- read_csv("Data/primary/online_individual_recoded.csv")

#### output folders ####
figure_folder <- "Figures/"

#### missing questionnaire responses ####

ind_data %>% 
  group_by(group,AQ_n_missing) %>% tally()

ind_data %>% 
  group_by(group,BAPQ_n_missing) %>% tally()

ind_data %>% 
  group_by(group,STQ_n_missing) %>% tally()

ind_data %>% 
  group_by(group, TAS_n_missing) %>% tally()

ind_data %>% 
  select(PID, group,Language, `Country of Residence`, contains("n_missing")) %>% 
  rowwise() %>% 
  mutate(total_n_missing = sum(c_across(contains("n_missing")))) %>% 
  filter(total_n_missing > 0) %>%
  select(-total_n_missing) %>% 
  arrange(group, -across(contains("n_missing"))) %>% 
  write_path_csv("Data/reports/", "online_missing_questionnaire_responses.csv")

####  survey date / time data  #### 

min(ind_data$`Start Date`)
min(ind_data$`Recorded Date`)

max(ind_data$`Start Date`)
max(ind_data$`Recorded Date`)

ind_data %>% 
  filter(group == "ASD") %>% 
  pull(`Recorded Date`) %>% max()

ind_data %>% 
  mutate(`End Difference` = `Recorded Date` - `End Date`) %>% 
  ggplot(aes(x = `End Difference`)) + geom_histogram()

open_plot_window(width = 4.6, height = 3.9)

ind_data %>% 
  ggplot(aes(x = `Recorded Date`)) + 
  facet_wrap(~ group, ncol = 1) +
  geom_histogram() +
  labs(x = 'Completion Date') +
  theme_bw() 

ggsave_path(figure_folder,"online_completion-date.png")

ind_data  %>% 
  filter(`Duration (minutes)` <= 60) %>% 
  ggplot(aes(x = `Duration (minutes)`)) + geom_histogram()

ind_data %>% 
  filter(`Duration (minutes)` > 60) %>% 
  ggplot(aes(x = `Duration (hours)`)) + geom_histogram()

####  demographics  #### 

open_plot_window()

ind_data %>% 
  ggplot() +
  facet_wrap(group ~ task) +
  geom_bar(
    aes(x = `Age Group`, fill = Gender),
    stat = "count",
    alpha = 0.7,
    colour = 'black'
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

ggsave_path(figure_folder,"online_demographics-1.pdf")

ctable(
  x = ind_data$group,
  y = ind_data$task
) %>% view

ctable(
  x = ind_data$group,
  y = ind_data$Language
) %>% view

ctable(
  x = ind_data$task,
  y = ind_data$Language
) %>% view

ctable(
  x = ind_data$group,
  y = ind_data$`Country of Residence`
) %>% view

ctable(
  x = ind_data$task,
  y = ind_data$`Country of Residence`
) %>% view

ctable(
  x = ind_data$group,
  y = ind_data$Gender
) %>% view

ctable(
  x = ind_data$task,
  y = ind_data$Gender
) %>% view

