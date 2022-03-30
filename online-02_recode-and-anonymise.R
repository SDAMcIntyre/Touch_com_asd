library(tidyverse)
library(summarytools)

#### functions ####
extract_touches <- function(x) {
  x_labels <- c("ATTENTION", "CALMING", "GRATITUDE", "HAPPINESS", "LOVE", "SADNESS", "something else")
  x_regex <- paste0('(', paste(x_labels, collapse = ')|('), ')')
  extracted <- str_extract(x, x_regex)
  output <- str_replace(
    extracted, 
    "something else", 
    "other"
  )
  tolower(output)
}

open_plot_window <- function(width = 7, height = 7, ...) {
  if (.Platform$OS.type == "unix") {
    quartz(width = width, height = height); plot(1:10)
  }  else { windows(width = width, height = height) }
}

#### read in data ####
valid_data <- read_csv("Data/private/online_valid-data.csv") 

#### recode data   #### 

recoded_data <- valid_data %>% 
  
  ####. remove variables used for screening (except ASD) ####
  select(-c(
    `Response Type`,
    Progress,
    `Distribution Channel`,
    Finished,
    starts_with("Screen")
  )) %>% 
  
  ####. add minutes/hours duration data   #### 
  mutate(
    `Duration (in minutes)` = `Duration (in seconds)`/60,
    `Duration (in hours)` = `Duration (in seconds)`/(60*60)
  ) %>% 
  
  ####. nice language response labels  #### 
  mutate(
    `User Language` = case_when(
      `User Language` == "EN" ~ "English",
      `User Language` == "SV" ~ "Swedish"
      )
    ) %>% 
  
  ####. nice country of residence names  #### 
  mutate(
    `Country of Residence` = coalesce(
      `Country of Residence (EN)`, 
      `Country of Residence (SV)`
      )
    ) %>% 
  select(-c(`Country of Residence (EN)`, `Country of Residence (SV)`)) %>% 
  mutate(
    `Country of Residence` = case_when(
      `Country of Residence` == "Sverige" ~ "Sweden",
      `Country of Residence` == "United Kingdom of Great Britain and Northern Ireland" ~ "UK",
      `Country of Residence` == "United States of America" ~ "USA",
      TRUE ~ `Country of Residence`
      )
    ) %>% 
  
  ####. make group variable from ASD question  #### 
  mutate(
    group = case_when(
      ASD == "Yes" ~ "ASD",
      ASD == "No" ~ "Control"
    ) 
  ) %>% 
  select(-c(ASD)) %>% 
  
  ####. make task variable for which comm task they were assigned  #### 
  mutate(
    `Communication Task` = coalesce(
      `Communication Condition ASD`,
      `Communication Condition Control`
      )
    ) %>% 
  select(-starts_with("Communication Condition")) %>% 
  mutate(
    `Communication Task` = case_when(
      str_detect(`Communication Task`, "FL_(40|29)") ~ "Forced Choice", 
      str_detect(`Communication Task`, "FL_(36|30)") ~ "Free Text"
      )
    ) %>% 
  
  ####. make single variable for display order (qualtrics gives separate variables for group and Task)  #### 
  mutate(
    `Communication Display Order` = coalesce(
      `CommunicationFC DO ASD`,
      `CommunicationFT DO ASD`,
      `CommunicationFC DO Control`,
      `CommunicationFT DO Control`
      )
    ) %>% 
  select(-matches("Communication.{2} DO .+")) %>% 
  
  ####. collapse age into bins   #### 
  mutate(
    `Age Group` = case_when(
      between(Age, 16, 20) ~ '16 - 20',
      between(Age, 21, 25) ~ '20 - 25',
      between(Age, 26, 30) ~ '26 - 30',
      between(Age, 31, 35) ~ '31 - 35',
      between(Age, 36, 40) ~ '36 - 40'
    ),
    `Age Cohort` = case_when(
      between(Age, 16, 17) ~ "Youth (16 - 17)",
      between(Age, 18, 40) ~ "Adult (18 - 40)"
    )
      ) %>% 
  # select(-c(Age)) %>% # remove exact age for improved privacy

  ####. recode communication forced choice responses ####
  mutate(
    across(
      .cols = matches("CommunicationFC .+") & !ends_with('DO'),
      .fns = extract_touches
    )
  ) 

####  survey date / time data  #### 

min(recoded_data$`Start Date`)
min(recoded_data$`Recorded Date`)

max(recoded_data$`Start Date`)
max(recoded_data$`Recorded Date`)

recoded_data %>% 
  filter(group == "ASD") %>% 
  pull(`Recorded Date`) %>% max()

recoded_data %>% 
  mutate(`End Difference` = `Recorded Date` - `End Date`) %>% 
  ggplot(aes(x = `End Difference`)) + geom_histogram()

open_plot_window(width = 4.6, height = 3.9)

recoded_data %>% 
  ggplot(aes(x = `Recorded Date`)) + 
  facet_wrap(~ group, ncol = 1) +
  geom_histogram() +
  labs(x = 'Completion Date') +
  theme_bw() 

figure_folder <- "Figures/"
if (!dir.exists(figure_folder)) {dir.create(figure_folder)}
ggsave(paste0(figure_folder,"online_completion-date.png"))

recoded_data  %>% 
  filter(`Duration (in seconds)` <= 60*60) %>% 
  ggplot(aes(x = `Duration (in minutes)`)) + geom_histogram()

recoded_data %>% 
  filter(`Duration (in seconds)` > 60*60) %>% 
  ggplot(aes(x = `Duration (in hours)`)) + geom_histogram()

####  demographics  #### 

open_plot_window()

recoded_data %>% 
  ggplot() +
  facet_wrap(group ~ `Communication Task`) +
  geom_bar(
    aes(x = `Age Group`, fill = Gender),
    stat = "count",
    alpha = 0.7,
    colour = 'black'
    ) +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

figure_folder <- "Figures/"
if (!dir.exists(figure_folder)) {dir.create(figure_folder)}
ggsave(paste0(figure_folder,"online_demographics-1.pdf"))

ctable(
  x = recoded_data$group,
  y = recoded_data$`Communication Task`
) %>% view

ctable(
  x = recoded_data$group,
  y = recoded_data$`User Language`
) %>% view

ctable(
  x = recoded_data$`Communication Task`,
  y = recoded_data$`User Language`
) %>% view

ctable(
  x = recoded_data$group,
  y = recoded_data$`Country of Residence`
) %>% view

ctable(
  x = recoded_data$`Communication Task`,
  y = recoded_data$`Country of Residence`
) %>% view

ctable(
  x = recoded_data$group,
  y = recoded_data$Gender
) %>% view

ctable(
  x = recoded_data$`Communication Task`,
  y = recoded_data$Gender
) %>% view
