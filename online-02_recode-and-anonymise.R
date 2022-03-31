library(tidyverse)
library(summarytools)

#### functions ####

to_regex <- function(x) {
  paste0('(', paste(x, collapse = ')|('), ')')
}

extract_touches <- function(x) {
  x_labels <- c("ATTENTION", "CALMING", "GRATITUDE", "HAPPINESS", "LOVE", "SADNESS", "something else")
  x_regex <- to_regex(x_labels)
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

sum_AQ <- function(x, reversed = FALSE) {
  recoded_scores <- case_when(
    x == "definitely agree" ~ 1,
    x == "slightly agree" ~ 1,
    x == "slightly disagree" ~ 0,
    x == "definitely disagree" ~ 0
  )
  
  if (reversed) {
    return(sum(1-recoded_scores))
  } else {
    return(sum(recoded_scores))
  }
}

sum_BAPQ <- function(x, reversed = FALSE) {
  recoded_scores <- case_when(
    x == "Very rarely" ~ 1,
    x == "Rarely" ~ 2,
    x == "Occasionally" ~ 3,
    x == "Somewhat often" ~ 4,
    x == "Often" ~ 5,
    x == "Very often" ~ 6
  )
  
  if (reversed) {
    return(sum(7-recoded_scores))
  } else {
    return(sum(recoded_scores))
  }  
}

sum_STQ <- function(x, reversed = FALSE) {
  recoded_scores <- case_when(
    x == "not at all" ~ 0,
    x == "slightly" ~ 1,
    x == "moderately" ~ 2,
    x == "very" ~ 3,
    x == "extremely" ~ 4
  )
  
  if (reversed) {
    return(sum(6-recoded_scores))
  } else {
    return(sum(recoded_scores))
  }
}

to_regex_AQ <- function(x) {
  to_regex(paste0("^AQ_",x,"$"))
}

to_regex_BAPQ <- function(x) {
  to_regex(paste0("^BAPQ_",x,"$"))
}

qvars_to_regex <- function(x, q_prefix) {
  to_regex(paste0(
    "^", q_prefix,
    "_",
    x, "$"
  ))
}

#### global variables ####

AQ_VARS_REGULAR <- c(2,4,5,6,7,9,12,13,16,18,19,20,21,22,23,26,33,35,39,41,42,43,45,46)
AQ_VARS_REVERSED <- c(1,3,8,10,11,14,15,17,24,25,27,28,29,30,31,32,34,36,37,38,40,44,47,48,49,50)

BAPQ_VARS_REVERSED <- c(1, 3, 7, 9, 12, 15, 16, 19, 21, 23, 25, 28, 30, 34, 36)
BAPQ_VARS_REGULAR <- setdiff(1:36, BAPQ_VARS_REVERSED)
BAPQ_VARS_ALOOF <- c(1, 5, 9, 12, 16, 18, 23, 25, 27, 28, 31, 36)
BAPQ_VARS_PRAGLANG <- c(2, 4, 7, 10, 11, 14, 17, 20, 21, 29, 32, 34)
BAPQ_VARS_RIGID <- c(3, 6, 8, 13, 15, 19, 22, 24, 26, 30, 33, 35)

STQ_VARS_REVERSED <- c(1,4,6,9,11,12,14,15,18,20)
STQ_VARS_REGULAR  <- setdiff(1:20, STQ_VARS_REVERSED)

#### read in data ####
valid_data <- read_csv("Data/private/online_valid-data.csv") 

#### recode data   #### 

####. survey variables   #### 
recoded_data_survey <- valid_data %>% 
  
  # remove screening variables (except ASD)
  select(-c(
    `Response Type`,
    Progress,
    `Distribution Channel`,
    Finished,
    starts_with("Screen")
  )) %>% 
  
  # add minutes/hours duration data 
  mutate(
    `Duration (in minutes)` = `Duration (in seconds)`/60,
    `Duration (in hours)` = `Duration (in seconds)`/(60*60)
  )

####. demographics variables   #### 
recoded_data_demog <- recoded_data_survey %>% 
  
  # nice language response labels
  mutate(
    `User Language` = case_when(
      `User Language` == "EN" ~ "English",
      `User Language` == "SV" ~ "Swedish"
      )
    ) %>% 
  
  # nice country of residence names
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
  
  # collapse age into bins
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
  select(-c(Age)) # remove exact age for improved privacy

####. main independent variables   #### 
recoded_data_indep <- recoded_data_demog %>% 
  
  # make group variable from ASD question
  mutate(
    group = case_when(
      ASD == "Yes" ~ "ASD",
      ASD == "No" ~ "Control"
    ) 
  ) %>% 
  select(-c(ASD)) %>% 
  
  # make task variable for which comm task they were assigned
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
  
  # make single variable for display order (qualtrics gives separate variables for group and Task
  mutate(
    `Communication DO` = coalesce(
      `CommunicationFC DO ASD`,
      `CommunicationFT DO ASD`,
      `CommunicationFC DO Control`,
      `CommunicationFT DO Control`
      )
    ) %>% 
  select(-matches("Communication.{2} DO .+")) 

####. main dependent variables ####

####.. communication forced choice responses ####
recoded_data_comm <- recoded_data_indep %>% 
  mutate(across(
    .cols = matches("CommunicationFC .+") & !ends_with('DO'),
    .fns = extract_touches
    )) 

####.. AQ ####

recoded_data_aq <- recoded_data_comm %>% 
  rowwise() %>% 
  mutate(
    AQ_n_missing = sum(is.na(c_across(starts_with("AQ_")))),
    
    AQ_total = sum_AQ(
      c_across(matches(to_regex_AQ(AQ_VARS_REGULAR))),
      reversed = FALSE
    ) +
      sum_AQ(
        c_across(matches(to_regex_AQ(AQ_VARS_REVERSED))),
        reversed = TRUE
      )
  ) %>% 
  select(-matches("^AQ_[0-9]+$")) # remove raw AQ responses for added privacy

# check missing responses
recoded_data_aq %>% 
  group_by(group,AQ_n_missing) %>% tally()

####.. BAPQ ####

recoded_data_bapq <- recoded_data_aq %>% 
  rowwise() %>% 
  mutate(
    BAPQ_n_missing = sum(is.na(c_across(starts_with("BAPQ_")))),
    
    # total
    BAPQ_total = sum_BAPQ(
      c_across(
        matches(to_regex_BAPQ(BAPQ_VARS_REGULAR))
        ),
      reversed = FALSE
    ) +
      sum_BAPQ(
        c_across(
          matches(to_regex_BAPQ(BAPQ_VARS_REVERSED))
          ),
        reversed = TRUE
      ),
    
    # subscale aloof
    BAPQ_sub_Aloof = sum_BAPQ(
      c_across(
        matches(to_regex_BAPQ(BAPQ_VARS_ALOOF)) &
          matches(to_regex_BAPQ(BAPQ_VARS_REGULAR))
      ),
      reversed = FALSE
    ) +
      sum_BAPQ(
        c_across(
          matches(to_regex_BAPQ(BAPQ_VARS_ALOOF)) &
            matches(to_regex_BAPQ(BAPQ_VARS_REVERSED))
        ),
        reversed = TRUE
      ),
    
    #subscale pragmatic language
    BAPQ_sub_PragLang = sum_BAPQ(
      c_across(
        matches(to_regex_BAPQ(BAPQ_VARS_PRAGLANG)) &
          matches(to_regex_BAPQ(BAPQ_VARS_REGULAR))
      ),
      reversed = FALSE
    ) +
      sum_BAPQ(
        c_across(
          matches(to_regex_BAPQ(BAPQ_VARS_PRAGLANG)) &
            matches(to_regex_BAPQ(BAPQ_VARS_REVERSED))
        ),
        reversed = TRUE
      ),
    
    # subscale rigid
    BAPQ_sub_Rigid = sum_BAPQ(
      c_across(
        matches(to_regex_BAPQ(BAPQ_VARS_RIGID)) &
          matches(to_regex_BAPQ(BAPQ_VARS_REGULAR))
      ),
      reversed = FALSE
    ) +
      sum_BAPQ(
        c_across(
          matches(to_regex_BAPQ(BAPQ_VARS_RIGID)) &
            matches(to_regex_BAPQ(BAPQ_VARS_REVERSED))
        ),
        reversed = TRUE
      )          
  ) %>% 
  select(-matches("^BAPQ_[0-9]+$")) # remove raw BAPQ responses for added privacy


####.. STQ ####

recoded_data_stq <- recoded_data_bapq %>% 
  rowwise() %>% 
  mutate(
    STQ_n_missing = sum(is.na(c_across(starts_with("STQ")))),
    
    STQ_total = sum_STQ(
      c_across(matches(qvars_to_regex(STQ_VARS_REGULAR, "STQ"))),
      reversed = FALSE
    ) +
      sum_STQ(
        c_across(matches(qvars_to_regex(STQ_VARS_REVERSED, "STQ"))),
        reversed = TRUE
      )
  ) %>% 
  select(-matches("^STQ_[0-9]+$")) # remove raw STQ responses for added privacy

# check missing responses
recoded_data_stq %>% 
  group_by(group,STQ_n_missing) %>% tally()


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
