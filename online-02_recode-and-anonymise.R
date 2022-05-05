library(tidyverse)
library(summarytools)

# source all .R files in the Rfunctions directory
sapply(list.files("Rfunctions", full.names = TRUE), source)

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

TAS_VARS_REVERSED <- c(4, 5, 10, 18, 19)
TAS_VARS_REGULAR <- setdiff(1:20, TAS_VARS_REVERSED)
TAS_VARS_IDFEELINGS <- c(1, 3, 6, 7, 9, 13, 14)
TAS_VARS_DESCFEELINGS <- c(2, 4, 11, 12, 17)
TAS_VARS_EXTTHINKING <- c(5, 8, 10, 15, 16, 18, 19, 20)

#### read in data ####
valid_data <- read_csv("Data/private/online_valid-data.csv") 

#### recode data   #### 

####. qualtrics variables   #### 
recoded_data_qualtrics <- valid_data %>% 
  
  # add minutes/hours duration data 
  mutate(
    `Duration (minutes)` = `Duration (in seconds)`/60,
    `Duration (hours)` = `Duration (in seconds)`/(60*60)
  ) %>% 
  
  # keep qualtrics variables
  select(c(
    PID,
    `Start Date`,
    `End Date`,
    `Recorded Date`,
    `Duration (minutes)`,
    `Duration (hours)`
    ))

####. demographics variables   #### 
recoded_data_demog <- valid_data %>% 
  
  # nice language response labels
  mutate(
    Language = case_when(
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
    between(Age, 21, 25) ~ '21 - 25',
    between(Age, 26, 30) ~ '26 - 30',
    between(Age, 31, 35) ~ '31 - 35',
    between(Age, 36, 40) ~ '36 - 40'
  ),
  `Age Cohort` = case_when(
    between(Age, 16, 17) ~ "Youth (16 - 17)",
    between(Age, 18, 40) ~ "Adult (18 - 40)"
  )
) %>% 
  
  # keep demographics variables
  select(
    PID,
    Language,
    `Country of Residence`,
    `Age Group`,
    `Age Cohort`
  )

####. main independent variables   #### 
recoded_data_indep <- valid_data %>% 
  
  # make group variable from ASD question
  mutate(
    group = case_when(
      ASD == "Yes" ~ "ASD",
      ASD == "No" ~ "Control"
    ) 
  ) %>% 

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
      str_detect(`Communication Task`, "FL_(40|29)") ~ "forced choice", 
      str_detect(`Communication Task`, "FL_(36|30)") ~ "free text"
      )
    ) %>% 
  
  # keep independent variables
  select(c(
    PID,
    group,
    `Communication Task`
  ))


####. main dependent variables ####

####.. communication forced choice responses ####
recoded_data_comm <- valid_data %>% 
  
  # make single variable for display order (qualtrics gives separate variables for group and Task)
  mutate(
    `Communication DO` = coalesce(
      `CommunicationFC DO ASD`,
      `CommunicationFT DO ASD`,
      `CommunicationFC DO Control`,
      `CommunicationFT DO Control`
    )
  ) %>% 
  
  # re-name FC responses
  mutate(across(
    .cols = matches("CommunicationFC .+") & !contains('DO'),
    .fns = extract_touches
    )) %>% 
  
  # keep comm variables
  select(
    PID, 
    matches("CommunicationF. .+") & !contains('DO'),
    `Communication DO`
    ) 


####.. AQ ####

recoded_data_aq <- valid_data %>% 
  rowwise() %>% 
  mutate(
    AQ_n_missing = sum(is.na(c_across(starts_with("AQ")))),
    
    AQ_total = sum_AQ(
      c_across(matches(qvars_to_regex(AQ_VARS_REGULAR, "AQ"))),
      reversed = FALSE
    ) + sum_AQ(
      c_across(matches(qvars_to_regex(AQ_VARS_REVERSED, "AQ"))),
      reversed = TRUE
      )
  ) %>% 
  select(
    PID, 
    AQ_total,
    AQ_n_missing
  )

# check missing responses
full_join(recoded_data_indep, recoded_data_aq) %>% 
  group_by(group,AQ_n_missing) %>% tally()

####.. BAPQ ####

recoded_data_bapq <- valid_data %>% 
  rowwise() %>% 
  mutate(
    BAPQ_n_missing = sum(is.na(c_across(starts_with("BAPQ")))),
    
    # total
    BAPQ_total = sum_BAPQ(
      c_across(
        matches(qvars_to_regex(BAPQ_VARS_REGULAR,"BAPQ"))
        ),
      reversed = FALSE
    ) + sum_BAPQ(
      c_across(
        matches(qvars_to_regex(BAPQ_VARS_REVERSED,"BAPQ"))
        ),
      reversed = TRUE
      ),
    
    # subscale aloof
    BAPQ_sub_Aloof = sum_BAPQ(
      c_across(
        matches(qvars_to_regex(BAPQ_VARS_ALOOF,"BAPQ")) &
          matches(qvars_to_regex(BAPQ_VARS_REGULAR,"BAPQ"))
      ),
      reversed = FALSE
    ) +
      sum_BAPQ(
        c_across(
          matches(qvars_to_regex(BAPQ_VARS_ALOOF,"BAPQ")) &
            matches(qvars_to_regex(BAPQ_VARS_REVERSED,"BAPQ"))
        ),
        reversed = TRUE
      ),
    
    #subscale pragmatic language
    BAPQ_sub_PragLang = sum_BAPQ(
      c_across(
        matches(qvars_to_regex(BAPQ_VARS_PRAGLANG,"BAPQ")) &
          matches(qvars_to_regex(BAPQ_VARS_REGULAR,"BAPQ"))
      ),
      reversed = FALSE
    ) +
      sum_BAPQ(
        c_across(
          matches(qvars_to_regex(BAPQ_VARS_PRAGLANG,"BAPQ")) &
            matches(qvars_to_regex(BAPQ_VARS_REVERSED,"BAPQ"))
        ),
        reversed = TRUE
      ),
    
    # subscale rigid
    BAPQ_sub_Rigid = sum_BAPQ(
      c_across(
        matches(qvars_to_regex(BAPQ_VARS_RIGID,"BAPQ")) &
          matches(qvars_to_regex(BAPQ_VARS_REGULAR,"BAPQ"))
      ),
      reversed = FALSE
    ) +
      sum_BAPQ(
        c_across(
          matches(qvars_to_regex(BAPQ_VARS_RIGID,"BAPQ")) &
            matches(qvars_to_regex(BAPQ_VARS_REVERSED,"BAPQ"))
        ),
        reversed = TRUE
      )          
  ) %>% 
  select(
    PID,
    BAPQ_total,
    BAPQ_sub_Aloof,
    BAPQ_sub_PragLang,
    BAPQ_sub_Rigid,
    BAPQ_n_missing
  )

# check missing responses
full_join(recoded_data_indep, recoded_data_bapq) %>% 
  group_by(group,BAPQ_n_missing) %>% tally()

####.. STQ ####

recoded_data_stq <- valid_data %>% 
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
  select(
    PID,
    STQ_total,
    STQ_n_missing
  )

# check missing responses
full_join(recoded_data_indep, recoded_data_stq) %>% 
  group_by(group,STQ_n_missing) %>% tally()

####.. TAS ####

recoded_data_tas <- valid_data %>% 
  rowwise() %>% 
  mutate(
    TAS_n_missing = sum(is.na(c_across(starts_with("TAS")))),
    
    # TAS total score
    TAS_total = sum_TAS(
      c_across(matches(qvars_to_regex(TAS_VARS_REGULAR, "TAS"))),
      reversed = FALSE
    ) +
      sum_TAS(
        c_across(matches(qvars_to_regex(TAS_VARS_REVERSED, "TAS"))),
        reversed = TRUE
      ),
    
    # TAS subscale difficulty identifying feelings
    TAS_sub_IdFeelings = sum_TAS(
      c_across(
        matches(qvars_to_regex(TAS_VARS_REGULAR, "TAS")) &
          matches(qvars_to_regex(TAS_VARS_IDFEELINGS, "TAS"))
      ),
      reversed = FALSE
    ) +
      sum_TAS(
        c_across(
          matches(qvars_to_regex(TAS_VARS_REVERSED, "TAS")) &
            matches(qvars_to_regex(TAS_VARS_IDFEELINGS, "TAS"))
          ),
        reversed = TRUE
      ),
    
    # TAS subscale difficulty describing feelings
    TAS_sub_DescFeelings = sum_TAS(
      c_across(
        matches(qvars_to_regex(TAS_VARS_REGULAR, "TAS")) &
          matches(qvars_to_regex(TAS_VARS_DESCFEELINGS, "TAS"))
      ),
      reversed = FALSE
    ) +
      sum_TAS(
        c_across(
          matches(qvars_to_regex(TAS_VARS_REVERSED, "TAS")) &
            matches(qvars_to_regex(TAS_VARS_DESCFEELINGS, "TAS"))
        ),
        reversed = TRUE
      ),
    
    # TAS subscale externally-oriented thinking
    TAS_sub_ExtThinking = sum_TAS(
      c_across(
        matches(qvars_to_regex(TAS_VARS_REGULAR, "TAS")) &
          matches(qvars_to_regex(TAS_VARS_EXTTHINKING, "TAS"))
      ),
      reversed = FALSE
    ) +
      sum_TAS(
        c_across(
          matches(qvars_to_regex(TAS_VARS_REVERSED, "TAS")) &
            matches(qvars_to_regex(TAS_VARS_EXTTHINKING, "TAS"))
        ),
        reversed = TRUE
      )
    
  ) %>% 
  select(
    PID,
    TAS_total,
    TAS_sub_IdFeelings,
    TAS_sub_DescFeelings,
    TAS_sub_ExtThinking,
    TAS_n_missing
  ) 

full_join(recoded_data_indep, recoded_data_tas) %>% 
  group_by(group, TAS_n_missing) %>% tally()

#### missing questionnaire responses ####
data_folder <- "Data/"
if (!dir.exists(data_folder)) {dir.create(data_folder)}
recoded_data_tas %>% 
  select(`Response ID`, group, `User Language`, `Country of Residence`, contains("n_missing")) %>% 
  rowwise() %>% 
  mutate(total_n_missing = sum(c_across(contains("n_missing")))) %>% 
  filter(total_n_missing > 0) %>%
  select(-total_n_missing) %>% 
  arrange(group, -across(contains("n_missing"))) %>% 
  write_csv(paste0(data_folder, "online_missing_questionnaire_responses.csv"))

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
