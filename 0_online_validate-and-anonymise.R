# load libraries  ####
library(qualtRics)
library(dplyr)
library(readr)
library(sjlabelled)

# source all .R files in the Rfunctions directory ####
sapply(list.files("Rfunctions", full.names = TRUE), source)

# input path ####
RAW_DATA_FOLDER <- "~/Library/CloudStorage/OneDrive-Linköpingsuniversitet/projects - in progress/Touch Comm ASD/Data/"

# output paths ####
REPORTS_FOLDER <- "Data/reports/"
PRIMARY_DATA_FOLDER <- "Data/primary/"

# ===== MAIN ===== 

# read in raw survey data exported from qualtrics ####
raw_data_file <- paste0(
  RAW_DATA_FOLDER,
  'Qualtrics Touch-Comm-ASD online survey March 28, 2022_14.50.csv'
)

raw_data <- read_survey(raw_data_file)

# SCREENING for those who... ####
valid_data <- raw_data %>% 
  # are not spam
  filter(Status != "Spam") %>% 
  # completed the survey
  filter(Progress == 100) %>% 
  # were not part of the testing/preview phase
  filter(DistributionChannel != "preview") %>% 
  # give consent
  filter(Q2 == "Yes, I will participate.") %>% 
  # are aged 15+
  filter(Q3 != "under 15") %>% 
  # do not have a bipolar or psychosis diagnosis
  filter(Q6 == "No") %>% 
  # do not regularly take recreational drugs
  filter(Q8 == "No") %>% 
  # do not regularly drink 5 or more alcoholic drinks per day
  filter(Q10 == "No") %>% 
  # are not interested in participating in the in-person study
  filter(Q12 == "No") %>% 
  # can see the video
  filter(Q15 == "Yes") %>% 
  # answered the question about ASD
  filter(!is.na(Q20)) %>% 

# collapse age into bins ####
  mutate(
    `Age Group` = case_when(
      between(Q19, 16, 20) ~ '16 - 20',
      between(Q19, 21, 25) ~ '21 - 25',
      between(Q19, 26, 30) ~ '26 - 30',
      between(Q19, 31, 35) ~ '31 - 35',
      between(Q19, 36, 40) ~ '36 - 40'
    ),
    `Age Cohort` = case_when(
      between(Q19, 16, 17) ~ "Youth (16 - 17)",
      between(Q19, 18, 40) ~ "Adult (18 - 40)"
    )
  ) %>% 
  
# remove screening and age variables for privacy ####  
  select(-c(
    Status, 
    Progress, 
    DistributionChannel,
    Q2,
    Q3,
    Q6,
    Q8,
    Q10,
    Q12,
    Q15,
    Q19
    ))

# save resulting data ####
write_path_csv(valid_data, PRIMARY_DATA_FOLDER,"online_valid-anon-data.csv")

# save report of variable names and descriptions from qualtrics ####
tibble(
  qualtrics_name = names(valid_data),
  qualtrics_description = get_label(valid_data)
) %>% 
  write_path_csv(REPORTS_FOLDER, "online_qualtrics-variable-names.csv")


# old code to adapt: survey date / time data  #### 

# min(ind_data$`Start Date`)
# min(ind_data$`Recorded Date`)
# 
# max(ind_data$`Start Date`)
# max(ind_data$`Recorded Date`)
# 
# ind_data %>% 
#   filter(group == "ASD") %>% 
#   pull(`Recorded Date`) %>% max()
# 
# ind_data %>% 
#   mutate(`End Difference` = `Recorded Date` - `End Date`) %>% 
#   ggplot(aes(x = `End Difference`)) + geom_histogram()
# 
# open_plot_window(width = 4.6, height = 3.9)
# 
# ind_data %>% 
#   ggplot(aes(x = `Recorded Date`)) + 
#   facet_wrap(~ group, ncol = 1) +
#   geom_histogram() +
#   labs(x = 'Completion Date') +
#   theme_bw() 
# 
# ggsave_path(figure_folder,"online_completion-date.png")
# 
# ind_data  %>% 
#   filter(`Duration (minutes)` <= 60) %>% 
#   ggplot(aes(x = `Duration (minutes)`)) + geom_histogram()
# 
# ind_data %>% 
#   filter(`Duration (minutes)` > 60) %>% 
#   ggplot(aes(x = `Duration (hours)`)) + geom_histogram()
# 
