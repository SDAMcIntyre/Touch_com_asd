library(qualtRics)
library(sjlabelled)
library(tidyverse)
library(summarytools)

raw_data_folder <- "~/Library/CloudStorage/OneDrive-Linköpingsuniversitet/projects - in progress/Touch Comm ASD/Data/online survey/"

raw_data_file <- paste0(
  raw_data_folder,
  'Communication with social touch in Autism spectrum disorder_March 28, 2022_14.50.csv'
)

raw_data <- read_survey(raw_data_file)

# SCREENING for those who...
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
  filter(!is.na(Q20)) 

data_labels <- tibble(
  var_name = names(valid_data),
  var_label = get_label(valid_data),
  var_nice_name = c(
    get_label(valid_data)[1:10],
    paste0("Screen", seq(1:7)),
    "Gender",
    "Age",
    "ASD",
    "Country of Residence (EN)",
    "Country of Residence (SV)",
    "Pleasantness Attention",
    "Pleasantness Calming",
    "Pleasantness Gratitude",
    "Pleasantness Happiness",
    "Pleasantness Love",
    "Pleasantness Sadness",
    "CommunicationFC Attention",
    "CommunicationFC Attention DO",
    "CommunicationFC Calming",
    "CommunicationFC Calming DO",
    "CommunicationFC Gratitude",
    "CommunicationFC Gratitude DO",
    "CommunicationFC Happiness",
    "CommunicationFC Happiness DO",
    "CommunicationFC Love",
    "CommunicationFC Love DO",
    "CommunicationFC Sadness",
    "CommunicationFC Sadness DO",
    "CommunicationFT Attention",
    "CommunicationFT Calming",
    "CommunicationFT Gratitude",
    "CommunicationFT Happiness",
    "CommunicationFT Love",
    "CommunicationFT Sadness",
    names(valid_data)[47:172],
    "Pleasantness DO",
    "Communication Condition ASD",
    "CommunicationFC DO ASD",
    "CommunicationFT DO ASD",
    "Communication Condition Control",
    "CommunicationFC DO Control",
    "CommunicationFT DO Control"
  )) %>% 
  mutate(var_nice_name = str_replace(
    string = var_nice_name,
    pattern = "Q24",
    replacement = "AQ"
  )) %>% 
  mutate(var_nice_name = str_replace(
    string = var_nice_name,
    pattern = "Q25",
    replacement = "BAPQ"
  )) %>% 
  mutate(var_nice_name = str_replace(
    string = var_nice_name,
    pattern = "Q26",
    replacement = "STQ"
  )) %>% 
  mutate(var_nice_name = str_replace(
    string = var_nice_name,
    pattern = "Q29",
    replacement = "TAS"
  ))

names(valid_data) <- data_labels$var_nice_name


# summary table
valid_data %>% 
  select(-matches(" DO")) %>% # don't show display order variables
  dfSummary() %>% view


data_folder <- "Data/"
if (!dir.exists(data_folder)) {dir.create(data_folder)}
write_csv(data_labels, paste0(data_folder,"/online_variable-labels.csv"))

private_data_folder <- "Data/private"
if (!dir.exists(private_data_folder)) {dir.create(private_data_folder)}
write_csv(valid_data, paste0(private_data_folder,"/online_valid-data.csv"))

