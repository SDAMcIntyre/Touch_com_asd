library(readxl)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# source all .R files in the Rfunctions directory
sapply(list.files("Rfunctions", full.names = TRUE), source)

RAW_DATA_FOLDER <- "~/Library/CloudStorage/OneDrive-Linköpingsuniversitet/projects - in progress/Touch Comm ASD/Data/"

# read metadata from scanned questionnaire files
scanned <- read_scanned_filenames(RAW_DATA_FOLDER, "\\.pdf") %>% 
  mutate(status = "scanned") %>% 
  pivot_wider(
    id_cols = c("group", "PID", "date_paper"),
    names_from = document,
    values_from = status
  ) %>% 
  select(-group)

# read metadata from the communication task
comm_task <- read_task_filenames(RAW_DATA_FOLDER, 'comm.*data\\.csv') %>% 
  group_by(PID,date_computer) %>% 
  tally() %>% 
  mutate(comm_task = "completed") %>% 
  select(-n) 

# read metadata from the pleasantness task
pleas_task <- read_task_filenames(RAW_DATA_FOLDER, 'pleas.*data\\.csv') %>% 
  group_by(PID,date_computer) %>% 
  tally() %>% 
  mutate(pleas_task = "completed") %>% 
  select(-n) 

# read notes about the experiment
notes <- read_tsv(paste0(RAW_DATA_FOLDER, "in person/missing-data-notes.txt"))

# save a report giving an overview of the data set

computer_tasks <- full_join(comm_task, pleas_task) 

full_join(computer_tasks, scanned) %>% 
  left_join(notes) %>% 
  mutate(
    date_conflict = if_else(
    !is.na(date_paper) & !is.na(date_computer) & date_paper != date_computer,
    TRUE,
    FALSE
  ), 
  comment = replace_na(comment, "")
  ) %>% 
  select(c("PID", starts_with("date"), everything())) %>% 
  arrange(PID) %>% 
  write_path_csv("Data/reports/", "live_data-validation.csv")

# reading from the spreadsheet (manually entered from paper surveys)

live_individual_data <- read_excel(
  paste0(RAW_DATA_FOLDER,"/in person/Quetionnairedata_2022-09-29.xlsx"), 
  sheet = "Control_Familjerelationer",
  range = "A1:E36"
  ) %>% 
  mutate(
    group = "Control"
  ) %>% 
  rbind(

    read_excel(
      paste0(RAW_DATA_FOLDER,"/in person/Quetionnairedata_2022-09-29.xlsx"), 
      sheet = "ASD_Familjerelationer",
      range = "A1:E36"
    ) %>% 
      mutate(
        group = "ASD"
      )
    
  ) %>% 
  select(c("PID", "Age", "Gender", "group"))

private_data_folder <- "Data/private"
if (!dir.exists(private_data_folder)) {dir.create(private_data_folder)}
write_csv(live_individual_data, paste0(private_data_folder,"/live_valid-individual-data.csv"))

