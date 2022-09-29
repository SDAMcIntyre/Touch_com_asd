library(readxl)
library(dplyr)
library(readr)

raw_data_folder <- "/Users/sarmc72/Library/CloudStorage/OneDrive-Linköpingsuniversitet/projects - in progress/Touch Comm ASD/Data"

live_individual_data <- read_excel(
  paste0(raw_data_folder,"/Quetionnairedata_2022-09-29.xlsx"), 
  sheet = "Control_Familjerelationer",
  range = "A1:E36"
  ) %>% 
  mutate(
    group = "Control"
  ) %>% 
  rbind(

    read_excel(
      paste0(raw_data_folder,"/Quetionnairedata_2022-09-29.xlsx"), 
      sheet = "ASD_Familjerelationer",
      range = "A1:E36"
    ) %>% 
      mutate(
        group = "AASD"
      )
    
  ) %>% 
  select(c("PID", "Age", "Gender", "group"))

private_data_folder <- "Data/private"
if (!dir.exists(private_data_folder)) {dir.create(private_data_folder)}
write_csv(live_individual_data, paste0(private_data_folder,"/live_valid-individual-data.csv"))

