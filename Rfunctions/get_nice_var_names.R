get_nice_var_names <- function(df, q_idx) {
  c(
    "Start Date",
    "End Date",
    "Duration (in seconds)",
    "Finished",
    "Recorded Date",
    'PID',
    "User Language",
    "Gender",
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
    names(df)[q_idx],
    "Pleasantness DO",
    "Communication Condition ASD",
    "CommunicationFC DO ASD",
    "CommunicationFT DO ASD",
    "Communication Condition Control",
    "CommunicationFC DO Control",
    "CommunicationFT DO Control",
    "Age Group",
    "Age Cohort"
  ) %>% 
    stringr::str_replace(
      pattern = "Q24",
      replacement = "AQ"
    ) %>% 
    stringr::str_replace(
      pattern = "Q25",
      replacement = "BAPQ"
    ) %>% 
    stringr::str_replace(
      pattern = "Q26",
      replacement = "STQ"
    ) %>% 
    stringr::str_replace(
      pattern = "Q29",
      replacement = "TAS"
    )  
  
}
