check_validity_TAS <- function(df) {
  df %>% 
    select(
      PID, 
      starts_with("TAS")
    ) %>% 
    pivot_longer(
      cols = starts_with("TAS"),
      names_to = "item",
      values_to = "response"
    ) %>% 
    mutate(response = if_else(
      response %in%  c(
        "completely agree",
        "agree",
        "neutral",
        "disagree",
        "completely disagree",
        NA_character_
      ),
      NA_character_,
      response
    )) %>% 
    filter(!is.na(response))
}