check_validity_AQ <- function(df) {
  df %>% 
    select(
      PID, 
      starts_with("AQ")
    ) %>% 
    pivot_longer(
      cols = starts_with("AQ"),
      names_to = "item",
      values_to = "response"
    ) %>% 
    mutate(response = if_else(
      response %in%  c(
        "definitely agree",
        "slightly agree",
        "slightly disagree",
        "definitely disagree", 
        NA_character_
      ),
      NA_character_,
      response
    )) %>% 
    filter(!is.na(response))
}