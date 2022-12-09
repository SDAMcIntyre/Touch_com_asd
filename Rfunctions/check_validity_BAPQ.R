check_validity_BAPQ <- function(df) {
  df %>% 
    select(
      PID, 
      starts_with("BAPQ")
    ) %>% 
    pivot_longer(
      cols = starts_with("BAPQ"),
      names_to = "item",
      values_to = "response"
    ) %>% 
    mutate(response = if_else(
      response %in%  c(
        "Very rarely",
        "Rarely",
        "Occasionally",
        "Somewhat often",
        "Often",
        "Very often",
        NA_character_
      ),
      NA_character_,
      response
    )) %>% 
    filter(!is.na(response))
}