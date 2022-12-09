check_validity_STQ <- function(df) {
  df %>% 
    select(
      PID, 
      starts_with("STQ")
    ) %>% 
    pivot_longer(
      cols = starts_with("STQ"),
      names_to = "item",
      values_to = "response"
    ) %>% 
    mutate(response = if_else(
      response %in%  c(
        "not at all",
        "slightly",
        "moderately",
        "very",
        "extremely",
        NA_character_
      ),
      NA_character_,
      response
    )) %>% 
    filter(!is.na(response))
}