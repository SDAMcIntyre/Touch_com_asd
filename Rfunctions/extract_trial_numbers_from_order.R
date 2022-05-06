extract_trial_numbers_from_order <- function(df, order_var) {
  df %>% 
    mutate(
      order = str_remove_all(.[[order_var]], "[a-z]")
    ) %>% 
    select(PID,order) %>% 
    separate(
      order,
      into = paste0("o",1:6),
      sep = "\\|"
    ) %>% 
    pivot_longer(
      cols = starts_with("o"),
      names_to = "trial",
      names_prefix = "o",
      names_transform = list(trial = parse_integer),
      values_to = "cued"
    ) %>% 
    mutate(
      cued = case_when(
        cued == "1" ~ "attention",
        cued == "2" ~ "calming",
        cued == "3" ~ "gratitude",
        cued == "4" ~ "happiness",
        cued == "5" ~ "love",
        cued == "6" ~ "sadness"
      )
    )
}
