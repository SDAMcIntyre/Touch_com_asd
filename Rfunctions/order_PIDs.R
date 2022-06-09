order_PIDs <- function(.data, PID) {
  .data %>%
    confusion_matrix_data({{PID}}) %>% 
    filter(cued == response) %>% 
    group_by({{PID}}) %>% 
    summarise(performance = mean(Percent)) %>% 
    arrange(-performance) %>% 
    pull({{PID}})
}
