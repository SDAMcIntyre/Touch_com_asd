confusion_matrix_data <- function(.data, ...) {
  if (!all(c("cued", "response") %in% names(.data))) {
    stop("`data` must contain `cued` and `response` columns")
  }
  .data %>% 
    tally_by(..., cued, response) %>%
    rename('respFreq' = 'n') %>%
    group_by(...,cued) %>%
    mutate(cuedFreq = sum(respFreq),
           Percent = 100*respFreq/cuedFreq) %>%
    ungroup()
}