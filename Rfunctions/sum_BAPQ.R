sum_BAPQ <- function(x, reversed = FALSE) {
  recoded_scores <- case_when(
    x == "Very rarely" ~ 1,
    x == "Rarely" ~ 2,
    x == "Occasionally" ~ 3,
    x == "Somewhat often" ~ 4,
    x == "Often" ~ 5,
    x == "Very often" ~ 6
  )
  
  if (reversed) {
    return(sum(7-recoded_scores))
  } else {
    return(sum(recoded_scores))
  }  
}
