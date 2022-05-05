
sum_STQ <- function(x, reversed = FALSE) {
  recoded_scores <- case_when(
    x == "not at all" ~ 0,
    x == "slightly" ~ 1,
    x == "moderately" ~ 2,
    x == "very" ~ 3,
    x == "extremely" ~ 4
  )
  
  if (reversed) {
    return(sum(4-recoded_scores))
  } else {
    return(sum(recoded_scores))
  }
}