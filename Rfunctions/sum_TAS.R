sum_TAS <- function(x, reversed = FALSE) {
  recoded_scores <- case_when(
    x == "completely agree" ~ 5,
    x == "agree" ~ 4,
    x == "neutral" ~ 3,
    x == "disagree" ~ 2,
    x == "completely disagree" ~ 1
  )
  
  if (reversed) {
    return(sum(6-recoded_scores))
  } else {
    return(sum(recoded_scores))
  }
}
