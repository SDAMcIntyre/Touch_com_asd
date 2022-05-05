sum_AQ <- function(x, reversed = FALSE) {
  recoded_scores <- case_when(
    x == "definitely agree" ~ 1,
    x == "slightly agree" ~ 1,
    x == "slightly disagree" ~ 0,
    x == "definitely disagree" ~ 0
  )
  
  if (reversed) {
    return(sum(1-recoded_scores))
  } else {
    return(sum(recoded_scores))
  }
}
