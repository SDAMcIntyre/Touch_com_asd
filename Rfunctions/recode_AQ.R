recode_AQ <- function(df) {
  
  #item coding
  
  AQ_VARS_REGULAR <- c(2,4,5,6,7,9,12,13,16,18,19,20,21,22,23,26,33,35,39,41,42,43,45,46)
  AQ_VARS_REVERSED <- c(1,3,8,10,11,14,15,17,24,25,27,28,29,30,31,32,34,36,37,38,40,44,47,48,49,50)
  
  # sum function to get total
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
  
  # main part of the function
  df %>% 
    rowwise() %>%
    mutate(
      AQ_n_missing = sum(is.na(c_across(starts_with("AQ")))),
      
      AQ_total = sum_AQ(
        c_across(matches(qvars_to_regex(AQ_VARS_REGULAR, "AQ"))),
        reversed = FALSE
      ) + sum_AQ(
        c_across(matches(qvars_to_regex(AQ_VARS_REVERSED, "AQ"))),
        reversed = TRUE
      )
    ) 
}





