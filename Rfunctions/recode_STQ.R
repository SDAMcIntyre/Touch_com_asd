recode_STQ <- function(df) {
  
  #item coding
  STQ_VARS_REGULAR <- c(1,4,6,9,11,12,14,15,18,20)
  STQ_VARS_REVERSED <- setdiff(1:20, STQ_VARS_REGULAR)
  
  # sum function to get total
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
  
  # main part of the function
  df %>% 
    rowwise() %>% 
    mutate(
      STQ_n_missing = sum(is.na(c_across(starts_with("STQ")))),
      
      STQ_total = sum_STQ(
        c_across(matches(qvars_to_regex(STQ_VARS_REGULAR, "STQ"))),
        reversed = FALSE
      ) +
        sum_STQ(
          c_across(matches(qvars_to_regex(STQ_VARS_REVERSED, "STQ"))),
          reversed = TRUE
        )
    )
}






