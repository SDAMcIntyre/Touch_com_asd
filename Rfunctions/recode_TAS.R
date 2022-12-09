recode_TAS <- function(df) {
  
  #item coding
  TAS_VARS_REVERSED <- c(4, 5, 10, 18, 19)
  TAS_VARS_REGULAR <- setdiff(1:20, TAS_VARS_REVERSED)
  TAS_VARS_IDFEELINGS <- c(1, 3, 6, 7, 9, 13, 14)
  TAS_VARS_DESCFEELINGS <- c(2, 4, 11, 12, 17)
  TAS_VARS_EXTTHINKING <- c(5, 8, 10, 15, 16, 18, 19, 20)
  
  # sum function to get total
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
  
  # main part of the function
  df %>% 
    rowwise() %>% 
      mutate(
        TAS_n_missing = sum(is.na(c_across(starts_with("TAS")))),
        
        # TAS total score
        TAS_total = sum_TAS(
          c_across(matches(qvars_to_regex(TAS_VARS_REGULAR, "TAS"))),
          reversed = FALSE
        ) +
          sum_TAS(
            c_across(matches(qvars_to_regex(TAS_VARS_REVERSED, "TAS"))),
            reversed = TRUE
          ),
        
        # TAS subscale difficulty identifying feelings
        TAS_sub_IdFeelings = sum_TAS(
          c_across(
            matches(qvars_to_regex(TAS_VARS_REGULAR, "TAS")) &
              matches(qvars_to_regex(TAS_VARS_IDFEELINGS, "TAS"))
          ),
          reversed = FALSE
        ) +
          sum_TAS(
            c_across(
              matches(qvars_to_regex(TAS_VARS_REVERSED, "TAS")) &
                matches(qvars_to_regex(TAS_VARS_IDFEELINGS, "TAS"))
            ),
            reversed = TRUE
          ),
        
        # TAS subscale difficulty describing feelings
        TAS_sub_DescFeelings = sum_TAS(
          c_across(
            matches(qvars_to_regex(TAS_VARS_REGULAR, "TAS")) &
              matches(qvars_to_regex(TAS_VARS_DESCFEELINGS, "TAS"))
          ),
          reversed = FALSE
        ) +
          sum_TAS(
            c_across(
              matches(qvars_to_regex(TAS_VARS_REVERSED, "TAS")) &
                matches(qvars_to_regex(TAS_VARS_DESCFEELINGS, "TAS"))
            ),
            reversed = TRUE
          ),
        
        # TAS subscale externally-oriented thinking
        TAS_sub_ExtThinking = sum_TAS(
          c_across(
            matches(qvars_to_regex(TAS_VARS_REGULAR, "TAS")) &
              matches(qvars_to_regex(TAS_VARS_EXTTHINKING, "TAS"))
          ),
          reversed = FALSE
        ) +
          sum_TAS(
            c_across(
              matches(qvars_to_regex(TAS_VARS_REVERSED, "TAS")) &
                matches(qvars_to_regex(TAS_VARS_EXTTHINKING, "TAS"))
            ),
            reversed = TRUE
          )
        
      )
}



