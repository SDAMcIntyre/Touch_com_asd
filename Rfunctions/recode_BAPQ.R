
recode_BAPQ <- function(df) {
  
  #item coding
  BAPQ_VARS_REVERSED <- c(1, 3, 7, 9, 12, 15, 16, 19, 21, 23, 25, 28, 30, 34, 36)
  BAPQ_VARS_REGULAR <- setdiff(1:36, BAPQ_VARS_REVERSED)
  BAPQ_VARS_ALOOF <- c(1, 5, 9, 12, 16, 18, 23, 25, 27, 28, 31, 36)
  BAPQ_VARS_PRAGLANG <- c(2, 4, 7, 10, 11, 14, 17, 20, 21, 29, 32, 34)
  BAPQ_VARS_RIGID <- c(3, 6, 8, 13, 15, 19, 22, 24, 26, 30, 33, 35)

  # sum function to get total
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

  # main part of the function
  df %>% 
    rowwise() %>% 
    mutate(
      BAPQ_n_missing = sum(is.na(c_across(starts_with("BAPQ")))),
      
      # total
      BAPQ_total = sum_BAPQ(
        c_across(
          matches(qvars_to_regex(BAPQ_VARS_REGULAR,"BAPQ"))
        ),
        reversed = FALSE
      ) + sum_BAPQ(
        c_across(
          matches(qvars_to_regex(BAPQ_VARS_REVERSED,"BAPQ"))
        ),
        reversed = TRUE
      ),
      
      # subscale aloof
      BAPQ_sub_Aloof = sum_BAPQ(
        c_across(
          matches(qvars_to_regex(BAPQ_VARS_ALOOF,"BAPQ")) &
            matches(qvars_to_regex(BAPQ_VARS_REGULAR,"BAPQ"))
        ),
        reversed = FALSE
      ) +
        sum_BAPQ(
          c_across(
            matches(qvars_to_regex(BAPQ_VARS_ALOOF,"BAPQ")) &
              matches(qvars_to_regex(BAPQ_VARS_REVERSED,"BAPQ"))
          ),
          reversed = TRUE
        ),
      
      #subscale pragmatic language
      BAPQ_sub_PragLang = sum_BAPQ(
        c_across(
          matches(qvars_to_regex(BAPQ_VARS_PRAGLANG,"BAPQ")) &
            matches(qvars_to_regex(BAPQ_VARS_REGULAR,"BAPQ"))
        ),
        reversed = FALSE
      ) +
        sum_BAPQ(
          c_across(
            matches(qvars_to_regex(BAPQ_VARS_PRAGLANG,"BAPQ")) &
              matches(qvars_to_regex(BAPQ_VARS_REVERSED,"BAPQ"))
          ),
          reversed = TRUE
        ),
      
      # subscale rigid
      BAPQ_sub_Rigid = sum_BAPQ(
        c_across(
          matches(qvars_to_regex(BAPQ_VARS_RIGID,"BAPQ")) &
            matches(qvars_to_regex(BAPQ_VARS_REGULAR,"BAPQ"))
        ),
        reversed = FALSE
      ) +
        sum_BAPQ(
          c_across(
            matches(qvars_to_regex(BAPQ_VARS_RIGID,"BAPQ")) &
              matches(qvars_to_regex(BAPQ_VARS_REVERSED,"BAPQ"))
          ),
          reversed = TRUE
        )          
    ) 
}