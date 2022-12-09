recode_questionnaire <- function(df, questionnaire) {
  
  #### questionnaire coding variables ####
  
  AQ_VARS_REGULAR <- c(2,4,5,6,7,9,12,13,16,18,19,20,21,22,23,26,33,35,39,41,42,43,45,46)
  AQ_VARS_REVERSED <- c(1,3,8,10,11,14,15,17,24,25,27,28,29,30,31,32,34,36,37,38,40,44,47,48,49,50)
  
  BAPQ_VARS_REVERSED <- c(1, 3, 7, 9, 12, 15, 16, 19, 21, 23, 25, 28, 30, 34, 36)
  BAPQ_VARS_REGULAR <- setdiff(1:36, BAPQ_VARS_REVERSED)
  BAPQ_VARS_ALOOF <- c(1, 5, 9, 12, 16, 18, 23, 25, 27, 28, 31, 36)
  BAPQ_VARS_PRAGLANG <- c(2, 4, 7, 10, 11, 14, 17, 20, 21, 29, 32, 34)
  BAPQ_VARS_RIGID <- c(3, 6, 8, 13, 15, 19, 22, 24, 26, 30, 33, 35)
  
  STQ_VARS_REVERSED <- c(1,4,6,9,11,12,14,15,18,20)
  STQ_VARS_REGULAR  <- setdiff(1:20, STQ_VARS_REVERSED)
  
  TAS_VARS_REVERSED <- c(4, 5, 10, 18, 19)
  TAS_VARS_REGULAR <- setdiff(1:20, TAS_VARS_REVERSED)
  TAS_VARS_IDFEELINGS <- c(1, 3, 6, 7, 9, 13, 14)
  TAS_VARS_DESCFEELINGS <- c(2, 4, 11, 12, 17)
  TAS_VARS_EXTTHINKING <- c(5, 8, 10, 15, 16, 18, 19, 20)
  
  # questionnaire specific variables
  q_n_missing <- paste0(questionnaire,"_n_missing")
  q_total <- paste0(questionnaire,"_total")
  
  sum_fns <- list(
    AQ = sum_AQ,
    BAPQ = sum_BAPQ,
    STQ = sum_STQ,
    TAS = sum_TAS
  )
  
  # main part of the function
  df %>% 
  rowwise() %>%
  mutate(
    !!q_n_missing := sum(is.na(c_across(starts_with(questionnaire)))),

    !!q_total := sum_fns[[questionnaire]](
      c_across(matches(qvars_to_regex(AQ_VARS_REGULAR, questionnaire))),
      reversed = FALSE
    ) + sum_fns[[questionnaire]](
      c_across(matches(qvars_to_regex(AQ_VARS_REVERSED, questionnaire))),
      reversed = TRUE
    )
  ) 
}