calculate_performance_metrics <- function(.data, item, response, ...) {
  .data %>% 
    # count up responses for all combos of item and response
    tally_by(..., {{item}}, {{response}}) %>% 
    # calculate total number of trials/presentations
    group_by(...) %>% mutate(Total = sum(n)) %>% 
    # number of times each cue appeared
    group_by(...,{{item}}) %>% mutate(Present = sum(n)) %>%
    # number of times each response was made
    group_by(..., {{response}}) %>% mutate(Selected = sum(n)) %>%
    # get rid of unneeded rows (we're summarising, one line for each cue)
    filter({{item}} == {{response}}) %>% rename('Hits' = 'n') %>% # now counts are just hits
    # calculate performance variables
    ungroup() %>% 
    mutate(Misses = Present - Hits,
           FalseAlarms = Selected - Hits,
           CorrectRejections = Total - Present - FalseAlarms,
           Recall = Hits/Present, # p correct when cue present
           Precision = Hits/Selected, # p correct with this response
           Precision = if_else(is.na(Precision), 0, Precision), 
           F1 = 2*( (Precision*Recall) / (Precision+Recall)), 
           F1 = if_else(is.na(F1), 0, F1),
           F1chance = 2*( (Present/Total) / ((Present/Total)+1) ) # always give same answer
    )
}


present <- function(label, item) {
  sum(item == label)
}

selected <- function(label, response) {
  sum(response == label)
}

hits <- function(label, item, response) {
  sum(item == response & item == label)
}

misses <- function(label, item, response) {
  present(label, item) - hits(label, item, response)
}

false_alarms <- function(label, item, response) {
  selected(label, response) - hits(label, item, response)
}

correct_rejections <- function(label, item, response) {
  length(item) - present(label, item) - false_alarms(label, item, response)
}

recall <- function(label, item, response) {
  # p correct when item present
  hits(label, item, response) / present(label, item)
}

precision <- function(label, item, response) {
  # p correct with this response
  precision <- hits(label, item, response) / selected(label, response) 
  if ( is.na(precision) ) {
    return(0)
  } else {
    return(precision)
  }
}

f1  <- function(label, item, response) {
  f1 <- 2 * (
    ( precision(label, item, response) * recall(label, item, response) ) / 
      (precision(label, item, response) + recall(label, item, response) )
    )
  if ( is.na(f1) ) {
    return(0) 
  } else {
      return(f1)
  }
}

f1_chance <- function(label, item, response) {
  # predicted f1 score if just guessing
  2 * ( 
    ( present(label, item) / length(item) ) / 
      ( (present(label, item) / length(item)) + 1 ) 
    )
}
