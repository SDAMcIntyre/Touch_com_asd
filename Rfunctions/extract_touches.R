extract_touches <- function(x) {
  x_labels <- c("ATTENTION", "CALMING", "GRATITUDE", "HAPPINESS", "LOVE", "SADNESS", "something else")
  x_regex <- to_regex(x_labels)
  extracted <- str_extract(x, x_regex)
  output <- str_replace(
    extracted, 
    "something else", 
    "other"
  )
  tolower(output)
}