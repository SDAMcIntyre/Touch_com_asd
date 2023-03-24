library(summarytools)

#https://stackoverflow.com/questions/67721916/getting-rid-of-the-variable-types-in-dfsummary-output-from-summarytools

freq_table <- function(df) {
  st <- df %>% 
    dfSummary(varnumbers = FALSE, valid.col = FALSE, na.col = FALSE)
  if (class(st) == "stby") {
    for (i in seq_along(st)) {
      st[[i]]$Variable <- str_replace(st[[i]]$Variable, "\\[.+\\]", "")
    }
  } else {
    st$Variable <- str_replace(st$Variable, "\\[.+\\]", "")
  }
  st
}
