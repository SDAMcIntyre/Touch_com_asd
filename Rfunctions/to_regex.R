to_regex <- function(x) {
  paste0('(', paste(x, collapse = ')|('), ')')
}