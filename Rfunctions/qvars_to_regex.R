qvars_to_regex <- function(x, q_prefix) {
  to_regex(paste0(
    "^", q_prefix,
    "_",
    x, "$"
  ))
}
