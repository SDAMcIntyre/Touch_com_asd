write_path_csv <- function(x, path, filename, ...) {
  if (!dir.exists(path)) {dir.create(path, recursive = TRUE)}    
  write_csv(x, paste0(path, filename, ...))
}