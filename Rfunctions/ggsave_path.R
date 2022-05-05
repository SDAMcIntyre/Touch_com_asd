ggsave_path <- function(path, filename, ...) {
  if (!dir.exists(path)) {dir.create(path)}    
  ggsave(paste0(path, filename, ...))
}