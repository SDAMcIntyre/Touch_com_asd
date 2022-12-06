read_scanned_filenames <- function(folder, pattern) {
  tibble(
    path = list.files(folder, pattern = pattern, full.names = TRUE, recursive = TRUE)
    ) %>% 
    mutate(
      filename = basename(path),
      path = str_remove(path, path.expand(folder)) 
      ) %>%
    separate(filename, c("date_paper", "group", "PID", "document"), sep = "_", fill = "left") %>% 
    mutate(
      group = group %>% str_replace_all('( - )|s|(adult)|(youth)', ''),
      document = document %>% str_remove("\\.pdf"),
    ) %>% 
    pad_PIDs() 
  }
