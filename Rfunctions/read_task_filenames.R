read_task_filenames <- function(folder, pattern) {
  tibble(
    path = list.files(folder, pattern, full.names = TRUE, recursive = TRUE)
  ) %>% 
    mutate(
      path = str_remove(path, path.expand(folder)),
      group = str_extract(path, '(ASD - )|(Controls - )') %>% str_replace_all('( - )|s', ''),
      PID = str_extract(path,'(asd[0-9]+a*/)|(sub[0-9]+a*/)') %>% str_remove("/"),
      date_computer = str_extract(path,'202[0-2]-[0-9]{2}-[0-9]{2}')
    ) %>% 
    pad_PIDs()
}