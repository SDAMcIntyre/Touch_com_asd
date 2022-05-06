collate_live_data <- function(folder, pattern) {
  folder %>% 
    list.files(pattern = pattern, full.names = TRUE, recursive = TRUE) %>% 
    read_csv(id = 'path', col_types = cols()) %>% 
    mutate(
      path = str_remove(path, path.expand(folder)),
      group = str_extract(path, '(ASD - )|(Controls - )') %>% str_replace_all('( - )|s', ''),
      PID = str_extract(path,'(asd[0-9]+a*/)|(sub[0-9]+a*/)') #%>% parse_number() %>% as.factor() %>% as.numeric()
    ) 
}
