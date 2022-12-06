pad_PIDs <- function(df) {
  df %>% 
  separate(PID, c("PID_text","PID_num"), sep = 3) %>% 
  mutate(
    PID_suffix = str_extract(PID_num, "a?"),
    PID_num = str_pad(PID_num, 2, pad = "0")
  ) %>% 
  unite("PID", c("PID_text","PID_num","PID_suffix"), sep = "")
}