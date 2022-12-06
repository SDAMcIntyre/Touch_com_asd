library(tidyverse)
library(summarytools)

# source all .R files in the Rfunctions directory
sapply(list.files("Rfunctions", full.names = TRUE), source)

#### read data ####
ind_data <- read_csv("Data/primary/live_individual_recoded.csv")

#### output folders ####
figure_folder <- "Figures/"

####  demographics  #### 

open_plot_window()

ind_data %>% 
  ggplot() +
  facet_wrap(. ~ group) +
  geom_bar(
    aes(x = `Age Group`, fill = Gender),
    stat = "count",
    alpha = 0.7,
    colour = 'black'
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

ggsave_path(figure_folder,"live_demographics-1.pdf")

ctable(
  x = ind_data$group,
  y = ind_data$Gender
) %>% view



