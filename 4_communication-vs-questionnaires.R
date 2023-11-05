library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(emmeans)
# source all .R files in the Rfunctions directory
source_files <- list.files("Rfunctions", full.names = TRUE)
sapply(source_files[grepl(
  "(plot_appearance)", 
  source_files
  )], source)


# input path ####
PROCESSED_DATA_FOLDER <- "Data/processed/"

# output paths ####
FIGURES_FOLDER <- "Figures/"
if ( !dir.exists(FIGURES_FOLDER) ) { dir.create(FIGURES_FOLDER) }

# read individual performance metrics ####

comm_F1micro <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'comm_F1micro-indiv.csv'), 
  col_types = cols()
) 

# read questionnaire data ####

questionnaires <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'questionnaire-data.csv'),
  col_types = cols() 
) %>% filter(task == "forced choice")

# correlations performance vs. questionnaires ####

indiv_data <- full_join(comm_F1micro, questionnaires) %>% 
  mutate(
    group_mode = recode(
      paste(group, experiment),
      `ASD felt touch` = paste0('Felt, Autistic'), 
      `Control felt touch` = paste0('Felt, Control'), 
      `ASD viewed touch` = paste0('Viewed, Autistic'), 
      `Control viewed touch` = paste0('Viewed, Control') 
    )
  )

mdl <- glm(
  formula = F1_micro ~ AQ_total, 
  family = binomial, 
  data = indiv_data,
  subset = experiment == "viewed touch",
  weights = indiv_data$N
)

summary(mdl)
coef(mdl)
confint(mdl)
plogis(coef(mdl))
plogis(confint(mdl))
predict(mdl, type = "response")
residuals(mdl, type = "response")

plogis(coef(mdl)[1] + 22 * coef(mdl)[2] + 0*coef(mdl)[3] + 0*22*coef(mdl)[4])
predict(mdl, tibble(AQ_total = 22, experiment = "felt touch"), type = "response")

plogis(coef(mdl)[1] + 22 * coef(mdl)[2] + 1*coef(mdl)[3] + 1*22*coef(mdl)[4])
predict(mdl, tibble(AQ_total = 22, experiment = "viewed touch"), type = "response")


# model predictions to plot
range_AQ <- min(indiv_data$AQ_total, na.rm = TRUE) : max(indiv_data$AQ_total, na.rm = TRUE)
plot_model_data <- tibble(
  AQ_total = rep(range_AQ,2),
  experiment = rep(unique(indiv_data$experiment), each = length(range_AQ))
)
plot_model_data <- plot_model_data %>% 
  mutate(F1_micro = predict(mdl, plot_model_data, type = "response"))

indiv_data %>% 
  ggplot(mapping = aes(y = F1_micro, x = AQ_total)) +
  facet_grid(. ~ experiment) +
  geom_line(data = plot_model_data) +
  geom_point(mapping = aes(colour = group_mode, fill = group_mode)) +
  theme_light(base_size = 16) +
  scales_touch_comm_asd + 
  theme_nofacetbox +
  theme(legend.position = "top") +
  labs(x = "AQ Score", y = "Overall Agreement Score\n(F1 micro)")

ggsave(paste0(FIGURES_FOLDER,"Agreement_vs_AQ.svg"), width = 8, height = 4)




# sensitivity analyses: ####

#. read subsets for sensitivity analyses ####

comm_F1micro_first6 <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'comm_F1micro-indiv_first6.csv'), 
  col_types = cols()
) 

comm_F1micro_strict <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'comm_F1micro-indiv_strict.csv'), 
  col_types = cols()
)

comm_F1micro_first6_strict <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'comm_F1micro-indiv_first6_strict.csv'), 
  col_types = cols()
)

#. first 6 trials only ####

indiv_data_first6 <- full_join(comm_F1micro_first6, questionnaires)

indiv_data_first6 %>% 
  ggplot(mapping = aes(y = F1_micro, x = BAPQ_total)) +
  facet_grid(experiment ~ .) +
  geom_point(mapping = aes(colour = group)) +
  geom_smooth(method = "lm")

#. strict dataset (similar sex and age demographics, N) ####

indiv_data_strict <- full_join(comm_F1micro_strict, questionnaires)

indiv_data_strict %>% 
  ggplot(mapping = aes(y = F1_micro, x = BAPQ_total)) +
  facet_grid(experiment ~ .) +
  geom_point(mapping = aes(colour = group)) +
  geom_smooth(method = "lm")

#. first 6 trials in strict dataset ####

indiv_data_first6_strict <- full_join(comm_F1micro_first6_strict, questionnaires)

indiv_data_first6_strict %>% 
  ggplot(mapping = aes(y = F1_micro, x = BAPQ_total)) +
  facet_grid(experiment ~ .) +
  geom_point(mapping = aes(colour = group)) +
  geom_smooth(method = "lm")
