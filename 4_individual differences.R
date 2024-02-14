library(readr)
library(dplyr)
# library(afex)
library(psych)
library(ggplot2)
# library(stringr)
library(tidyr)
# library(emmeans)
library(patchwork)
library(svglite)
# source .R files in the Rfunctions directory
source("Rfunctions/plot_appearance.R")
source("Rfunctions/open_plot_window.R")
source("Rfunctions/corr_matrix.R")

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

# read pleasantness data ####

pleasantness <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'pleasantness-data.csv'), 
  col_types = cols() 
) %>% 
  filter(task == "forced choice") %>% 
  group_by(experiment, group, PID) %>% 
  summarise(Pleasantness = mean(response, na.rm = TRUE) )

# read questionnaire data ####

questionnaires <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'questionnaire-data.csv'),
  col_types = cols() 
) %>% filter(task == "forced choice")

# combine data ####

indiv_data <- full_join(comm_F1micro, questionnaires) %>% 
  full_join(pleasantness) %>% 
  mutate(
    group_mode = recode(
      paste(group, experiment),
      `ASD felt touch` = paste0('Felt, Autistic'), 
      `Control felt touch` = paste0('Felt, Control'), 
      `ASD viewed touch` = paste0('Viewed, Autistic'), 
      `Control viewed touch` = paste0('Viewed, Control') 
    )
  )

afex::set_sum_contrasts()

# correlation matrices ####

cormat_felt <- indiv_data %>% 
  filter(experiment == "felt touch") %>% 
  select(c(F1_micro, Pleasantness, AQ_total, TAS_total, STQ_total)) %>% 
  corr.test(adjust = "holm")

print(cormat_felt, short = FALSE)
cormat_felt$p

plot_cormat_felt <- cormat_felt %>% 
  corr_matrix_df(var_labels = c("Agreem.", "Pleasant.", "AQ", "TAS", "STQ")) %>% 
  corr_matrix_plot() +
  labs(title = "Felt touch")

cormat_viewed <- indiv_data %>% 
  filter(experiment == "viewed touch") %>% 
  select(c(F1_micro, Pleasantness, AQ_total, TAS_total, STQ_total)) %>% 
  corr.test(adjust = "holm")

print(cormat_viewed, short = FALSE)
cormat_viewed$p

plot_cormat_viewed <- cormat_viewed %>% 
  corr_matrix_df(var_labels = c("Agreem.", "Pleasant.", "AQ", "TAS", "STQ")) %>% 
  corr_matrix_plot() +
  labs(title = "Viewed touch")

open_plot_window()
plot_cormat_felt + plot_cormat_viewed + plot_layout(guides = "collect")
ggsave(paste0(FIGURES_FOLDER,"correlation_matrices.svg"), width = 7.9, height = 3.8)


# Agreement from AQ ####

#. plots ####
indiv_data %>% 
  ggplot(mapping = aes(y = F1_micro, x = AQ_total)) +
  facet_grid(. ~ experiment) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.5) +
  geom_point(mapping = aes(colour = group_mode, fill = group_mode)) +
  theme_light(base_size = 16) +
  exp_group_scales + 
  theme_nofacetbox +
  theme(legend.position = "none") +
  labs(x = "Autism Quotient (AQ)", y = "Message agreement\n(F1 micro)") 

ggsave(paste0(FIGURES_FOLDER,"Agreement_AQ.svg"), width = 8, height = 4)


#. stats agreement from AQ ####
# separate for felt and viewed

mdl_AQ_comm_f <- lm(
  formula = F1_micro ~  AQ_total, 
  data = indiv_data,
  subset = experiment == "felt touch"
)

plot(mdl_AQ_comm_f)  # https://library.virginia.edu/data/articles/diagnostic-plots

(mdl_AQ_comm_f.aov <- car::Anova(mdl_AQ_comm_f))
# https://cran.r-project.org/web/packages/effectsize/vignettes/anovaES.html
effectsize::eta_squared(mdl_AQ_comm_f.aov)
coef(mdl_AQ_comm_f)
confint(mdl_AQ_comm_f)

mdl_AQ_comm_v <- lm(
  formula = F1_micro ~  AQ_total, 
  data = indiv_data,
  subset = experiment == "viewed touch"
)

plot(mdl_AQ_comm_v)
(mdl_AQ_comm_v.aov <- car::Anova(mdl_AQ_comm_v))
effectsize::eta_squared(mdl_AQ_comm_v.aov)


# Pleasantness from AQ, STQ ####

#. plots ####
plot_aq_pleas <- indiv_data %>% 
  ggplot(mapping = aes(y = Pleasantness, x = AQ_total)) +
  #facet_grid(. ~ experiment) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.5) +
  geom_point(mapping = aes(colour = group_mode, fill = group_mode)) +
  theme_light(base_size = 16) +
  exp_group_scales + 
  theme_nofacetbox +
  theme(legend.position = "none") +
  labs(x = "Autism Quotient (AQ)", y = "Pleasantness rating (VAS)") 

plot_stq_pleas <- indiv_data %>% 
  ggplot(mapping = aes(y = Pleasantness, x = STQ_total)) +
  #facet_grid(. ~ experiment) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 0.5) +
  geom_point(mapping = aes(colour = group_mode, fill = group_mode)) +
  theme_light(base_size = 16) +
  exp_group_scales + 
  theme_nofacetbox +
  theme(legend.position = "none") +
  labs(x = "Social Touch Questionnaire (STQ)", y = NULL) 

plot_aq_pleas + plot_stq_pleas

ggsave(paste0(FIGURES_FOLDER,"Pleasantness_AQ_STQ.svg"), width = 8, height = 4)


#. stats pleasantness ####

mdl_pleas <- lm(
  formula = Pleasantness ~ AQ_total + STQ_total, 
  data = indiv_data
)

# plot(mdl_pleas) 

(mdl_pleas.aov <- car::Anova(mdl_pleas, type = 2))
effectsize::eta_squared(mdl_pleas.aov, partial = TRUE)
coef(mdl_pleas)
confint(mdl_pleas)


# Sensitivity analyses: ####

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
