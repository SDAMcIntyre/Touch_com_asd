library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(afex)
library(emmeans)
library(psych)
library(patchwork)
library(parallel)
library(formattable)

# source all .R files in the Rfunctions directory
sapply(list.files("Rfunctions", full.names = TRUE), source)

# input path ####
PROCESSED_DATA_FOLDER <- "Data/processed/"

# output paths ####
FIGURES_FOLDER <- "Figures/"
if ( !dir.exists(FIGURES_FOLDER) ) { dir.create(FIGURES_FOLDER) }

# read data ####
comm_data <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'communication-data.csv'),
  col_types = "cccccccccc"
  )

# read performance metrics ####

performance_data <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'comm_performance-indiv.csv'), 
  col_types = cols()
  ) 

# Ns ####

Ns <- comm_data %>% 
  group_by(experiment, group,PID) %>% 
  tally() %>% tally()

# check it's the same
performance_data %>% 
  group_by(experiment, group, PID) %>% 
  tally() %>% tally()

N_felt_ASD <- Ns %>% filter(experiment == "felt touch" & group == 'ASD') %>% pull(n)
N_felt_Control <- Ns %>% filter(experiment == "felt touch" & group == 'Control') %>% pull(n)
N_viewed_ASD <- Ns %>% filter(experiment == "viewed touch" & group == 'ASD') %>% pull(n)
N_viewed_Control <- Ns %>% filter(experiment == "viewed touch" & group == 'Control') %>% pull(n)


# figure without stats ####

Ns <- live_performance_data %>% 
  group_by(group,PID) %>% 
  tally() %>% tally()

N.ASD <- Ns %>% filter(group == 'ASD') %>% pull(n)
N.Control <- Ns %>% filter(group == 'Control') %>% pull(n)

performance_data %>%
  filter(experiment == 'felt touch') %>% 
  mutate(
    group = recode(
      group,
      ASD = paste0('ASD (n = ',N.ASD,')'),
      Control = paste0('Control (n = ',N.Control,')') 
      ) 
    ) %>% 
  mutate(
    group = as.factor(group),
    cued = factor(cued, levels = ORDERED_CUES)
    ) %>% 
  f1_plot()


# mixed effects model ####
set_sum_contrasts()
theme_set(theme_light())

# slow to run, so load from earlier; (~ 2 hours on MacBook Pro 2020 2.3 GHz Quad-Core Intel Core i7)

# ----- load from earlier
load(paste0(PROCESSED_DATA_FOLDER, "comm-felt-touch-F1_mm-pb.RData"))

# ----- run model
cl <- makeCluster(rep("localhost", detectCores()))
set.seed(1409)
mm <- mixed(
  F1 ~ group*cued + (1|PID),
  data = live_performance_data,
  method = 'PB',
  args_test = list(nsim = 1000, cl = cl),
  family = binomial,
  weights=live_performance_data$Total
  )
# save so you can load next time
save(mm, file = paste0(PROCESSED_DATA_FOLDER, "comm-felt-touch-F1_mm-pb.RData"))
#----- end run model


#. effect of group ####
emmeans(mm, ~ group , type = "response")
anova(mm)[1,]

#. effect of expression ####
anova(mm)[2,]
# order of agreement (for presentation purposes)
emmeans(mm, ~ cued) %>% as_tibble() %>% arrange(-emmean) %>% pull(cued)

#. group/cue interaction ####
anova(mm)[3,]

#. vs. chance ####

( vs.chance <- emmeans(mm, ~ group + cued) %>% 
        as_tibble() %>% 
    mutate(
      emmean.p = logistic(emmean),
      chance = live_performance_data$F1chance[1],
      z.vs.chance = (emmean - logit(chance))/(emmean*SE),
      p.vs.chance = pt(abs(z.vs.chance), df = df, lower.tail = FALSE),
      p.holm = p.adjust(p.vs.chance, method = 'holm'),
      sig = if_else(p.holm < 0.05, "*", "")
      ) )

# supplementary table
formattable(vs.chance, digits = 2, format = "f")

#. pairwise control vs ASD by cue
emm <- emmeans(mm, ~ group + cued)
pairs(emm, simple = 'group', adjust = 'holm', type = 'response', infer = TRUE)


#. figure compare ####

emmeans(mm,  ~  group + cued, type = 'response') %>%
  as_tibble() %>% 
  mutate(group = recode(group, 
                        ASD = paste0('ASD (n = ',N.ASD,')'), 
                        Control = paste0('Control (n = ',N.Control,')') ) ) %>% 
  mutate(group = as.factor(group),
         cued = factor(cued, levels = ORDERED_CUES)) %>% 
  f1_emm_plot() -> compare.plot

# confusion matrices ####

#. figure live ASD ####
live_comm_data %>%
  filter(group == 'ASD') %>% 
  confusion_matrix_data() %>%
  confusion_matrix_plot(COLOUR_ASD, ylabels = c(ORDERED_CUES,'other')) -> confmat.live.ASD

#. figure live ASD individual ####
orderedPIDs.liveASD <- live_comm_data %>% 
  filter(group == 'ASD') %>% 
  order_PIDs(PID)

live_comm_data %>%
  filter(group == 'ASD') %>% 
  confusion_matrix_data(PID)  %>%
  mutate(PID = factor(PID, levels = orderedPIDs.liveASD)) %>% 
  confusion_matrix_individual_plot(COLOUR_ASD, ylabels = c(ORDERED_CUES,'other')) +
  facet_wrap(. ~ PID, nrow = 5) -> confmat.live.ASD.ind

#. figure live Control ####
live_comm_data %>%
  filter(group == 'Control') %>% 
  confusion_matrix_data() %>%
  confusion_matrix_plot(COLOUR_CONTROL, ylabels = c(ORDERED_CUES,'other')) -> confmat.live.Control

#. figure live Control individual ####
orderedPIDs.liveControl <- live_comm_data %>%
  filter(group == 'Control') %>% 
  order_PIDs(PID)

live_comm_data %>%
  filter(group == 'Control') %>% 
  confusion_matrix_data(PID)  %>%
  mutate(PID = factor(PID, levels = orderedPIDs.liveControl)) %>% 
  confusion_matrix_individual_plot(COLOUR_CONTROL, ylabels = c(ORDERED_CUES,'other')) +
  facet_wrap(. ~ PID, nrow = 5) -> confmat.live.Control.ind

# combine figures ####

design.compare = '
AAAB
AAAC
'

open_plot_window(width = 10.5, height = 5.7); plot(1:10)
compare.plot + confmat.live.ASD + confmat.live.Control +
  plot_annotation(tag_levels = 'A') +
  plot_layout(design = design.compare) 
ggsave('Figures/comm-fc-live_Compare_ASD-vs-Control.svg')
ggsave('Figures/comm-fc-live_Compare_ASD-vs-Control.pdf')

design.confmat.ind = '
AB
'

open_plot_window(width = 11.3, height = 6.5); plot(1:10)
confmat.live.ASD.ind +labs(title = 'ASD') + 
  confmat.live.Control.ind +labs(title = 'Control') +
  plot_annotation(tag_levels = 'A') +
  plot_layout(design = design.confmat.ind) 

ggsave('Figures/comm-fc-live_confmat-individual.svg')
ggsave('Figures/comm-fc-live_confmat-individual.pdf')
