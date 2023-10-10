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
  ) %>% 
  filter(task == 'forced choice') 

# read performance metrics ####

performance_felt <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, 'comm-felt-touch_performance-indiv.csv'), 
  col_types = cols()
  ) 

performance_group <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, "comm_performance-group.csv"),
  col_types = cols()
)

performance_first6 <- read_csv(
  paste0(PROCESSED_DATA_FOLDER, "comm_performance-group-first6.csv"),
  col_types = cols()
)

# Ns ####

Ns <- comm_data %>% 
  group_by(experiment, group,PID) %>% 
  tally() %>% tally()

# check it's the same
# performance_felt %>%
#   group_by(group, PID) %>%
#   tally() %>% tally()

N_felt_ASD <- Ns %>% filter(experiment == "felt touch" & group == 'ASD') %>% pull(n)
N_felt_Control <- Ns %>% filter(experiment == "felt touch" & group == 'Control') %>% pull(n)
N_viewed_ASD <- Ns %>% filter(experiment == "viewed touch" & group == 'ASD') %>% pull(n)
N_viewed_Control <- Ns %>% filter(experiment == "viewed touch" & group == 'Control') %>% pull(n)

# figures without stats ####

#. felt touch F1-individual ####
performance_felt %>%
  mutate(
    group = recode(
      group,
      ASD = paste0('ASD (n = ',N_felt_ASD,')'),
      Control = paste0('Control (n = ',N_felt_Control,')') 
      ) 
    ) %>% 
  mutate(
    group = as.factor(group),
    Label = factor(Label, levels = ORDERED_CUES)
    ) %>% 
  f1_indiv_plot()

# #. felt touch F1 group ####
# 
# performance_group %>%
#   filter(experiment == "felt touch") %>% 
#   mutate(
#     group = recode(
#       group,
#       ASD = paste0('ASD (n = ',N_felt_ASD,')'),
#       Control = paste0('Control (n = ',N_felt_Control,')') 
#     ) 
#   ) %>% 
#   mutate(
#     group = as.factor(group),
#     Label = factor(Label, levels = ORDERED_CUES)
#   ) %>% 
#   f1_group_plot()
# 
# 
# #. viewed touch F1 ####
# performance_group %>%
#   filter(experiment == 'viewed touch') %>% 
#   mutate(
#     group = recode(
#       group,
#       ASD = paste0('ASD (n = ',N_viewed_ASD,')'),
#       Control = paste0('Control (n = ',N_viewed_Control,')') 
#     ) 
#   ) %>% 
#   mutate(
#     group = as.factor(group),
#     Label = factor(Label, levels = ORDERED_CUES)
#   ) %>% 
#   f1_group_plot()


#. felt vs. viewed touch F1 FIRST 6 ####
performance_first6 %>%
  mutate(
    group_mode = recode(
      paste(group, experiment),
      `ASD felt touch` = paste0('Felt, Autistic (n = ',N_felt_ASD,')'),
      `Control felt touch` = paste0('Felt, Control (n = ',N_felt_Control,')'),
      `ASD viewed touch` = paste0('Viewed, Autistic (n = ',N_viewed_ASD,')'),
      `Control viewed touch` = paste0('Viewed, Control (n = ',N_viewed_Control,')')
    )
  ) %>%
  mutate(
    group_mode = as.factor(group_mode),
    Label = factor(Label, levels = ORDERED_CUES)
  ) %>% 
  f1_group_mode_plot()

# models ####
set_sum_contrasts()
theme_set(theme_light())

#. FELT TOUCH mixed effects ####

# slow to run, so load from earlier; (~ 2 hours on MacBook Pro 2020 2.3 GHz Quad-Core Intel Core i7)

# ----- load from earlier
load(paste0(PROCESSED_DATA_FOLDER, "comm-felt-touch-F1_mm-pb.RData"))

# ----- run model
perf_data_felt <- performance_data %>% filter(experiment == 'felt touch')
cl <- makeCluster(rep("localhost", detectCores()))
set.seed(1409)
mm_felt <- mixed(
  F1 ~ group*cued + (1|PID),
  data = perf_data_felt,
  method = 'PB',
  args_test = list(nsim = 1000, cl = cl),
  family = binomial,
  weights=perf_data_felt$Total
  )
# save so you can load next time
save(mm_felt, file = paste0(PROCESSED_DATA_FOLDER, "comm-felt-touch-F1_mm-pb.RData"))
#----- end run model

#.. effect of group ####
emmeans(mm_felt, ~ group , type = "response")
anova(mm_felt)[1,]

#.. effect of expression ####
anova(mm_felt)[2,]
# order of agreement (for presentation purposes)
emmeans(mm_felt, ~ cued) %>% as_tibble() %>% arrange(-emmean) %>% pull(cued)

#.. group/cue interaction ####
anova(mm_felt)[3,]

#.. vs. chance ####

( vs.chance <- emmeans(mm_felt, ~ group + cued) %>% 
        as_tibble() %>% 
    mutate(
      emmean.p = logistic(emmean),
      chance = perf_data_felt$F1chance[1],
      z.vs.chance = (emmean - logit(chance))/(emmean*SE),
      p.vs.chance = pt(abs(z.vs.chance), df = df, lower.tail = FALSE),
      p.holm = p.adjust(p.vs.chance, method = 'holm'),
      sig = if_else(p.holm < 0.05, "*", "")
      ) )

#.. supplementary table
formattable(vs.chance, digits = 2, format = "f")

#.. pairwise control vs ASD by cue
emm_felt <- emmeans(mm_felt, ~ group + cued)
pairs(emm_felt, simple = 'group', adjust = 'holm', type = 'response', infer = TRUE)


#.. figure compare ####

emmeans(mm_felt,  ~  group + cued, type = 'response') %>%
  as_tibble() %>% 
  mutate(group = recode(group, 
                        ASD = paste0('ASD (n = ',N.ASD,')'), 
                        Control = paste0('Control (n = ',N.Control,')') ) ) %>% 
  mutate(group = as.factor(group),
         cued = factor(cued, levels = ORDERED_CUES)) %>% 
  f1_emm_felt_plot() -> plot_felt_ASD_vs_Control

#. VIEWED TOUCH mixed model ####

# slow to run, so load from earlier; (~ 2 hours on MacBook Pro 2020 2.3 GHz Quad-Core Intel Core i7)

# ----- load from earlier
#load(paste0(PROCESSED_DATA_FOLDER, "comm-viewed-touch-F1_mm-pb.RData"))

# ----- run model
perf_data_viewed <- performance_data %>% 
  filter(experiment == 'viewed touch') %>% 
  mutate(
    group = as.factor(group),
    cued = factor(cued, levels = ORDERED_CUES)
  )

set.seed(230510)
mm_viewed <- mixed(
  F1 ~ group*cued + (1|PID),
  data = perf_data_viewed,
  method = 'PB',
  args_test = list(nsim = 1000, cl = cl),
  family = binomial,
  weights=perf_data_viewed$Total
)

# save so you can load next time
save(mm_felt, file = paste0(PROCESSED_DATA_FOLDER, "comm-viewed-touch-F1_mm-pb.RData"))
#----- end run model

# glm model

glm_viewed <- glm(
  formula = F1 ~ group * cued, 
  family = binomial, 
  data = perf_data_viewed,
  weights = perf_data_viewed$Total
  )

anova(glm_viewed)

#.. effect of group ####
emmeans(glm_viewed, ~ group , type = "response")
anova(mm_viewed)[1,]

#.. effect of expression ####
anova(mm_viewed)[2,]
# order of agreement (for presentation purposes)
emmeans(mm_viewed, ~ cued) %>% as_tibble() %>% arrange(-emmean) %>% pull(cued)

#.. group/cue interaction ####
anova(mm_viewed)[3,]

#.. vs. chance ####

( vs.chance <- emmeans(mm_viewed, ~ group + cued) %>% 
    as_tibble() %>% 
    mutate(
      emmean.p = logistic(emmean),
      chance = perf_data_viewed$F1chance[1],
      z.vs.chance = (emmean - logit(chance))/(emmean*SE),
      p.vs.chance = pt(abs(z.vs.chance), df = df, lower.tail = FALSE),
      p.holm = p.adjust(p.vs.chance, method = 'holm'),
      sig = if_else(p.holm < 0.05, "*", "")
    ) )

#.. supplementary table
formattable(vs.chance, digits = 2, format = "f")

#.. pairwise control vs ASD by cue
emm_viewed <- emmeans(mm_viewed, ~ group + cued)
pairs(emm_viewed, simple = 'group', adjust = 'holm', type = 'response', infer = TRUE)


#.. figure compare ####

emmeans(mm_viewed,  ~  group + cued, type = 'response') %>%
  as_tibble() %>% 
  mutate(group = recode(group, 
                        ASD = paste0('ASD (n = ',N.ASD,')'), 
                        Control = paste0('Control (n = ',N.Control,')') ) ) %>% 
  mutate(group = as.factor(group),
         cued = factor(cued, levels = ORDERED_CUES)) %>% 
  f1_emm_plot() -> plot_viewed_ASD_vs_Control





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
plot_felt_ASD_vs_Control + confmat.live.ASD + confmat.live.Control +
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
