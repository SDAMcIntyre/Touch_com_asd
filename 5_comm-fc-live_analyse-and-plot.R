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

#### read data ####
live_comm_data <- read_csv('Data/primary/live-comm_collated.csv', col_types = cols()) %>% 
  mutate(experiment = "felt touch")

#### read performance metrics ####

live_performance_data <- read_csv('Data/processed/live-comm_performance-indiv.csv', col_types = cols()) %>% 
  mutate(cued = factor(cued, levels = orderedCues))

#### mixed effects model ####
set_sum_contrasts()
theme_set(theme_light())

mm <- mixed(F1 ~ group*cued + (1|PID),
            data = live_performance_data, 
            method = 'LRT',
            family = binomial, weights=live_performance_data$Total )

mm.pb <- mixed(F1 ~ group*cued + (1|PID),
            data = live_performance_data, 
            method = 'PB',
            family = binomial, weights=live_performance_data$Total )

#summary(mm)
#anova(mm)

#### effect of group ####
emmeans(mm, ~ group , type = "response")
anova(mm)[1,]

#### effect of cue ####
anova(mm)[2,]
# order of agreement (for presentation purposes)
emmeans(mm, ~ cued) %>% as_tibble() %>% arrange(-emmean) %>% pull(cued)

#### group/cue interaction ####
anova(mm)[3,]
pairs(emm, simple = 'group', adjust = 'holm', type = 'response', infer = TRUE)


#### vs. chance ####

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

### figure compare ####

Ns <- live_performance_data %>% 
  group_by(group,PID) %>% 
  tally() %>% tally()

N.ASD <- Ns %>% filter(group == 'ASD') %>% pull(n)
N.Control <- Ns %>% filter(group == 'Control') %>% pull(n)

emmeans(mm,  ~  group + cued, type = 'response') %>%
  as_tibble() %>% 
  mutate(group = recode(group, 
                        ASD = paste0('ASD (n = ',N.ASD,')'), 
                        Control = paste0('Control (n = ',N.Control,')') ) ) %>% 
  mutate(group = as.factor(group),
         cued = factor(cued, levels = orderedCues)) %>% 
  ggplot(aes(y = prob, x = cued, colour = group, fill = group)) +
  geom_hline(yintercept = live_performance_data$F1chance[1], 
             colour = 'grey', linetype = 'dashed', size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.4, size = 1.3,
                position = position_dodge(0.2)) +
  geom_point(size = 6, shape = "\u2014", # unicode m-dash for horizontal line
             position = position_dodge(0.2)) +
  scale_color_manual(values = c(colour.ASD, colour.Control)) +
  scale_fill_manual(values = c(colour.ASD, colour.Control)) +
  scale_x_discrete(label = str_trunc(str_to_title(orderedCues),3,'right','')) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = NULL, y = 'Agreement Score (F1)', colour = NULL, fill = NULL) +
  theme_light(base_size = 14) + theme_x45deg + theme_insidelegend(0.85,0.85) +
  annotate("text", x = 1.5, y = 0.25, label = 'italic(chance)', parse = TRUE, colour = 'darkgrey') -> compare.plot

#### confusion matrices ####

###. figure live ASD ####
live_comm_data %>%
  filter(group == 'ASD') %>% 
  confusion_matrix_data() %>%
  confusion_matrix_plot(colour.ASD, ylabels = c(orderedCues,'other')) -> confmat.live.ASD

###. figure live ASD individual ####
orderedPIDs.liveASD <- live_comm_data %>% 
  filter(group == 'ASD') %>% 
  order_PIDs(PID)
live_comm_data %>%
  filter(group == 'ASD') %>% 
  confusion_matrix_data(PID)  %>%
  mutate(PID = factor(PID, levels = orderedPIDs.liveASD)) %>% 
  confusion_matrix_individual_plot(colour.ASD, ylabels = c(orderedCues,'other')) +
  facet_wrap(. ~ PID, nrow = 5) -> confmat.live.ASD.ind

###. figure live Control ####
live_comm_data %>%
  filter(group == 'Control') %>% 
  confusion_matrix_data() %>%
  confusion_matrix_plot(colour.Control, ylabels = c(orderedCues,'other')) -> confmat.live.Control

###. figure live Control individual ####
orderedPIDs.liveControl <- live_comm_data %>%
  filter(group == 'Control') %>% 
  order_PIDs(PID)
live_comm_data %>%
  filter(group == 'Control') %>% 
  confusion_matrix_data(PID)  %>%
  mutate(PID = factor(PID, levels = orderedPIDs.liveControl)) %>% 
  confusion_matrix_individual_plot(colour.Control, ylabels = c(orderedCues,'other')) +
  facet_wrap(. ~ PID, nrow = 5) -> confmat.live.Control.ind

#### combine figures ####

if ( !dir.exists('Figures') ) { dir.create('Figures') }

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
