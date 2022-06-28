library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(afex)
library(emmeans)
library(psych)
library(patchwork)
library(parallel)
install.packages("svglite")
library(svglite)

# source all .R files in the Rfunctions directory
sapply(list.files("Rfunctions", full.names = TRUE), source)

#### read data ####

online_comm_data <- read_csv('Data/primary/online_comm_recoded.csv') %>% 
  filter(task == 'forced choice') %>% 
  select(-task) %>% 
  mutate(experiment = "viewed touch")


#### read performance metrics ####

online_performance_data <- read_csv('Data/processed/online-comm_performance-group.csv', col_types = cols()) %>% 
  mutate(cued = factor(cued, levels = orderedCues))


### plot performance - no statistical model ##

online_performance_data %>% 
  ggplot(mapping = aes(x = cued, y = F1, colour = group)) +
  geom_point()

#### generalized linear model ####
set_sum_contrasts()
theme_set(theme_light())

glm1 <- glm(F1 ~ group+cued,
            data = online_performance_data, 
           # method = 'LRT',
            #control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e9)),
            family = binomial, weights=online_performance_data$Total )

summary(glm1)

# report
anova(glm1)

emmeans(glm1, ~ cued, type = 'response')
emmeans(glm1,  ~  group, type = 'response')
(emm <- emmeans(glm1,  ~  group + cued))
plot(emm, comparisons = TRUE, adjust = 'holm', by = 'group') # quick look

# report
pairs(emm, simple = 'group', adjust = 'holm', type = 'response', infer = TRUE)
emmeans(glm1, ~ cued) %>% as_tibble() %>% arrange(-emmean) %>% pull(cued)


### figure compare ####

Ns <- online_comm_data %>% 
  group_by(group) %>% 
  tally() 

N.ASD <- Ns %>% filter(group == 'ASD') %>% pull(n)
N.Control <- Ns %>% filter(group == 'Control') %>% pull(n)

emmeans(glm1,  ~  group + cued, type = 'response') %>%
  as_tibble() %>% 
  mutate(group = recode(group, 
                        ASD = paste0('ASD (n = ',N.ASD,')'), 
                        Control = paste0('Control (n = ',N.Control,')') ) ) %>% 
  mutate(group = as.factor(group),
         cued = factor(cued, levels = orderedCues)) %>% 
  ggplot(aes(y = prob, x = cued, colour = group, fill = group)) +
  geom_hline(yintercept = online_performance_data$F1chance[1], 
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
  labs(x = NULL, y = 'Performance F1', colour = NULL, fill = NULL) +
  theme_light(base_size = 14) + theme_x45deg + theme_insidelegend(0.85,0.85) +
  annotate("text", x = 1.5, y = 0.25, label = 'italic(chance)', parse = TRUE, colour = 'darkgrey') -> compare.plot

#### confusion matrices ####

###. figure online ASD ####
online_comm_data %>%
  filter(group == 'ASD') %>% 
  confusion_matrix_data() %>%
  confusion_matrix_plot(colour.ASD, ylabels = c(orderedCues,'other')) -> confmat.online.ASD


###. figure online Control ####
online_comm_data %>%
  filter(group == 'Control') %>% 
  confusion_matrix_data() %>%
  confusion_matrix_plot(colour.Control, ylabels = c(orderedCues,'other')) -> confmat.online.Control


#### combine figures with patchwork ####

if ( !dir.exists('figures') ) { dir.create('figures') }

design.compare = '
AAAB
AAAC
'

open_plot_window(width = 10.5, height = 5.7); plot(1:10)
compare.plot + confmat.online.ASD + confmat.online.Control +
  plot_annotation(tag_levels = 'A',
                  title= "Online Viewed Touch") +
  plot_layout(design = design.compare) 
ggsave('figures/Compare_online-ASD-vs-Control.svg')
ggsave('figures/Compare_online-ASD-vs-Control.pdf')



