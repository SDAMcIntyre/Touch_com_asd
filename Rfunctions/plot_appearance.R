library(ggplot2)

# order to display in plots
ORDERED_CUES <- c('attention','love','happiness','calming','sadness','gratitude')

COLOUR_ASD <-  '#377EB8' #blue
COLOUR_CONTROL <- "#E69F00" #yellow

theme_x45deg <- theme(
  axis.text.x=element_text(angle=45, hjust = 1 ))

theme_insidelegend <- function(x,y) {
  theme(legend.position = c(x,y), 
        legend.background = element_blank(),
        legend.key = element_rect(colour = 'grey'))}

theme_confmat_legend <- theme(legend.margin=margin(0,0,0,0),
                              legend.box.margin=margin(t = 0, r = 0, b = 0,l = -10), 
                              legend.text=element_text(size=8))

theme_nofacetbox <- theme(strip.background = element_blank(), 
                          strip.text = element_text(colour = 'black'))


# theme_basic <- theme_bw() +theme(panel.border = element_blank(), axis.line = element_line(colour = 'black'))
# 
# theme_nolegend <- theme(legend.position = "none")
# 
# theme_bwstrip <- theme(strip.background = element_rect(fill = 'white',colour = 'grey',size = 1), 
#                        strip.text = element_text(colour = 'black'))
# 


pleasantness_plot <- function(df) {
  df %>% 
    ggplot(aes(y = response, x = cued, colour = group, fill = group)) +
    geom_hline(yintercept = 0, 
               colour = 'grey', linetype = 'dashed', size = 1) +
    stat_summary(geom = 'errorbar', fun.data = 'mean_cl_normal', 
                 width = 0.4, size = 1.3,
                 position = position_dodge(0.2)) +
    stat_summary(geom = "point", fun= "mean",
                 size = 6, shape = "\u2014", # unicode m-dash for horizontal line
                 position = position_dodge(0.2)) +
    scale_color_manual(values = c(COLOUR_ASD, COLOUR_CONTROL)) +
    scale_fill_manual(values = c(COLOUR_ASD, COLOUR_CONTROL)) +
    scale_x_discrete(label = str_trunc(str_to_title(ORDERED_CUES),3,'right','')) +
    scale_y_continuous(breaks = c(-10,0,10), limits = c(-10,10), labels = c('unpleasant','','pleasant')) +
    theme_x45deg + 
    labs(y = 'Pleasantness rating (VAS)', x = NULL) +
    theme_light(base_size = 14) + theme_x45deg + theme_insidelegend(0.87,0.87) 
}

f1_plot <- function(df) {
  df %>% 
    ggplot(aes(y = F1, x = cued, colour = group, fill = group)) +
    geom_hline(yintercept = live_performance_data$F1chance[1], 
               colour = 'grey', linetype = 'dashed', size = 1) +
    stat_summary(geom = 'errorbar', fun.data = 'mean_cl_normal', 
                 width = 0.4, size = 1.3,
                 position = position_dodge(0.2)) +
    stat_summary(geom = "point", fun= "mean",
                 size = 6, shape = "\u2014", # unicode m-dash for horizontal line
                 position = position_dodge(0.2)) +
    scale_color_manual(values = c(COLOUR_ASD, COLOUR_CONTROL)) +
    scale_fill_manual(values = c(COLOUR_ASD, COLOUR_CONTROL)) +
    scale_x_discrete(label = str_trunc(str_to_title(ORDERED_CUES),3,'right','')) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x = NULL, y = 'Agreement Score (F1)', colour = NULL, fill = NULL) +
    theme_light(base_size = 14) + theme_x45deg + theme_insidelegend(0.85,0.85) +
    annotate("text", x = 1.5, y = 0.25, label = 'italic(chance)', parse = TRUE, colour = 'darkgrey')
}

f1_emm_plot <- function(df) {
  df %>% 
    ggplot(aes(y = prob, x = cued, colour = group, fill = group)) +
    geom_hline(yintercept = live_performance_data$F1chance[1], 
               colour = 'grey', linetype = 'dashed', size = 1) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                  width = 0.4, size = 1.3,
                  position = position_dodge(0.2)) +
    geom_point(size = 6, shape = "\u2014", # unicode m-dash for horizontal line
               position = position_dodge(0.2)) +
    scale_color_manual(values = c(COLOUR_ASD, COLOUR_CONTROL)) +
    scale_fill_manual(values = c(COLOUR_ASD, COLOUR_CONTROL)) +
    scale_x_discrete(label = str_trunc(str_to_title(ORDERED_CUES),3,'right','')) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x = NULL, y = 'Agreement Score (F1)', colour = NULL, fill = NULL) +
    theme_light(base_size = 14) + theme_x45deg + theme_insidelegend(0.85,0.85) +
    annotate("text", x = 1.5, y = 0.25, label = 'italic(chance)', parse = TRUE, colour = 'darkgrey')
}