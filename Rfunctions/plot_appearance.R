library(ggplot2)
library(stringr)

# order to display in plots
ORDERED_CUES <- c('attention','love','happiness','calming','sadness','gratitude')

# RColorBrewer::brewer.pal(12, "Paired")
# RColorBrewer::display.brewer.pal(12, "Paired")
# library(unikn)
# seecol(pal = "pal_unikn_pair")

COLOUR_ASD_FELT <- "#008ECE"
COLOUR_ASD_VIEWED <- "#59C7EB"
COLOUR_CONTROL_FELT <- "#E0607E" 
COLOUR_CONTROL_VIEWED <- "#ECA0B2"
DODGE_WIDTH <- position_dodge(0.2)
ERRORBAR_WIDTH <- 0.4
ERRORBAR_SIZE <- 1.3
POINT_SIZE <- 6
POINT_SHAPE <- "\u2014" # unicode m-dash for horizontal line
HLINE_COLOUR <- 'grey'
HLINE_LINETYPE <- 'dashed'
HLINE_SIZE <- 1

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

theme_touch_comm_asd <- theme_light(base_size = 14) + 
  theme_x45deg + 
  theme_insidelegend(0.87,0.87)

scales_touch_comm_asd <- list(
  scale_color_manual(values = c(COLOUR_ASD_FELT, COLOUR_CONTROL_FELT, COLOUR_ASD_VIEWED, COLOUR_CONTROL_VIEWED)),
  scale_fill_manual(values = c(COLOUR_ASD_FELT, COLOUR_CONTROL_FELT, COLOUR_ASD_VIEWED, COLOUR_CONTROL_VIEWED)),
  scale_x_discrete(label = str_trunc(str_to_title(ORDERED_CUES),3,'right','')) 
)

scales_group_mode <- list(
  scale_color_manual(values = rep(c(COLOUR_ASD, COLOUR_CONTROL), 2)),
  scale_fill_manual(values = rep(c(COLOUR_ASD, COLOUR_CONTROL), 2)),
  scale_alpha_manual(values = rep(c(ALPHA_FELT, ALPHA_VIEWED), each = 2)),
  scale_linetype_manual(values = rep(c(LINETYPE_FELT, LINETYPE_VIEWED), each = 2)),
  scale_x_discrete(label = str_trunc(str_to_title(ORDERED_CUES),3,'right','')) 
)


# pleasantness appearance now orphaned, need to restore
# +
#   scale_y_continuous(breaks = c(-10,0,10), limits = c(-10,10), labels = c('unpleasant','','pleasant')) +
#   labs(y = 'Pleasantness rating (VAS)', x = NULL) +

f1_indiv_plot <- function(df) {
  df %>% 
    ggplot(aes(y = F1, x = Label, colour = group, fill = group)) +
    geom_hline(yintercept = df$F1_chance[1], 
               colour = HLINE_COLOUR, linetype = HLINE_LINETYPE, size = HLINE_SIZE) +
    stat_summary(geom = 'errorbar', fun.data = 'mean_cl_boot', 
                 width = ERRORBAR_WIDTH, size = ERRORBAR_SIZE,
                 position = DODGE_WIDTH) +
    stat_summary(geom = "point", fun= "mean",
                 size = POINT_SIZE, shape = POINT_SHAPE, 
                 position = DODGE_WIDTH) +
    scales_touch_comm_asd +
    theme_touch_comm_asd +
    scale_y_continuous(limits = c(0,1)) +
    labs(x = NULL, y = 'Agreement Score (F1)', colour = NULL, fill = NULL) +
    annotate("text", x = 1.5, y = 0.25, label = 'italic(chance)', parse = TRUE, colour = 'darkgrey')
}


f1_group_plot <- function(df) {
  df %>% 
    ggplot(mapping = aes(y = F1, x = Label, colour = group, fill = group)) +
    geom_hline(
      yintercept = df$F1_chance[1], 
      colour = HLINE_COLOUR, linetype = HLINE_LINETYPE, size = HLINE_SIZE
    ) +
    geom_errorbar(
      mapping = aes(ymin = F1_bCI_lower, ymax = F1_bCI_upper),
      width = ERRORBAR_WIDTH, size = ERRORBAR_SIZE, position = DODGE_WIDTH
    ) +
    geom_point(
      size = POINT_SIZE, shape = POINT_SHAPE, position = DODGE_WIDTH
    ) +
    scales_touch_comm_asd +
    theme_touch_comm_asd +
    scale_y_continuous(limits = c(0,1)) +
    labs(x = NULL, y = 'Agreement Score (F1)', colour = NULL, fill = NULL) +
    annotate("text", x = 1.5, y = 0.25, label = 'italic(chance)', parse = TRUE, colour = 'darkgrey')
}

f1_group_mode_plot <- function(df) {
  df %>% 
    ggplot(mapping = aes(
      y = F1, x = Label, 
      colour = group_mode, fill = group_mode #linetype = group_mode, alpha = group_mode, 
    )) +
    geom_hline(
      yintercept = df$F1_chance[1], 
      colour = HLINE_COLOUR, linetype = HLINE_LINETYPE, size = HLINE_SIZE
    ) +
    geom_errorbar(
      mapping = aes(ymin = F1_bCI_lower, ymax = F1_bCI_upper),
      width = ERRORBAR_WIDTH, size = ERRORBAR_SIZE, position = position_dodge(0.6)
    ) +
    geom_point(
      size = POINT_SIZE, shape = POINT_SHAPE, position = position_dodge(0.6)
    ) +
    scales_touch_comm_asd +
    theme_touch_comm_asd +
    scale_y_continuous(limits = c(0,1)) +
    labs(x = NULL, y = 'Agreement Score (F1)', colour = NULL, fill = NULL, alpha = NULL, linetype = NULL) +
    annotate("text", x = 1, y = 0.25, label = 'italic(chance)', parse = TRUE, colour = 'darkgrey')
}

f1_emm_plot <- function(df) {
  df %>% 
    ggplot(aes(y = prob, x = cued, colour = group, fill = group)) +
    geom_hline(yintercept = df$F1chance[1], 
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

pleasantness_plot <- function(df) {
  df %>% 
    ggplot(aes(y = response, x = cued, colour = group, fill = group)) +
    geom_hline(yintercept = 0, 
               colour = HLINE_COLOUR, linetype = HLINE_LINETYPE, size = HLINE_SIZE) +
    stat_summary(geom = 'errorbar', fun.data = 'mean_cl_boot', 
                 width = ERRORBAR_WIDTH, size = ERRORBAR_SIZE,
                 position = DODGE_WIDTH) +
    stat_summary(geom = "point", fun= "mean",
                 size = POINT_SIZE, shape = POINT_SHAPE, 
                 position = DODGE_WIDTH) +
    scales_touch_comm_asd +
    theme_touch_comm_asd +
    scale_y_continuous(
      breaks = c(-10,0,10), 
      limits = c(-10,10), 
      labels = c('unpleasant','','pleasant')
      ) +
    labs(y = 'Pleasantness rating (VAS)', x = NULL) 
}

