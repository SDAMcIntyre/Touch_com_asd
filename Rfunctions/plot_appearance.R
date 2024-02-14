library(ggplot2)
library(stringr)

# order to display in plots
ORDERED_CUES <- c('attention','love','happiness','calming','sadness','gratitude')

COLOUR_ATTENTION <- "#0A75AD"
COLOUR_LOVE <- "#FF7F50"
COLOUR_HAPPINESS <- "#2ACAEA"
COLOUR_CALMING <- "#008000"
COLOUR_SADNESS <- "#FF1493"
COLOUR_GRATITUDE <- "#B30000"

# RColorBrewer::brewer.pal(12, "Paired")
# RColorBrewer::display.brewer.pal(12, "Paired")
# unikn::seecol(pal = "pal_unikn_pair")

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

theme_nofacets <- theme_nofacetbox + theme(strip.text.x = element_text(size=0))

theme_touch_comm_asd <- theme_light(base_size = 14) + 
  theme_x45deg + 
  theme_insidelegend(0.84,0.87)

exp_group_scales <- list(
  scale_color_manual(name = NULL, values = c(COLOUR_ASD_FELT, COLOUR_CONTROL_FELT, COLOUR_ASD_VIEWED, COLOUR_CONTROL_VIEWED)),
  scale_fill_manual(name = NULL, values = c(COLOUR_ASD_FELT, COLOUR_CONTROL_FELT, COLOUR_ASD_VIEWED, COLOUR_CONTROL_VIEWED))
)

viewed_only_scales <- list(
  scale_color_manual(name = NULL, values = c(COLOUR_ASD_VIEWED, COLOUR_CONTROL_VIEWED)),
  scale_fill_manual(name = NULL, values = c(COLOUR_ASD_VIEWED, COLOUR_CONTROL_VIEWED))
)

x_touch_labels <- scale_x_discrete(label = str_trunc(str_to_title(ORDERED_CUES),3,'right','')) 


# questionnaire boxplots by group ####

questionnaire_plot <- function(df, y) {
  df %>% 
    # for plot colours
    mutate(
      group_mode = recode(
        paste(group, experiment),
        `ASD felt touch` = paste0('Felt, Autistic'), 
        `Control felt touch` = paste0('Felt, Control'), 
        `ASD viewed touch` = paste0('Viewed, Autistic'), 
        `Control viewed touch` = paste0('Viewed, Control') 
      )
    ) %>% 
    ggplot(mapping = aes(x = group_mode, y = {{y}}, fill = group_mode)) +
    geom_boxplot() +
    theme_light(base_size = 16) +
    theme_x45deg + 
    theme(legend.position = "none") 
}

questionnaire_plot_combined <- function(df, y, ylab) {
  y_scale <- c(min(df[[y]], na.rm = TRUE), max(df[[y]], na.rm = TRUE)) 
  
  forced <- df %>% 
    filter(task == "forced choice") %>% 
    questionnaire_plot(.data[[y]]) +
    exp_group_scales +
    scale_y_continuous(limits = y_scale) +
    labs(x = NULL, y = ylab, title = "Forced choice")
  
  free <- df %>% 
    filter(task == "free text") %>% 
    questionnaire_plot(.data[[y]]) +
    viewed_only_scales +
    scale_y_continuous(limits = y_scale) +
    labs(x = NULL, y = NULL, title = "Free text")
  
  forced + free + plot_layout(widths = c(2,1))
}

