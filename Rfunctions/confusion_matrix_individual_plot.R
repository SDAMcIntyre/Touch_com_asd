confusion_matrix_individual_plot <- function(df, grad.colour, grad.limit = c(0,100), ylabels, abr = 1) {
  df %>% 
    mutate(cued = factor(cued, levels = ORDERED_CUES),
           response = factor(response, levels = rev(ylabels))) %>%
    ggplot(aes(x=cued, y=response, fill=Percent)) +
    geom_tile(color="black",size=0.1) +
    scale_fill_gradient(name =' %', na.value = 'white', low='white', high=grad.colour, 
                        guide = 'legend', limits = grad.limit) + 
    scale_x_discrete(label = str_trunc(str_to_title(ORDERED_CUES),abr,'right','')) +
    scale_y_discrete(label = str_trunc(str_to_title(rev(ylabels)),abr,'right','')) +
    theme_classic() + theme_confmat_legend + 
    theme_nofacetbox + theme(axis.line = element_blank(), axis.ticks = element_blank()) +
    labs(x = 'Touch expression', y = 'Assigned meaning')
}