confusion_matrix_plot <- function(df, grad.colour, grad.limit = c(0,100), ylabels, abr = 3) {
  df %>% 
    mutate(cued = factor(cued, levels = orderedCues),
           response = factor(response, levels = rev(ylabels))) %>%
    ggplot(aes(x=cued, y=response, fill=Percent)) +
    geom_tile(color="black",size=0.1) +
    geom_text(aes(label=round_integer(respFreq)), size=3, colour="black") +
    scale_fill_gradient(name =' %', na.value = 'white', low='white', high=grad.colour, 
                        guide = 'legend', limits = grad.limit) + 
    scale_x_discrete(label = str_trunc(str_to_title(orderedCues),abr,'right','')) +
    scale_y_discrete(label = str_trunc(str_to_title(rev(ylabels)),abr,'right','')) +
    theme_classic() + theme_x45deg + theme_confmat_legend + 
    theme_nofacetbox + theme(axis.line = element_blank()) +
    labs(x = 'Cued word', y = 'Receiver response')
}