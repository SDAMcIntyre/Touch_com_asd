# order to display in plots
orderedCues <- c('attention','love','happiness','calming','sadness','gratitude')

colour.ASD <-  '#377EB8' #blue
colour.Control <- "#E69F00" #yellow

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
