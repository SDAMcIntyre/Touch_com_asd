library(readxl)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel) # for labels

# source all .R files in the Rfunctions directory ####
source_files <- list.files("Rfunctions", full.names = TRUE)
sapply(source_files[grepl(
  "(get_unit_correlation)|(get_max_firing_rates)|(plot_appearance)", 
  source_files
)], source)
###################################################################
# READ DATA 
# read excel file with pleasentness ratings
ratings_data_file_path <- "Data/primary/live_pleas-data.csv"
ratings_data_all <- read.csv(ratings_data_file_path)
# get min and max values of pleasentness ratings
min_rating <- min(ratings_data_all$response)
max_rating <- max(ratings_data_all$response)

# get min and max values of pleasentness ratings fot the plot
min_plot_rating <- -8
max_plot_rating <- 8

# change cued column name to the more meaningful: Stimulus
# Example assuming "cued" is an existing column
colnames(ratings_data_all)[colnames(ratings_data_all) == "cued"] <- "Stimulus"
# separate data to control and asd
ratings_data_all |> 
  filter(group == "Control") -> ratings_data_control
ratings_data_all |> 
  filter(group == "ASD") -> ratings_data_asd


# read excel file with isi data and calculate max iff data
iff_data_file_path <- "Data/primary/ALL_ISIs_after_exclusions.xlsx"
iff_data <- get_max_firing_rates(iff_data_file_path)
all_unit_types <- unique(iff_data$UnitType)
###################################################################
unit_type <- "MS"

# create dataframe with means for MS
# control
ms_control <- get_joined_by_stimulus_df(unit_type, iff_data, ratings_data_control)
# add column with unit type
ms_control$UnitType <- rep(unit_type, nrow(ms_control)) 
# add column with group
ms_control$Group <- rep("Control", nrow(ms_control))
# run correlation
ms_control |>
  mutate(R = cor(mean.ratings,
                   mean.iff,
                   method = "spearman",
                   use = "na.or.complete")) -> ms_control

# asd data
ms_asd <- get_joined_by_stimulus_df(unit_type, iff_data, ratings_data_asd)
# add column with unit type
ms_asd$UnitType <- rep(unit_type, nrow(ms_asd)) 
# add column with group
ms_asd$Group <- rep("ASD", nrow(ms_asd)) 
# run correlation
ms_asd |>
  mutate(R = cor(mean.ratings,
                   mean.iff,
                   method = "spearman",
                   use = "na.or.complete")) -> ms_asd
#####################################################################
unit_type <- "CT"
# create dataframe with means for CT
# control
ct_control <- get_joined_by_stimulus_df(unit_type, iff_data, ratings_data_control)
# add column with unit type
ct_control$UnitType <- rep(unit_type, nrow(ct_control)) 
# add column with group
ct_control$Group <- rep("Control", nrow(ct_control)) 
# run correlation
ct_control |>
  mutate(R = cor(mean.ratings,
                   mean.iff,
                   method = "spearman",
                   use = "na.or.complete")) -> ct_control

# asd data
ct_asd <- get_joined_by_stimulus_df(unit_type, iff_data, ratings_data_asd)
# add column with unit type
ct_asd$UnitType <- rep(unit_type, nrow(ct_asd)) 
# add column with group
ct_asd$Group <- rep("ASD", nrow(ct_asd)) 
# run correlation
ct_asd |>
  mutate(R = cor(mean.ratings,
                   mean.iff,
                   method = "spearman",
                   use = "na.or.complete")) -> ct_asd
#####################################################################
unit_type <- "FA-II"
# create dataframe with means for FA-II
# control
faII_control <- get_joined_by_stimulus_df(unit_type, iff_data, ratings_data_control)
# add column with unit type
faII_control$UnitType <- rep(unit_type, nrow(faII_control)) 
# add column with group
faII_control$Group <- rep("Control", nrow(faII_control)) 
# run correlation
faII_control |>
  mutate(R = cor(mean.ratings,
                   mean.iff,
                   method = "spearman",
                   use = "na.or.complete")) -> faII_control

# asd data
faII_asd <- get_joined_by_stimulus_df(unit_type, iff_data, ratings_data_asd)
# add column with unit type
faII_asd$UnitType <- rep(unit_type, nrow(faII_asd)) 
# add column with group
faII_asd$Group <- rep("ASD", nrow(faII_asd)) 
# run correlation
faII_asd |>
  mutate(R = cor(mean.ratings,
                   mean.iff,
                   method = "spearman",
                   use = "na.or.complete")) -> faII_asd
####################################################################
unit_type <- "Field"
# create dataframe with means for Field
# control
field_control <- get_joined_by_stimulus_df(unit_type, iff_data, ratings_data_control)
# add column with unit type
field_control$UnitType <- rep(unit_type, nrow(field_control)) 
# add column with group
field_control$Group <- rep("Control", nrow(field_control)) 
# run correlation
field_control |>
  mutate(R = cor(mean.ratings,
                   mean.iff,
                   method = "spearman",
                   use = "na.or.complete")) -> field_control

# asd data
field_asd <- get_joined_by_stimulus_df(unit_type, iff_data, ratings_data_asd)
# add column with unit type
field_asd$UnitType <- rep(unit_type, nrow(field_asd)) 
# add column with group
field_asd$Group <- rep("ASD", nrow(field_asd)) 
# run correlation
field_asd |>
  mutate(R = cor(mean.ratings,
                   mean.iff,
                   method = "spearman",
                   use = "na.or.complete")) -> field_asd
####################################################################
unit_type <- "HFA"
# create dataframe with means for HFA
# control
hfa_control <- get_joined_by_stimulus_df(unit_type, iff_data, ratings_data_control)
# add column with unit type
hfa_control$UnitType <- rep(unit_type, nrow(hfa_control)) 
# add column with group
hfa_control$Group <- rep("Control", nrow(hfa_control)) 
# run correlation
hfa_control |>
  mutate(R = cor(mean.ratings,
                   mean.iff,
                   method = "spearman",
                   use = "na.or.complete")) -> hfa_control
# asd data
hfa_asd <- get_joined_by_stimulus_df(unit_type, iff_data, ratings_data_asd)
# add column with unit type
hfa_asd$UnitType <- rep(unit_type, nrow(hfa_asd)) 
# add column with group
hfa_asd$Group <- rep("ASD", nrow(hfa_asd)) 
# run correlation
hfa_asd |>
  mutate(R = cor(mean.ratings,
                   mean.iff,
                   method = "spearman",
                   use = "na.or.complete")) -> hfa_asd
####################################################################
unit_type <- "SA-II"
# create dataframe with means for HFA
# control
saII_control <- get_joined_by_stimulus_df(unit_type, iff_data, ratings_data_control)
# add column with unit type
saII_control$UnitType <- rep(unit_type, nrow(saII_control)) 
# add column with group
saII_control$Group <- rep("Control", nrow(saII_control)) 
# run correlation
saII_control |>
  mutate(R = cor(mean.ratings,
                 mean.iff,
                 method = "spearman",
                 use = "na.or.complete")) -> saII_control
# asd data
saII_asd <- get_joined_by_stimulus_df(unit_type, iff_data, ratings_data_asd)
# add column with unit type
saII_asd$UnitType <- rep(unit_type, nrow(saII_asd)) 
# add column with group
saII_asd$Group <- rep("ASD", nrow(saII_asd)) 
# run correlation
saII_asd |>
  mutate(R = cor(mean.ratings,
                 mean.iff,
                 method = "spearman",
                 use = "na.or.complete")) -> saII_asd
#######################################################
#################################################################################
# COMBINE DATA FROM ALL UNITS AND GROUPS
# create final dataframe with all unit types and groups
final_df <- bind_rows(list(ms_control,ms_asd,
                           ct_control,ct_asd,
                           faII_control,faII_asd,
                           field_control,field_asd,
                           hfa_control,hfa_asd,
                           saII_control,saII_asd))

# round correlation coef to 2 decimal places
final_df$R <- round(final_df$R, digits = 2)

# separate control and asd to 2 dataframes
final_df |> 
  filter(Group == "Control") -> plot_df_control
final_df |> 
  filter(Group == "ASD") -> plot_df_asd
########################################
###################################################################################
# PLOT
# a function to open the nice figure device window, regardless of OS
# (good for reproducibility)
open_plot_window <- function(width = 7, height = 7, ...) {
  if (.Platform$OS.type == "unix") {
    quartz(width = width, height = height)
  }  else { windows(width = width, height = height) }
}
##################################################################################
# control
# create each plot lables with correlation coef
my_control_labels <- c(paste('CT\n\nR = ', round(ct_control$R[1], digits=2)),
                       paste('Field\n\nR = ', round(field_control$R[1], digits=2)),
                       paste('MS\n\nR = ', round(ms_control$R[1], digits=2)),
                       paste('HFA\n\nR = ', round(hfa_control$R[1], digits=2)),
                       paste('FA-II\n\nR = ', round(faII_control$R[1], digits=2)),
                       paste('SA-II\n\nR = ', round(saII_control$R[1], digits=2)))
# replace them in df
plot_df_control$UnitType <- factor(plot_df_control$UnitType, levels = c('CT', 'Field', 'MS', 'HFA','FA-II','SA-II'), 
                                   labels = my_control_labels)
# change first letters of stimulus to upper case
plot_df_control |> 
  mutate(Stimulus = str_to_title(Stimulus)) -> plot_df_control
###############################################################################################
# asd
# create each plot lables with correlation coef
my_asd_labels <- c(paste('CT\n\nR = ', round(ct_asd$R[1], digits=2)),
                   paste('Field\n\nR = ', round(field_asd$R[1], digits=2)),
                   paste('MS\n\nR = ', round(ms_asd$R[1], digits=2)),
                   paste('HFA\n\nR = ', round(hfa_asd$R[1], digits=2)),
                   paste('FA-II\n\nR = ', round(faII_asd$R[1], digits=2)),
                   paste('SA-II\n\nR = ', round(saII_asd$R[1], digits=2)))
# replace them in df
plot_df_asd$UnitType <- factor(plot_df_asd$UnitType, levels = c('CT', 'Field', 'MS', 'HFA','FA-II','SA-II'),
                               labels = my_asd_labels)

# change first letters of stimulus to upper case
plot_df_asd |> 
  mutate(Stimulus = str_to_title(Stimulus)) -> plot_df_asd

##############
# Shapes:
############
# CONTROL
control_only_p <- ggplot(plot_df_control, aes(x=mean.ratings, y=mean.iff)) + 
  geom_point(aes(shape=Stimulus,size=Stimulus),color = COLOUR_CONTROL_FELT)+
  # geom_point(size = 5) +
  # geom_point(shape = 17, size = 3) +
  scale_shape_manual(values=c(15, 0, 6, 17, 19, 8))+
  scale_size_manual(values = c(5, 8, 4, 5, 5, 3)) +
  geom_smooth(method = "lm", formula = y ~ poly(x,1),  se = F, color = "black", linewidth = 0.5, linetype = "dashed") +
  labs(title = "Control",
       x = "\nPleasantness",
       y = "Peak Firing Rate (Hz)\n",
       shape = "") +
  xlim(min_plot_rating, max_plot_rating) +
  facet_wrap(~ UnitType, scales = "free") +
  theme_classic() +
  # theme_tufte() + # no grid
  theme(
        legend.position = "bottom", # legend at the bottom
        legend.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 12, face = "bold", hjust = 0),
        strip.background = element_blank(),
        panel.spacing.y = unit(2, "lines"),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text = element_text(size=12)
    ) +
  guides(
    size = guide_legend(title ="", nrow = 1),
    shape = guide_legend(title ="", nrow = 1)
  ) # legend in a single row, showing shapes in the correct sizes
# show plot
open_plot_window(width = 1237, height = 867)
control_only_p
############
# ASD
asd_only_p <- ggplot(plot_df_asd, aes(x=mean.ratings, y=mean.iff)) + 
  geom_point(aes(shape=Stimulus,size=Stimulus),color = COLOUR_ASD_FELT)+
  # geom_point(color = COLOUR_ASD_FELT) +
  # geom_point(shape = 17, size = 3) +
  scale_shape_manual(values=c(15, 0, 6, 17, 19, 8))+
  scale_size_manual(values = c(5, 8, 4, 5, 5, 3)) +
  geom_smooth(method = "lm", formula = y ~ poly(x,1),  se = F, color = "black", linewidth = 0.5, linetype = "dashed") +
  labs(title = "ASD",
       x = "\nPleasantness",
       y = "Peak Firing Rate (Hz)\n",
       shape = "") +
  xlim(min_plot_rating, max_plot_rating) +
  facet_wrap(~ UnitType, scales = "free") +
  # facet_wrap(~ UnitType, scales = "free") +
  theme_classic() +
  # theme_tufte() + # no grid
  theme(
        legend.position = "bottom", # legend at the bottom
        legend.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 12, face = "bold", hjust = 0),
        strip.background = element_blank(),
        panel.spacing.y = unit(2, "lines"),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text = element_text(size=12)
        ) +
  # guides(shape = guide_legend(nrow = 1), size="none") # legend in a single row
  guides(
    size = guide_legend(title ="", nrow = 1),
    shape = guide_legend(title ="", nrow = 1)
  ) # legend in a single row, showing shapes in the correct sizes
# show plot
open_plot_window(width = 1237, height = 867)
asd_only_p
################################################################################
# TOGETHER

# change first letters of stimulus to upper case
final_df |> 
  mutate(Stimulus = str_to_title(Stimulus)) -> final_df

# create correlation coefficients annotations manually
ann_text_ct_asd <- data.frame(mean.ratings = -Inf, mean.iff = Inf,lab = sprintf("R = %s", round(ct_asd$R[1], digits=2)),
                              UnitType = factor('CT',levels = c('CT', 'Field', 'MS', 'HFA','FA-II','SA-II')))
ann_text_ct_control <- data.frame(mean.ratings = -Inf, mean.iff = Inf,lab = sprintf("R = %s ", round(ct_control$R[1], digits=2)),
                                  UnitType = factor('CT',levels = c('CT', 'Field', 'MS', 'HFA','FA-II','SA-II')))
ann_text_field_asd <- data.frame(mean.ratings = -Inf, mean.iff = -Inf,lab = sprintf("R = %s", round(field_asd$R[1], digits=2)),
                                 UnitType = factor('Field',levels = c('CT', 'Field', 'MS', 'HFA','FA-II','SA-II')))
ann_text_field_control <- data.frame(mean.ratings = -Inf, mean.iff = -Inf,lab = sprintf("R = %s ", round(field_control$R[1], digits=2)),
                                     UnitType = factor('Field',levels = c('CT', 'Field', 'MS', 'HFA','FA-II','SA-II')))
ann_text_ms_asd <- data.frame(mean.ratings = -Inf, mean.iff = -Inf,lab = sprintf("R = %s", round(ms_asd$R[1], digits=2)),
                              UnitType = factor('MS',levels = c('CT', 'Field', 'MS', 'HFA','FA-II','SA-II')))
ann_text_ms_control <- data.frame(mean.ratings = -Inf, mean.iff = -Inf,lab = sprintf("R = %s ", round(ms_control$R[1], digits=2)),
                                  UnitType = factor('MS',levels = c('CT', 'Field', 'MS', 'HFA','FA-II','SA-II')))
ann_text_hfa_asd <- data.frame(mean.ratings = -Inf, mean.iff = -Inf,lab = sprintf("R = %s", round(hfa_asd$R[1], digits=2)),
                               UnitType = factor('HFA',levels = c('CT', 'Field', 'MS', 'HFA','FA-II','SA-II')))
ann_text_hfa_control <- data.frame(mean.ratings = -Inf, mean.iff = -Inf,lab = sprintf("R = %s ", round(hfa_control$R[1], digits=2)),
                                   UnitType = factor('HFA',levels = c('CT', 'Field', 'MS', 'HFA','FA-II','SA-II')))
ann_text_faII_asd <- data.frame(mean.ratings = -Inf, mean.iff = -Inf,lab = sprintf("R = %s", round(faII_asd$R[1], digits=2)),
                                UnitType = factor('FA-II',levels = c('CT', 'Field', 'MS', 'HFA','FA-II','SA-II')))
ann_text_faII_control <- data.frame(mean.ratings = -Inf, mean.iff = -Inf,lab = sprintf("R = %s ", round(faII_control$R[1], digits=2)),
                                    UnitType = factor('FA-II',levels = c('CT', 'Field', 'MS', 'HFA','FA-II','SA-II')))
ann_text_saII_asd <- data.frame(mean.ratings = -Inf, mean.iff = -Inf,lab = sprintf("R = %s", round(saII_asd$R[1], digits=2)),
                                UnitType = factor('SA-II',levels = c('CT', 'Field', 'MS', 'HFA','FA-II','SA-II')))
ann_text_saII_control <- data.frame(mean.ratings = -Inf, mean.iff = -Inf,lab = sprintf("R = %s ", round(saII_control$R[1], digits=2)),
                                    UnitType = factor('SA-II',levels = c('CT', 'Field', 'MS', 'HFA','FA-II','SA-II')))

# plot with stimulus legend at the bottom
p1 <- ggplot(final_df, aes(x=mean.ratings, y=mean.iff)) + 
  geom_point(aes(shape=Stimulus,size=Stimulus,color=Group)) +
  geom_smooth(mapping = aes(group = Group, color=Group),method = "lm", formula = y ~ poly(x,1),  se = F, linewidth = 0.5, linetype = "dashed",show.legend = FALSE) +
  scale_color_manual(values=c(COLOUR_ASD_FELT,COLOUR_CONTROL_FELT)) +
  scale_shape_manual(values=c(15, 0, 6, 17, 19, 8))+
  scale_size_manual(values = c(5, 8, 4, 5, 5, 3)) +
  labs(title = "",
       x = "\nPleasantness",
       y = "Peak Firing Rate (Hz)\n") +
  xlim(min_plot_rating, max_plot_rating) +
  # change to the user defined unit order
  facet_wrap(~factor(UnitType, levels=c('CT', 'Field', 'MS', 'HFA','FA-II','SA-II')),scales = "free") +
  theme_classic() +
  theme(
    legend.position = "bottom", # legend at the bottom
    legend.box = "vertical",
    legend.spacing.y = unit(0.1, 'cm'),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 12, face = "bold", hjust = 0),
    strip.background = element_blank(),
    panel.spacing.y = unit(2, "lines"),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    axis.text = element_text(size=12)
  ) +
  guides(
    size = guide_legend(title ="", nrow = 1),
    shape = guide_legend(title ="", nrow = 1),
    color = "none"
    # color = guide_legend(title ="", nrow = 1,order=1)
  ) # legend in a single row, showing shapes in the correct sizes

# add annotations with correlation coefficients manually
p1 <- p1 + geom_text(data = ann_text_ct_asd,
                         label = ann_text_ct_asd$lab[1],
                         color = COLOUR_ASD_FELT,
                         hjust   = -0.2,
                         vjust   = 2) + 
          geom_text(data = ann_text_ct_control,
                    label = ann_text_ct_control$lab[1],
                    color = COLOUR_CONTROL_FELT,
                    hjust   = -0.2,
                    vjust   = 4) +
          geom_text(data = ann_text_field_asd,
                    label = ann_text_field_asd$lab[1],
                    color = COLOUR_ASD_FELT,
                    hjust   = -0.2,
                    vjust   = -4) + 
          geom_text(data = ann_text_field_control,
                    label = ann_text_field_control$lab[1],
                    color = COLOUR_CONTROL_FELT,
                    hjust   = -0.2,
                    vjust   = -2) +
          geom_text(data = ann_text_ms_asd,
                    label = ann_text_ms_asd$lab[1],
                    color = COLOUR_ASD_FELT,
                    hjust   = -0.2,
                    vjust   = -4) + 
          geom_text(data = ann_text_ms_control,
                    label = ann_text_ms_control$lab[1],
                    color = COLOUR_CONTROL_FELT,
                    hjust   = -0.2,
                    vjust   = -2) +
          geom_text(data = ann_text_hfa_asd,
                    label = ann_text_hfa_asd$lab[1],
                    color = COLOUR_ASD_FELT,
                    hjust   = -0.2,
                    vjust   = -4) + 
          geom_text(data = ann_text_hfa_control,
                    label = ann_text_hfa_control$lab[1],
                    color = COLOUR_CONTROL_FELT,
                    hjust   = -0.2,
                    vjust   = -2) +
          geom_text(data = ann_text_faII_asd,
                    label = ann_text_faII_asd$lab[1],
                    color = COLOUR_ASD_FELT,
                    hjust   = -0.2,
                    vjust   = -4) + 
          geom_text(data = ann_text_faII_control,
                    label = ann_text_faII_control$lab[1],
                    color = COLOUR_CONTROL_FELT,
                    hjust   = -0.2,
                    vjust   = -2) +
          geom_text(data = ann_text_saII_asd,
                    label = ann_text_saII_asd$lab[1],
                    color = COLOUR_ASD_FELT,
                    hjust   = -0.2,
                    vjust   = -4) + 
          geom_text(data = ann_text_saII_control,
                    label = ann_text_saII_control$lab[1],
                    color = COLOUR_CONTROL_FELT,
                    hjust   = -0.2,
                    vjust   = -2)


p2 <- ggplot(final_df, aes(x=mean.ratings, y=mean.iff)) + 
  geom_point(aes(shape=Stimulus,size=Stimulus,color=Group)) +
  geom_smooth(mapping = aes(group = Group, color = Group),method = "lm", formula = y ~ poly(x,1),  se = F, linewidth = 0.5, linetype = "dashed",show.legend = FALSE) +
  scale_color_manual(values=c(COLOUR_ASD_FELT,COLOUR_CONTROL_FELT)) +
  scale_shape_manual(values=c(15, 0, 6, 17, 19, 8))+
  scale_size_manual(values = c(5, 8, 4, 5, 5, 3)) +
  labs(title = "",
       x = "\nPleasantness",
       y = "Peak Firing Rate (Hz)\n") +
  xlim(min_plot_rating, max_plot_rating) +
  # change to the user defined unit order
  facet_wrap(~factor(UnitType, levels=c('CT', 'Field', 'MS', 'HFA','FA-II','SA-II')),scales = "free",labeller = label_parsed) +
  theme_classic() +
  theme(
    legend.position = "top", # legend at the bottom
    legend.box = "vertical",
    legend.spacing.y = unit(0.1, 'cm'),
    legend.text = element_text(size = 16, face = "bold"),
    strip.background = element_blank(),
    panel.spacing.y = unit(2, "lines"),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    axis.text = element_text(size=12)
  ) +
  guides(
    size = "none",
    shape = "none",
    color = guide_legend(title ="", nrow = 1,override.aes=list(size = 5))
  ) # legend in a single row, showing shapes in the correct sizes

library(cowplot)   # get_legend() & plot_grid() functions
library(patchwork) # blank plot: plot_spacer()

# get the group legend that was at the top
leg <- get_legend(p2)

# create a blank plot for legend alignment 
blank_p <- plot_spacer() + theme_void()


# plot with group legend on teh top and stimulus legend at the bottom
together_p <- plot_grid(leg,
                        p1,
                        nrow = 2,
                        rel_heights = c(.08, .92)
                        )
# show plot
open_plot_window(width = 1237, height = 867)
together_p
##########################################################################
#############################
# Colors:
# colour palette for Stimuli
# my_colour_palette <- c(COLOUR_ATTENTION, COLOUR_CALMING, COLOUR_GRATITUDE, COLOUR_HAPPINESS, COLOUR_LOVE, COLOUR_SADNESS)
# ggplot(plot_df_control, aes(mean.ratings, mean.iff, colour = Stimulus)) + 
#   geom_point(size = 5) +
#   # geom_point(shape = 17, size = 3) +
#   geom_smooth(method = "lm", formula = y ~ poly(x,1),  se = F, color = "black", linewidth = 0.5, linetype = "dashed") +
#   labs(title = "Control",
#        x = "\nPleasantness",
#        y = "Peak Firing Rate (Hz)\n",
#        colour = "") +
#   # scale_shape_manual(values=c(0, 1, 2, 5, 6, 9))+
#   scale_colour_manual(values = my_colour_palette) +
#   xlim(min_rating, max_rating) +
#   facet_wrap(~ UnitType, scales = "free_x") +
#   theme_classic() +
#   # theme_tufte() + # no grid
#   theme(legend.position = "bottom", # legend at the bottom
#         legend.text = element_text(size = 12, face = "bold"),
#         plot.title = element_text(size = 16, face = "bold"),
#         strip.text = element_text(size = 12, face = "bold", hjust = 0),
#         strip.background = element_blank(),
#         panel.spacing.y = unit(2, "lines"),
#         axis.title.x = element_text(size=12),
#         axis.title.y = element_text(size=12)) +
#   guides(colour = guide_legend(nrow = 1)) # legend in a single row

# ggplot(plot_df_asd, aes(mean.ratings, mean.iff, colour = Stimulus)) + 
#   geom_point(size = 5) +
#   # geom_point(shape = 17, size = 3) +
#   geom_smooth(method = "lm", formula = y ~ poly(x,1),  se = F, color = "black", linewidth = 0.5, linetype = "dashed") +
#   labs(title = "ASD",
#        x = "\nPleasantness",
#        y = "Peak Firing Rate (Hz)\n",
#        colour = "") +
#   scale_colour_manual(values = my_colour_palette) +
#   xlim(min_rating, max_rating) +
#   facet_wrap(~ UnitType, scales = "free_x") +
#   theme_classic() +
#   # theme_tufte() + # no grid
#   theme(legend.position = "bottom", # legend at the bottom
#         legend.text = element_text(size = 12, face = "bold"),
#         plot.title = element_text(size = 16, face = "bold"),
#         strip.text = element_text(size = 12, face = "bold", hjust = 0),
#         strip.background = element_blank(),
#         panel.spacing.y = unit(2, "lines"),
#         axis.title.x = element_text(size=12),
#         axis.title.y = element_text(size=12)) +
#   guides(colour = guide_legend(nrow = 1)) # legend in a single row
#############################################################################################


# Sarah's modification:

# # ASD
# ggplot(
#   data = plot_df_asd, 
#   mapping = aes(x=mean.ratings, y=mean.iff)) + 
#   geom_point(
#     mapping = aes(shape=Stimulus), 
#     color = COLOUR_ASD_FELT,
#     size = 5) +
#   # geom_label_repel(aes(label = Stimulus),
#   #                  label.size = NA, # no box frame
#   #                  max.overlaps=Inf, # no label dissapearing due to overlap
#   #                  box.padding   = 0.5, 
#   #                  point.padding = 0.55,
#   #                  segment.color = 'grey50',
#   #                  segment.size = 0.2,
#   #                  nudge_y = 500
#   # ) +
#   scale_shape_manual(values=c(15, 0, 23, 17, 19, 8))+
#   # scale_shape_manual(values=c(3, 0, 23, 24, 4, 8))+
#   # scale_shape_manual(values=c(65, 67, 71, 72, 76, 83)) +
#   geom_smooth(method = "lm", formula = y ~ poly(x,1),  se = F, color = "black", linewidth = 0.5, linetype = "dashed") +
#   labs(title = "ASD",
#        x = "\nPleasantness",
#        y = "Peak Firing Rate (Hz)\n",
#        shape = "") +
#   # xlim(min_rating, max_rating) +
#   # facet_wrap(~ UnitType, scales = "free_x") +
#   facet_wrap(~ UnitType, scales = "free") +
#   theme_classic() +
#   # theme_tufte() + # no grid
#   theme(legend.position = "bottom", # legend at the bottom
#         legend.text = element_text(size = 12, face = "bold"),
#         plot.title = element_text(size = 16, face = "bold"),
#         strip.text = element_text(size = 12, face = "bold", hjust = 0),
#         strip.background = element_blank(),
#         panel.spacing.y = unit(2, "lines"),
#         axis.title.x = element_text(size=12),
#         axis.title.y = element_text(size=12)) +
#   guides(shape = guide_legend(nrow = 1), size="none") # legend in a single row



