library(readxl)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggthemes)

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
################################################################
# create final dataframe with all unit types and groups
final_df <- bind_rows(list(ms_control,ms_asd,ct_control,ct_asd,faII_control,faII_asd,field_control,field_asd,hfa_control,hfa_asd))
# round correlation coef to 2 decimal places
final_df$R <- round(final_df$R, digits = 2)

# separate control and asd to 2 dataframes
final_df |> 
  filter(Group == "Control") -> plot_df_control
final_df |> 
  filter(Group == "ASD") -> plot_df_asd
###################################################################################
# PLOT
# colour palette for Stimuli
my_colour_palette <- c(COLOUR_ATTENTION, COLOUR_CALMING, COLOUR_GRATITUDE, COLOUR_HAPPINESS, COLOUR_LOVE, COLOUR_SADNESS)
##################################################################################
# control
# create each plot lables with correlation coef
my_control_labels <- c(paste('CT\n\nR = ', round(ct_control$R[1], digits=2)),
                       paste('FA-II\n\nR = ', round(faII_control$R[1], digits=2)),
                       paste('Field\n\nR = ', round(field_control$R[1], digits=2)),
                       paste('HFA\n\nR = ', round(hfa_control$R[1], digits=2)),
                       paste('MS\n\nR = ', round(ms_control$R[1], digits=2)))
# replace them in df
plot_df_control$UnitType <- factor(plot_df_control$UnitType, levels = c('CT', 'FA-II', 'Field', 'HFA','MS'), 
                   labels = my_control_labels)
# change first letters of stimulus to upper case
plot_df_control |> 
  mutate(Stimulus = str_to_title(Stimulus)) -> plot_df_control

ggplot(plot_df_control, aes(mean.ratings, mean.iff, colour = Stimulus)) + 
  geom_point(size = 5) +
  geom_smooth(method = "lm", formula = y ~ poly(x,1),  se = F, color = "black", linewidth = 0.5, linetype = "dashed") +
  labs(title = "Control",
       x = "\nPleasantness",
       y = "Peak Firing Rate (Hz)\n",
       colour = "") +
  scale_colour_manual(values = my_colour_palette) +
  xlim(min_rating, max_rating) +
  facet_wrap(~ UnitType, scales = "free") +
  theme_classic() +
  # theme_tufte() + # no grid
  theme(legend.position = "bottom", # legend at the bottom
        legend.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 12, face = "bold", hjust = 0),
        strip.background = element_blank(),
        panel.spacing.y = unit(2, "lines"),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12)) +
  guides(colour = guide_legend(nrow = 1)) # legend in a single row
###############################################################################################
# asd
# create each plot lables with correlation coef
my_asd_labels <- c(paste('CT\n\nR = ', round(ct_asd$R[1], digits=2)),
                       paste('FA-II\n\nR = ', round(faII_asd$R[1], digits=2)),
                       paste('Field\n\nR = ', round(field_asd$R[1], digits=2)),
                       paste('HFA\n\nR = ', round(hfa_asd$R[1], digits=2)),
                       paste('MS\n\nR = ', round(ms_asd$R[1], digits=2)))
# replace them in df
plot_df_asd$UnitType <- factor(plot_df_asd$UnitType, levels = c('CT', 'FA-II', 'Field', 'HFA','MS'),
                                   labels = my_asd_labels)

# change first letters of stimulus to upper case
plot_df_asd |> 
  mutate(Stimulus = str_to_title(Stimulus)) -> plot_df_asd

ggplot(plot_df_asd, aes(mean.ratings, mean.iff, colour = Stimulus)) + 
  geom_point(size = 5) +
  geom_smooth(method = "lm", formula = y ~ poly(x,1),  se = F, color = "black", linewidth = 0.5, linetype = "dashed") +
  labs(title = "ASD",
       x = "\nPleasantness",
       y = "Peak Firing Rate (Hz)\n",
       colour = "") +
  scale_colour_manual(values = my_colour_palette) +
  xlim(min_rating, max_rating) +
  facet_wrap(~ UnitType, scales = "free") +
  theme_classic() +
  # theme_tufte() + # no grid
  theme(legend.position = "bottom", # legend at the bottom
        legend.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 12, face = "bold", hjust = 0),
        strip.background = element_blank(),
        panel.spacing.y = unit(2, "lines"),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12)) +
  guides(colour = guide_legend(nrow = 1)) # legend in a single row
