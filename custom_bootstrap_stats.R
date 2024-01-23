library(readxl)
library(dplyr)
library(tidyr)
library(gtsummary)
library(flextable)
library(ggplot2)
library(ggthemes)



# source all .R files in the Rfunctions directory ####
source_files <- list.files("Rfunctions", full.names = TRUE)
sapply(source_files[grepl(
  "(get_unit_correlation)|(get_max_firing_rates)|(plot_appearance)", 
  source_files
)], source)

# read correlation dataframe
corr_file_path <- "Data/processed/corr_dataframe13_12_2023.xlsx"
corr_df <- read_excel(corr_file_path)
# create dataframe for box plot (requires group names in one column and matching coef values in the other)
long_df <- corr_df |> gather(Group, coef)

################################################################################
# STATS
################################################################################
# SUMMARY
# In the table, display medians and bootstrap confidence intervals:
percentile_2.5 <- function(x) { quantile(x, c(0.025, 0.975))[1] }
percentile_97.5 <- function(x) { quantile(x, c(0.025, 0.975))[2] }

corr_df |>
  tbl_summary(statistic = list(all_continuous() ~ "{median} ({percentile_2.5}, {percentile_97.5})")) |>
  as_flex_table() |> # Convert the table to a format that can be exported
  save_as_docx(path = "Data/processed/coef_median_summary_table.docx")
############################################################################
# MS
# start creating rows for data frame for summary plot
ms_control_quantile <- quantile(corr_df$MS.control, c(0.025, 0.975))
ms_control_median <- median(corr_df$MS.control)
summary(corr_df$MS.control)
quantile(corr_df$MS.asd, c(0.025, 0.975))
median(corr_df$MS.asd)
summary(corr_df$MS.asd)

# create rows for summary data frame
ms_control_row <- c("MS","Control",quantile(corr_df$MS.control, c(0.025, 0.975)),median(corr_df$MS.control))
ms_asd_row <- c("MS","ASD",quantile(corr_df$MS.asd, c(0.025, 0.975)),median(corr_df$MS.asd))

# PLOT
long_df |> 
  filter(Group == "MS.control" | Group == "MS.asd") |> 
  ggplot(aes(Group,coef)) +
  geom_boxplot() +
  labs(title = "MS",
       x="",
       y="coef") +
  theme_tufte() # no grid
############################
# CT 
quantile(corr_df$CT.control, c(0.025, 0.975))
median(corr_df$CT.control)
summary(corr_df$CT.control)
quantile(corr_df$CT.asd, c(0.025, 0.975))
median(corr_df$CT.asd)
summary(corr_df$CT.asd)

# create rows for summary data frame
ct_control_row <- c("CT","Control",quantile(corr_df$CT.control, c(0.025, 0.975)),median(corr_df$CT.control))
ct_asd_row <- c("CT","ASD",quantile(corr_df$CT.asd, c(0.025, 0.975)),median(corr_df$CT.asd))

# PLOT
long_df |> 
  filter(Group == "CT.control" | Group == "CT.asd") |> 
  ggplot(aes(Group,coef)) +
    geom_boxplot() +
    labs(title = "CT",
         x="",
         y="coef") +
  theme_tufte() # no grid
############################
# FA-II
quantile(corr_df$FAII.control, c(0.025, 0.975))
median(corr_df$FAII.control)
summary(corr_df$FAII.control)
quantile(corr_df$FAII.asd, c(0.025, 0.975))
median(corr_df$FAII.asd)
summary(corr_df$FAII.asd)

# create rows for summary data frame
faII_control_row <- c("FA-II","Control",quantile(corr_df$FAII.control, c(0.025, 0.975)),median(corr_df$FAII.control))
faII_asd_row <- c("FA-II","ASD",quantile(corr_df$FAII.asd, c(0.025, 0.975)),median(corr_df$FAII.asd))

# PLOT
long_df |> 
  filter(Group == "FAII.control" | Group == "FAII.asd") |> 
  ggplot(aes(Group,coef)) +
  geom_boxplot() +
  labs(title = "FA-II",
       x="",
       y="coef") +
  theme_tufte() # no grid
############################
# Field
quantile(corr_df$Field.control, c(0.025, 0.975))
median(corr_df$Field.control)
summary(corr_df$Field.control)
quantile(corr_df$Field.asd, c(0.025, 0.975))
median(corr_df$Field.asd)
summary(corr_df$Field.asd)

# create rows for summary data frame
field_control_row <- c("Field","Control",quantile(corr_df$Field.control, c(0.025, 0.975)),median(corr_df$Field.control))
field_asd_row <- c("Field","ASD",quantile(corr_df$Field.asd, c(0.025, 0.975)),median(corr_df$Field.asd))

# PLOT
long_df |> 
  filter(Group == "Field.control" | Group == "Field.asd") |> 
  ggplot(aes(Group,coef)) +
  geom_boxplot() +
  labs(title = "Field",
       x="",
       y="coef") +
  theme_tufte() # no grid
############################
# HFA
quantile(corr_df$HFA.control, c(0.025, 0.975))
median(corr_df$HFA.control)
summary(corr_df$HFA.control)
quantile(corr_df$HFA.asd, c(0.025, 0.975))
median(corr_df$HFA.asd)
summary(corr_df$HFA.asd)

# create rows for summary data frame
HFA_control_row <- c("HFA","Control",quantile(corr_df$HFA.control, c(0.025, 0.975)),median(corr_df$HFA.control))
HFA_asd_row <- c("HFA","ASD",quantile(corr_df$HFA.asd, c(0.025, 0.975)),median(corr_df$HFA.asd))

# PLOT
long_df |> 
  filter(Group == "HFA.control" | Group == "HFA.asd") |> 
  ggplot(aes(Group,coef)) +
  geom_boxplot() +
  labs(title = "HFA",
       x="",
       y="coef") +
  theme_tufte() # no grid
############################
# SA-II
quantile(corr_df$SAII.control, c(0.025, 0.975))
median(corr_df$SAII.control)
summary(corr_df$SAII.control)
quantile(corr_df$SAII.asd, c(0.025, 0.975))
median(corr_df$SAII.asd)
summary(corr_df$SAII.asd)

# create rows for summary data frame
saII_control_row <- c("SA-II","Control",quantile(corr_df$SAII.control, c(0.025, 0.975)),median(corr_df$SAII.control))
saII_asd_row <- c("SA-II","ASD",quantile(corr_df$SAII.asd, c(0.025, 0.975)),median(corr_df$SAII.asd))

# PLOT
long_df |> 
  filter(Group == "SAII.control" | Group == "SAII.asd") |> 
  ggplot(aes(Group,coef)) +
  geom_boxplot() +
  labs(title = "SA-II",
       x="",
       y="coef") +
  theme_tufte() # no grid

###################################################
#
summary_df <- data.frame(ms_control_row,
                         ms_asd_row,
                         ct_control_row,
                         ct_asd_row,
                         faII_control_row,
                         faII_asd_row,
                         field_control_row,
                         field_asd_row,
                         HFA_control_row,
                         HFA_asd_row,
                         saII_control_row,
                         saII_asd_row)
# transpose
summary_df <- t(summary_df) # now it becomes a vector of characters
# convert back as data frame
summary_df <- as.data.frame(summary_df)
# add column names
colnames(summary_df) <- c("UnitType", "Group", "LowCI", "HighCI","Median")

# convert characters to numeric
summary_df$LowCI <- as.numeric(summary_df$LowCI)
summary_df$HighCI <- as.numeric(summary_df$HighCI)
summary_df$Median <- as.numeric(summary_df$Median)

###################################################################################
# PLOT
# a function to open the nice figure device window, regardless of OS
# (good for reproducibility)
open_plot_window <- function(width = 7, height = 7, ...) {
  if (.Platform$OS.type == "unix") {
    quartz(width = width, height = height)
  }  else { windows(width = width, height = height) }
}


corr_p <- ggplot(data = summary_df) +
  geom_point(aes(x=UnitType,y=Median,color=Group), size = 5, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(x=UnitType, y=Median, color=Group, ymin=LowCI, ymax=HighCI), 
                show.legend = FALSE,
                position = position_dodge(width = 0.4)) +
  scale_color_manual(values=c(COLOUR_ASD_FELT,COLOUR_CONTROL_FELT)) +
  labs(title = "Correlation of pleasantness ratings and afferent firing rate",
     x = "",
     y = "\nSpearman correlation coefficient (r)\n") +
  theme_classic() +
  theme(
    legend.position = "top", # legend at the bottom
    legend.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=14, face = "bold"),
    axis.text.x = element_text(size=14, face = "bold"),
    axis.text.y = element_text(size=12)
  ) +
  guides(
    color = guide_legend(title ="", nrow = 1)
  ) # legend in a single row, showing shapes in the correct sizes

# show plot
open_plot_window(width = 1406, height = 717)
corr_p

