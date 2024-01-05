library(readxl)
library(dplyr)
library(tidyr)
library(gtsummary)
library(flextable)
library(ggplot2)
library(ggthemes)

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
percentile_9.75 <- function(x) { quantile(x, c(0.025, 0.975))[2] }

corr_df |>
  tbl_summary(statistic = list(all_continuous() ~ "{median} ({percentile_2.5}, {percentile_9.75})")) |>
  as_flex_table() |> # Convert the table to a format that can be exported
  save_as_docx(path = "Data/processed/coef_median_summary_table.docx")
############################################################################
# MS
quantile(corr_df$MS.control, c(0.025, 0.975))
median(corr_df$MS.control)
summary(corr_df$MS.control)
quantile(corr_df$MS.asd, c(0.025, 0.975))
median(corr_df$MS.asd)
summary(corr_df$MS.asd)

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

# PLOT
long_df |> 
  filter(Group == "SAII.control" | Group == "SAII.asd") |> 
  ggplot(aes(Group,coef)) +
  geom_boxplot() +
  labs(title = "SA-II",
       x="",
       y="coef") +
  theme_tufte() # no grid



# In the table, display medians and bootstrap confidence intervals instead:
percentile_2.5 <- function(x) { quantile(x, c(0.025, 0.975))[1] }
percentile_9.75 <- function(x) { quantile(x, c(0.025, 0.975))[2] }

corr_df |>
  tbl_summary(statistic = list(all_continuous() ~ "{median} ({percentile_2.5}, {percentile_9.75})"))

