library(readxl)
library(openxlsx)
library(dplyr)

# total number of runs for bootstraping
B = 9999


# source all .R files in the Rfunctions directory ####
source_files <- list.files("Rfunctions", full.names = TRUE)
sapply(source_files[grepl(
  "(get_unit_correlation)|(get_max_firing_rates)", 
  source_files
)], source)
###################################################################
# READ DATA 
# read excel file with pleasentness ratings
ratings_data_file_path <- "Data/primary/live_pleas-data.csv"
ratings_data_all <- read.csv(ratings_data_file_path)
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

# control data
correlations_ms_control <- get_coefs_per_unit_type(unit_type, iff_data, ratings_data_control,B)
# create dataframe that will hold all coef results
corr_df <- data.frame(MS.control = correlations_ms_control)

# asd data
correlations_ms_asd <- get_coefs_per_unit_type(unit_type, iff_data, ratings_data_asd,B)
# create dataframe that will hold all coef results
corr_df$MS.asd <- correlations_ms_asd

#########
# PLOT
# Set up a side-by-side layout
par(mfrow = c(1, 2))
# Create the first histogram
hist(corr_df$MS.control, main = "MS control", xlab = "coef", ylab = "count")
# Create the second histogram
hist(corr_df$MS.asd, main = "MS asd", xlab = "coef", ylab = "count")

#####################################################################
unit_type <- "CT"

# control data
correlations_ct_control <- get_coefs_per_unit_type(unit_type, iff_data, ratings_data_control,B)
# create dataframe that will hold all coef results
corr_df$CT.control <- correlations_ct_control

# asd data
correlations_ct_asd <- get_coefs_per_unit_type(unit_type, iff_data, ratings_data_asd,B)
# create dataframe that will hold all coef results
corr_df$CT.asd <- correlations_ct_asd

#########
# PLOT
# Set up a side-by-side layout
par(mfrow = c(1, 2))
# Create the first histogram
hist(corr_df$CT.control, main = "CT control", xlab = "coef", ylab = "count")
# Create the second histogram
hist(corr_df$CT.asd, main = "CT asd", xlab = "coef", ylab = "count")

#####################################################################
unit_type <- "FA-II"
# control data
correlations_faII_control <- get_coefs_per_unit_type(unit_type, iff_data, ratings_data_control,B)
# create dataframe that will hold all coef results
corr_df$FAII.control <- correlations_faII_control

# asd data
correlations_faII_asd <- get_coefs_per_unit_type(unit_type, iff_data, ratings_data_asd,B)
# create dataframe that will hold all coef results
corr_df$FAII.asd <- correlations_faII_asd

#########
# PLOT
# Set up a side-by-side layout
par(mfrow = c(1, 2))
# Create the first histogram
hist(corr_df$FAII.control, main = "FA-II control", xlab = "coef", ylab = "count")
# Create the second histogram
hist(corr_df$FAII.asd, main = "FA-II asd", xlab = "coef", ylab = "count")

####################################################################
unit_type <- "Field"

# control data
correlations_field_control <- get_coefs_per_unit_type(unit_type, iff_data, ratings_data_control,B)
# create dataframe that will hold all coef results
corr_df$Field.control <- correlations_field_control

# asd data
correlations_field_asd <- get_coefs_per_unit_type(unit_type, iff_data, ratings_data_asd,B)
# create dataframe that will hold all coef results
corr_df$Field.asd <- correlations_field_asd

#########
# PLOT
# Set up a side-by-side layout
par(mfrow = c(1, 2))
# Create the first histogram
hist(corr_df$Field.control, main = "Field control", xlab = "coef", ylab = "count")
# Create the second histogram
hist(corr_df$Field.asd, main = "Field asd", xlab = "coef", ylab = "count")

####################################################################
unit_type <- "HFA"

# control data
correlations_hfa_control <- get_coefs_per_unit_type(unit_type, iff_data, ratings_data_control,B)
# create dataframe that will hold all coef results
corr_df$HFA.control <- correlations_hfa_control

# asd data
correlations_hfa_asd <- get_coefs_per_unit_type(unit_type, iff_data, ratings_data_asd,B)
# create dataframe that will hold all coef results
corr_df$HFA.asd <- correlations_hfa_asd

#########
# PLOT
# Set up a side-by-side layout
par(mfrow = c(1, 2))
# Create the first histogram
hist(corr_df$HFA.control, main = "HFA control", xlab = "coef", ylab = "count")
# Create the second histogram
hist(corr_df$HFA.asd, main = "HFA asd", xlab = "coef", ylab = "count")


# Save data frame to an Excel file
write.xlsx(corr_df, "Data/processed/corr_dataframe.xlsx")
