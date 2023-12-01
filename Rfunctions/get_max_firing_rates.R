library(readxl)
library(dplyr)

get_max_firing_rates <- function(path2file) {
  # read excel file with inter spike intervals
  # isi_data_file_path <- "Data/ALL_ISIs_after_exclusions.xlsx"
  isi_data <- read_excel(path2file)
  
  ###############################################
  # Check the data content
  # check unit types
  unique(isi_data$UnitType)
  length(unique(isi_data$UnitType))
  # check unit names
  unique(isi_data$UnitName)
  length(unique(isi_data$UnitName))
  
  #############################################
  # find  and save  grouped max firing rates
  isi_data |>
    mutate(ifreq = 1/isi.sec) |>            # calculate instantenous frequency data
    group_by(StimNumber,UnitType,UnitName,Stimulus) |> # group by important values
    summarise(MaxUnitIfreq = max(ifreq)) -> # find max instantenous frequency for each StimNumber
    my_max_unit_ifreqs                      # save results to my_max_unit_ifreqs
  return(my_max_unit_ifreqs)
}



