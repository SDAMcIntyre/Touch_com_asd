library(dplyr)

# function that will return the number of rows of the shorter dataframe
n_sample_rows <- function(first_df, second_df) {
  min(nrow(first_df), nrow(second_df))
}


# function that will run total_iterations of times for a given unit and return a vector of correlation coefficients
get_coefs_per_unit_type <- function(unit, iff_df, ratings_df, total_iterations) {
  # create a vector that will hold all coefficient values
  correlations <- vector("numeric", total_iterations)
  # repeat total_iterations times
  for(i in 1:total_iterations)
  {
    # select only one unit type data
    iff_df |> 
      filter(UnitType == unit) -> iff_unit_data
    
    total_sample_rows <- n_sample_rows(ratings_df,iff_unit_data)
    # select with replacement randomly n rows
    ratings_sample <- ratings_df[sample(nrow(ratings_df),size=total_sample_rows,replace=TRUE),]
    iff_unit_sample <- iff_unit_data[sample(nrow(iff_unit_data),size=total_sample_rows,replace=TRUE),]
    
    # get mean ratings by stimulus (column called cued)
    ratings_sample |> 
      group_by(Stimulus) |> 
      summarise(mean.ratings = mean(response)) -> mean_ratings
    
    iff_unit_sample |> 
      group_by(Stimulus) |> 
      summarise(mean.iff = mean(MaxUnitIfreq)) -> mean_iff
    
    # join ratings means with iff means
    mean_ratings |> full_join(
      mean_iff,
      by = "Stimulus") -> joined_by_stimulus
    
    # run correlation
    joined_by_stimulus |> 
      with(cor(mean.ratings,
               mean.iff,
               method = "spearman",
               use = "na.or.complete")) -> correlations[i]
    
  } # end iterations
  return(correlations)
} # end get_coefs_per_unit_type