library(tidyverse)
#' Function calculates either the mean, median, or standard deviation of the DRG
#' codes. over all of the DRG codes for average Medicare payments
#'
#' @param statistics is a string which specifies the statistic and can take
#' values - mean, medium and standard deviation
#'
#' @return returns the chosen statistic calculated
#' @export
stats <- function(stat_type = "mean") {

  data <- read.csv("data/DRG_data.csv")

  # Check if the input statistic is valid
  if (!(stat_type %in% c("mean", "median", "sd"))) {
    stop("Invalid statistic. Please choose 'mean', 'median', or 'sd'.")
  }

  # For each stats, calculate seperately
  calculate <- list(
    mean = function(x) mean(x, na.rm = TRUE),
    median = function(x) median(x, na.rm = TRUE),
    sd = function(x) sd(x, na.rm = TRUE)
  )

  # Get statistics based on the  input stat_type
  result <- data %>%
    summarise(
      stat_value = calculate[[stat_type]](Average.Medicare.Payments)
    ) %>%
    pull(stat_value)

  return(result)
}
