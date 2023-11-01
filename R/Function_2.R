#' Function calculates either the mean, median, or standard deviation of the DRG
#' codes. over all of the DRG codes for average Medicare payments
#'
#' @param statistics is a string which specifies the statistic and can take
#' values - mean, medium and standard deviation
#'
#' @return returns the chosen statistic calculated
#' @export
stats <- function(statistics){
  # Assign different number to indicate the location of the statistics
  if(statistics == "mean"){
    i <- 2
  }else if(statistics == "medium"){
    i <- 3
  }else{
    i <- 4
  }
  da1 <- data %>%
    # Calculate the mean, medium, and standard deviation of each group
    group_by(DRG.Definition) %>%
    summarise(Mean = mean(Average.Medicare.Payments),
              Medium = median(Average.Medicare.Payments),
              Standard_Deviation  = sd(Average.Medicare.Payments))
  #  Replace (.) with spaces
  colnames(da1) <- gsub("\\.", " ", colnames(da1))
  #  Replace (_) with spaces
  colnames(da1) <- gsub("\\_", " ", colnames(da1))
  # return the statistics that we choose
  output2 <- knitr::kable(da1[,c(1,i)])
  return(output2)
}
