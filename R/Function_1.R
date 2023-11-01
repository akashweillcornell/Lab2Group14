#' Create a Boxplot
#'
#' This function creates a boxplot for DRG data by taking input of type of payment
#'
#' @param payment Type of payment
#' @return Boxplot
#' @export
payment <- function(payment_type)
{
  # Group by each of the three categories
  medicare <- data %>%
    group_by(DRG.Definition) %>%
    summarise(mean = mean(Average.Medicare.Payments))
  total_payment <- data %>%
    group_by(DRG.Definition) %>%
    summarise(mean = mean(Average.Total.Payments))
  covered_charges <- data %>%
    group_by(DRG.Definition) %>%
    summarise(mean = mean(Average.Covered.Charges))
  
  
  # Check which category the user wants to plot
  if (payment_type == "medicare"){
    
    # Plot it
    p <- ggplot(medicare,
                aes(x=DRG.Definition, y=mean)) +
      geom_boxplot() +
      xlab("DRG Codes") +
      ylab("Average Medicare Payment (in dollars)") +
      ggtitle("Average Medicare Payment by DRG Code") +
      theme(plot.title = element_text(hjust = 0.5))
    print(p)
  }
  else if (payment_type == "total_payment"){
    p <- ggplot(total_payment,
                aes(x=DRG.Definition, y=mean)) +
      geom_boxplot() +
      xlab("DRG Codes") +
      ylab("Average Total Payment (in dollars)") +
      ggtitle("Average Total Payment by DRG Code") +
      theme(plot.title = element_text(hjust = 0.5))
    print(p)
  }
  else{
    p <- ggplot(covered_charges,
                aes(x=DRG.Definition, y=mean)) +
      geom_boxplot() +
      xlab("DRG Codes") +
      ylab("Average Covered Charges (in dollars)") +
      ggtitle("Average Covered Charges by DRG Code") +
      theme(plot.title = element_text(hjust = 0.5))
    print(p)
  }
  
  
}