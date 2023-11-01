library(tidyverse)
#' Create a Boxplot
#'
#' This function creates a boxplot for DRG data by taking input of type of payment
#'
#' @param payment Type of payment - "medicare", "total", "covered"
#' @return Boxplot
#' @export
payment <- function(payment_type)
{
  data <- read_csv("data/DRG_data.csv")

  medicare <- data %>%
    group_by(DRG.Definition) %>%
    summarise(mean = mean(Average.Medicare.Payments))
  total <- data %>%
    group_by(DRG.Definition) %>%
    summarise(mean = mean(Average.Total.Payments))
  covered <- data %>%
    group_by(DRG.Definition) %>%
    summarise(mean = mean(Average.Covered.Charges))

  if (payment_type == "medicare"){
    plt <- medicare %>%
      ggplot(aes(x=DRG.Definition,
                 y=mean)) +
      geom_boxplot() +
      labs(x = "DRG Codes",
           y = "Average Medicare Payment (dollars)",
           title = "Average Medicare Payment by DRG Code") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_minimal()
  }
  else if (payment_type == "total"){
    plt <- total %>%
      ggplot(aes(x=DRG.Definition,
                 y=mean)) +
      geom_boxplot() +
      labs(x = "DRG Codes",
           y = "Average Total Payment (dollars)",
           title = "Average Total Payment by DRG Code") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_minimal()
  }
  else{
    plt <- covered %>%
      ggplot(aes(x=DRG.Definition,
                 y=mean)) +
      geom_boxplot() +
      labs(x = "DRG Codes",
           y = "Average Covered Payment (dollars)",
           title = "Average Covered Payment by DRG Code") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_minimal()
  }
}
