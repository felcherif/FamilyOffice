#' @title fx
#' @description function that rebase currency
#' @param output is the only parameter 'input' needed is the input files name.
#' @return this function returns a text file 'output.txt'
#' @export

fx <- function(output) {

  reporting_currency <- output$reporting$reporting_currency
  exchange_rate_base_CAD <- output$economic_data$exchange_rate_base_CAD

  if (reporting_currency == "CAD") {
    output$economic_data$exchange_rate <- exchange_rate_base_CAD
  }

  if (reporting_currency == "EUR") {
    output$economic_data$exchange_rate$GBP <- exchange_rate_base_CAD$GBP / exchange_rate_base_CAD$EUR
    output$economic_data$exchange_rate$CAD <- 1 / exchange_rate_base_CAD$EUR
    output$economic_data$exchange_rate$USD <- exchange_rate_base_CAD$EUR / exchange_rate_base_CAD$USD
    output$economic_data$exchange_rate$EUR <- 1
  }

  if (reporting_currency == "USD") {
    output$economic_data$exchange_rate$GBP <- exchange_rate_base_CAD$GBP / exchange_rate_base_CAD$USD
    output$economic_data$exchange_rate$CAD <- 1 / exchange_rate_base_CAD$USD
    output$economic_data$exchange_rate$USD <- 1
    output$economic_data$exchange_rate$EUR <- exchange_rate_base_CAD$EUR / exchange_rate_base_CAD$USD
  }

  if (reporting_currency == "GBP") {
    output$economic_data$exchange_rate$GBP <- 1
    output$economic_data$exchange_rate$CAD <- 1 / exchange_rate_base_CAD$GBP
    output$economic_data$exchange_rate$USD <- exchange_rate_base_CAD$USD / exchange_rate_base_CAD$GBP
    output$economic_data$exchange_rate$EUR <- exchange_rate_base_CAD$EUR / exchange_rate_base_CAD$GBP
  }

  return(output)
}

