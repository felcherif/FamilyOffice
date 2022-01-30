#' @title inflation_adjustment
#' @description function that adjust projection monetary amount for inflation in the financial statements
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @param amount is the amount to adjust for inflation
#' @param currency is used to select the inflation to use
#' @param output contains all data.
#' @return this function returns the amount modified for inflation
#' @export

inflation_adjustment <- function(amount, currency, output) {
  frequency <- output$reporting$frequency_number
  inflation_rate <- output$economic_data$inflation[[currency]]
  inflation <- (1 + inflation_rate)^(1 / frequency)
  inflated_cash_flow <- amount * inflation
  return(inflated_cash_flow)
}
