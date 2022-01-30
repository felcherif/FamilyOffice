#' @title risk_premium_adjustment
#' @param amount value in currency
#' @param asset_class property or
#' @param output all in memory
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @return cash_flow (increased by risk premium)
#' @export

risk_premium_adjustment <- function(amount, asset_class, output) {
  frequency <- output$reporting$frequency_number

  if (asset_class == "property_class") {
    risk_premium <- output$economic_data$risk_premium$property
  }

  if (asset_class == "share_class") {
    risk_premium <- output$economic_data$risk_premium$share
  }

  rp <- (1 + risk_premium) ^ (1 / frequency)
  cash_flow <- amount * rp

  return(cash_flow)

}
