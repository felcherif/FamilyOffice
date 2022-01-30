#' @title tax_calculations
#' @description function that calculate income tax for the statement
#' @param income is the income
#' @param x is an item in the portfolio
#' @param output contains all data
#' @return output with a modified income tax
#' @export

tax_calculations <- function(x, output, income) {

  "%+=%" <- roperators::`%+=%`
  "%-=%" <- roperators::`%-=%`
  "%*=%" <- roperators::`%*=%`

  if (is.element(el = "currency", set = methods::slotNames(x))) {
    currency <- x@currency
  } else {
    currency <- output$reporting$reporting_currency
  }

  if (is.element(el = "taxpayer", set = methods::slotNames(x))) {
    taxpayer <- x@taxpayer
  } else {
    taxpayer <- output$reporting$taxpayer
  }

  portfolio_name <- output$reporting$portfolio_name
  date <- output$reporting$reporting_date
  reporting_dates <- output$reporting$reporting_dates
  threshold <- as.numeric(output$economic_data$tax$income[[currency]][, "threshold"])
  tax_rate <- as.numeric(output$economic_data$tax$income[[currency]][, "tax_rate"])
  FX <- output$economic_data$exchange_rate[[currency]]
  current_year <- lubridate::year(date) == lubridate::year(reporting_dates)

  if (length(taxpayer) == 1) {
    output$metrics$taxable_income[currency, date, taxpayer] %+=% income
    cumulative_income <- sum(output$metrics$taxable_income[currency, current_year, taxpayer])
    cumulative_tax <- sum(output$metrics$tax[currency, current_year, taxpayer])
    tax_rate <- tax_rate[which.min(abs(cumulative_income - threshold))]
    tax <- tax_rate * cumulative_income - cumulative_tax
    output$metrics$tax[currency, date, taxpayer] %+=% tax
  }

  if (length(taxpayer) == 2) {
    output$metrics$taxable_income[currency, date, taxpayer[1]] %+=% (income / 2)
    cumulative_income_1 <- sum(output$metrics$taxable_income[currency, current_year, taxpayer[1]])
    tax_rate_1 <- tax_rate[which.min(abs(cumulative_income_1 - threshold))]
    cumulative_tax_1 <- sum(output$metrics$tax[currency, current_year, taxpayer[1]])
    tax_1 <- tax_rate_1 * cumulative_income_1 - cumulative_tax_1
    output$metrics$tax[currency, date, taxpayer[1]] %+=% tax_1
    tax_1 <- output$metrics$tax[currency, date, taxpayer[1]]

    output$metrics$taxable_income[currency, date, taxpayer[2]] %+=% (income / 2)
    cumulative_income_2 <- sum(output$metrics$taxable_income[currency, current_year, taxpayer[2]])
    tax_rate_2 <- tax_rate[which.min(abs(cumulative_income_2 - threshold))]
    cumulative_tax_2 <- sum(output$metrics$tax[currency, current_year, taxpayer[2]])
    tax_2 <- tax_rate_2 * cumulative_income_2 - cumulative_tax_2
    output$metrics$tax[currency, date, taxpayer[2]] %+=% tax_2
    tax_2 <- output$metrics$tax[currency, date, taxpayer[2]]
    tax <- tax_1 + tax_2
  }

  tax_reporting_currency <- FX * tax
  output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$income_tax[portfolio_name, date] %+=% tax_reporting_currency

  return(output)
}
