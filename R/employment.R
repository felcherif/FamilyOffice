#' @title employment
#' @description function that projects the employment in the financial statements
#' @param x is the employment object
#' @param output is the only parameter 'input' needed.
#' @return this function returns a output with the statement modified by employment x
#' @export

employment <- function(x, output) {

  "%+=%" <- roperators::`%+=%`
  "%-=%" <- roperators::`%-=%`
  "%*=%" <- roperators::`%*=%`

  date <- output$reporting$reporting_date
  portfolio_name <- output$reporting$portfolio_name
  currency <- x@currency
  taxpayer <- x@taxpayer
  FX <- output$economic_data$exchange_rate[[currency]]

  reporting_dates <- as.character(output$reporting$reporting_dates)
  next_date <- which(reporting_dates == date) + 1
  previous_date <- which(reporting_dates == date) - 1

  if (date == output$reporting$reporting_dates[1]) {
    metric <- matrix(
      data = c(
        x@retirement_age,
        x@yearly_salary,
        0,
        0,
        0,
        0,
        0,
        0,
        output$economic_data$exchange_rate[[x@currency]]
      ),
      nrow = 9,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(
          "retirement_age",
          "yearly_salary",
          "gross_salary",
          "tax_reporting_currency",
          "net_salary",
          "age",
          "retired",
          "paycheck_reporting_currency",
          "FX"
        ),
        as.character(output$reporting$reporting_dates)
      )
    )
  } else {
    metric <- output$Portfolio$portfolio[[portfolio_name]]@metrics$metric
  }
  metric["age", date] <- age <- output$reporting$age[[x@taxpayer]][, date]
  metric["retired", date] <- retired <- output$reporting$age[[x@taxpayer]][, date] >= x@retirement_age
  tax_rate <- x@tax_rate

  gross_salary <- 0
  net_salary <- 0
  if (retired) {
    metric["yearly_salary", date] <- 0
  }
  if (!retired) {
    frequency <- output$reporting$frequency_number
    if (date != as.character(output$reporting$reporting_dates[1])) {
      metric["yearly_salary", date] <- inflation_adjustment(amount = metric["yearly_salary", previous_date], currency = x@currency, output = output)
    }
    yearly_salary <- metric["yearly_salary", date]
    if (length(tax_rate) == 0) {
      output <- tax_calculations(x, output, income = yearly_salary / frequency)
      metric["tax_reporting_currency", date] <- output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$income_tax[portfolio_name, date]
    }
    paycheck_reporting_currency <- FX * yearly_salary / frequency
    metric["paycheck_reporting_currency", date] <- paycheck_reporting_currency
    gross_salary <- paycheck_reporting_currency
    metric["gross_salary", date] <- gross_salary
    if (length(tax_rate) != 0) {
      metric["tax_reporting_currency", date] <- gross_salary * tax_rate
    }
    net_salary <- gross_salary - metric["tax_reporting_currency", date]
    metric["net_salary", date] <- net_salary
  }

  # store metrics
  output$Portfolio$portfolio[[portfolio_name]]@metrics$metric <- metric

  # store income statement
  output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$income$salary[portfolio_name, date] <- gross_salary
  output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$income_tax[portfolio_name, date] <- metric["tax_reporting_currency", date]

  # store balance sheet
  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$cash[output$Portfolio$class$cash_account, date] %+=% net_salary

  # store cash flow statement
  output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$salary_income[portfolio_name, date] <- gross_salary
  output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$income_taxes_paid[portfolio_name, date] <- metric["tax_reporting_currency", date]

  output <- portfolio_to_statement(output, portfolio_name, date)

  output$Portfolio$portfolio[[portfolio_name]] <- statement_total_program(X = output$Portfolio$portfolio[[portfolio_name]], reporting_date = date)

  return(output)
}
