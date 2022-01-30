#' @title annuity
#' @family cash flow
#' @description function that projects the debt in the financial statements
#' @param x annuity object name
#' @param output is object containing all data
#' @return this function returns a text file 'output' updated for annuity
#' @export

annuity <- function(x, output) {

  "%+=%" <- roperators::`%+=%`
  "%-=%" <- roperators::`%-=%`
  "%*=%" <- roperators::`%*=%`

  taxpayer <- x@taxpayer
  date <- as.character(output$reporting$reporting_date)
  portfolio_name <- output$reporting$portfolio_name
  currency <- x@currency
  FX <- output$economic_data$exchange_rate[[currency]]
  frequency <- output$reporting$frequency_number

  reporting_dates <- as.character(output$reporting$reporting_dates)
  previous_date <- which(reporting_dates == date) - 1
  Previous_date <- as.character(reporting_dates[previous_date])

  if (date == output$reporting$reporting_dates[1]) {
    metric <- matrix(
      data = c(
        x@minimum_age_payment,
        x@maximum_age_payment,
        0,
        0,
        0,
        0,
        0,
        FX,
        0,
        frequency,
        0
      ),
      nrow = 11,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(
          "minimum_age_payment",
          "maximum_age_payment",
          "annual_notional",
          "payment",
          "payment_FX_reporting",
          "payment_tax",
          "payment_net",
          "FX",
          "age",
          "frequency",
          "in_payment"
        ),
        as.character(output$reporting$reporting_dates)
      )
    )
    annual_notional <- x@annual_notional
  } else {
    metric <- output$Portfolio$portfolio[[portfolio_name]]@metrics$metric
    annual_notional <- metric["annual_notional", Previous_date]
  }

  metric["age", date] <- age <- output$reporting$age[[taxpayer]][, date]
  in_payment <- ((age >= x@minimum_age_payment) & (age <= x@maximum_age_payment))

  if (x@inflation_indexation) {
    annual_notional <- inflation_adjustment(amount = annual_notional, currency = currency, output = output)
  }

  if (in_payment) {
    payment <- annual_notional / frequency
    payment_FX_reporting <- payment * FX

    if (length(x@tax_rate) != 0) {
      tax_rate <- output$Portfolio$portfolio[[portfolio_name]]@tax_rate
      payment_tax <- payment_FX_reporting * tax_rate
    }

    if (length(x@tax_rate) == 0) {
      output <- tax_calculations(x = x, output = output, income = payment_FX_reporting)
      payment_tax <- output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$income_tax["total", date]
    }

    payment_net <- payment_FX_reporting - payment_tax
  } else {
    payment <- 0
    payment_tax <- 0
    payment_net <- 0
    payment_FX_reporting <- 0
  }

  metric["payment_tax", date] <- payment_tax
  metric["payment_FX_reporting", date] <- payment_FX_reporting
  metric["payment", date] <- payment
  metric["in_payment", date] <- in_payment
  metric["payment_net", date] <- payment_net
  metric["annual_notional", date] <- annual_notional

  # store metrics
  output$Portfolio$portfolio[[portfolio_name]]@metrics$metric <- metric

  # store income statement
  output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$income$salary[portfolio_name, date] <- payment_FX_reporting

  # store balance sheet
  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$cash[output$Portfolio$class$cash_account, date] %+=% payment_net

  # store cash flow statement
  output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$income_taxes_paid[portfolio_name, date] <- payment_tax
  output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$salary_income[portfolio_name, date] <- payment

  output <- portfolio_to_statement(output = output, portfolio_name = portfolio_name, date = date)

  output$Portfolio$portfolio[[portfolio_name]] <- statement_total_program(X = output$Portfolio$portfolio[[portfolio_name]], reporting_date = date)

  return(output)
}
