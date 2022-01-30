#' @title pension
#' @description produce cash flow for pension object
#' @family cash flow function
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @param x the pension object
#' @param output all in memory
#' @return output with updated pension
#' @export

pension <- function(x, output) {

  withdrawal_tax <- NULL
  
  "%+=%" <- roperators::`%+=%`
  "%-=%" <- roperators::`%-=%`
  "%*=%" <- roperators::`%*=%`

  taxpayer <- x@taxpayer
  date <- output$reporting$reporting_date
  age <- output$reporting$age[[taxpayer]][, date]
  portfolio_name <- output$reporting$portfolio_name
  minimum_age_withdrawal <- x@minimum_age_withdrawal
  withdrawal_in_payment <- age >= minimum_age_withdrawal
  frequency <- output$reporting$frequency_number
  tax_rate <- output$Portfolio$portfolio[[portfolio_name]]@tax_rate
  currency <- x@currency
  FX <- output$economic_data$exchange_rate[[currency]]

  reporting_dates <- as.character(output$reporting$reporting_dates)
  next_date <- which(reporting_dates == date) + 1
  previous_date <- which(reporting_dates == date) - 1
  Previous_date <- as.character(reporting_dates[previous_date])

  if (date == as.character(output$reporting$reporting_dates[1])) {
    metric <- matrix(
      data = c(
        age,
        withdrawal_in_payment,
        FX,
        frequency,
        0,
        0,
        0,
        0,
        0,
        0,
        x@value,
        0
      ),
      nrow = 12,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(
          "age",
          "withdrawal_in_payment",
          "FX",
          "frequency",
          "withdrawal",
          "withdrawal_tax",
          "withdrawal_net",
          "withdrawal_reporting_currency",
          "withdrawal_tax_reporting_currency",
          "withdrawal_net_reporting_currency",
          "value",
          "value_reporting_currency"
        ),
        as.character(output$reporting$reporting_dates)
      )
    )
  } else {
    metric <- output$Portfolio$portfolio[[portfolio_name]]@metrics$metric
    metric["value", date] <- inflation_adjustment(amount = metric["value", Previous_date], currency = x@currency, output = output)
    metric["value", date] <- risk_premium_adjustment(amount = metric["value", date], asset_class = "share_class", output = output)
    metric["age", date] <- age
    metric["withdrawal_in_payment", date] <- withdrawal_in_payment
    metric["FX", date] <- FX
    metric["frequency", date] <- frequency
  }

  if (withdrawal_in_payment) {
    cash_withdrawal <- output$Portfolio$portfolio[[portfolio_name]]@cash_withdrawal
    cash_withdrawal <- cash_withdrawal / frequency
    percentage_withdrawal <- output$Portfolio$portfolio[[portfolio_name]]@percentage_withdrawal
    if (length(cash_withdrawal) != 0 & metric["value", date] > 0) {
      metric["value", date] %-=% cash_withdrawal
      if (length(x@tax_rate) == 0) {
        metric["withdrawal_tax", date] <- tax_calculations(x = x, output = output, income = cash_withdrawal)
      }
      if (length(x@tax_rate) != 0) {
        metric["withdrawal_tax", date] <- cash_withdrawal * tax_rate
      }
      metric["withdrawal_net", date] <- cash_withdrawal - withdrawal_tax
    } else {
      if (length(percentage_withdrawal) != 0 & metric["value", date] > 0) {
        metric["value", date] <- metric["value", date] * (1 - percentage_withdrawal / frequency)
        metric["withdrawal", date] <- percentage_withdrawal / frequency * metric["value", date]
        if (length(x@tax_rate) != 0) {
          metric["withdrawal_tax", date] <- percentage_withdrawal * tax_rate
        }
        if (length(x@tax_rate) == 0) {
          output <- tax_calculations(x = x, output = output, income = metric["withdrawal", date])
          metric["withdrawal_tax", date] <- output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$income_tax["total", date]
        }
        metric["withdrawal_reporting_currency", date] <- metric["withdrawal", date] * FX
        metric["withdrawal_tax_reporting_currency", date] <- metric["withdrawal_tax", date] * FX
        metric["withdrawal_net", date] %-=% (-metric["withdrawal", date])
        metric["withdrawal_net_reporting_currency", date] <- metric["withdrawal_net", date] * FX
      }
    }
  }

  # adjust for reporting currency and period (frequency)
  metric["value_reporting_currency", date] <- metric["value", date] * FX

  # store metrics
  output$Portfolio$portfolio[[portfolio_name]]@metrics$metric <- metric

  # store income statement
  output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$income$salary[portfolio_name, date] <- metric["withdrawal", date]
  output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$income_tax[portfolio_name, date] <- metric["withdrawal_tax", date]

  # store balance sheet
  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$pension[portfolio_name, date] %+=% metric["value_reporting_currency", date]

  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$cash[output$Portfolio$class$cash_account_class, date] %+=% metric["withdrawal_net", date]

  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$equity$total[portfolio_name, date] <- metric["value_reporting_currency", date]

  # store cash flow statement
  output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$salary_income[portfolio_name, date] <- metric["withdrawal", date]
  output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$income_taxes_paid[portfolio_name, date] <- metric["withdrawal_tax", date]

  # sum portfolio impact into statement
  output <- portfolio_to_statement(output = output, portfolio_name = portfolio_name, date = date)

  # store marginal impact of single portfolio name
  output$Portfolio$portfolio[[portfolio_name]] <- statement_total_program(X = output$Portfolio$portfolio[[portfolio_name]], reporting_date = date)

  return(output)
}
