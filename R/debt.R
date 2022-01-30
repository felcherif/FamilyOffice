#' @title debt
#' @description function that projects the debt in the financial statements
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @param x is the debt object
#' @param output is the only parameter 'input' needed is the input files name.
#' @return this function returns a output with the statement modified by debt x
#' @export

debt <- function(x, output) {

  "%+=%" <- roperators::`%+=%`
  "%-=%" <- roperators::`%-=%`
  "%*=%" <- roperators::`%*=%`

  date <- output$reporting$reporting_date
  portfolio_name <- output$reporting$portfolio_name
  frequency <- output$reporting$frequency_number
  FX <- output$economic_data$exchange_rate[[x@currency]]

  reporting_dates <- as.character(output$reporting$reporting_dates)
  previous_date <- which(reporting_dates == date) - 1
  Previous_date <- as.character(reporting_dates[previous_date])

  if (length(x@promo_interest_rate)) {
    if (date < x@promo_interest_rate_end_date) {
      IR <- x@promo_interest_rate
    } else {
      IR <- x@interest_rate
    }
  } else {
    IR <- x@interest_rate
  }

  if (date == as.character(output$reporting$reporting_dates[1])) {
    metric <- matrix(
      data = c(
        IR,
        x@balance,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        FX
      ),
      nrow = 12,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(
          "interest_rate",
          "balance",
          "balance_reporting_currency",
          "interest",
          "interest_reporting_currency",
          "repayment_cash",
          "repayment_capital",
          "repayment_interest",
          "repayment_reporting_currency",
          "repayment_capital_reporting_currency",
          "repayment_interest_reporting_currency",
          "FX"
        ),
        as.character(reporting_dates)
      )
    )
  } else {
    metric <- output$Portfolio$portfolio[[portfolio_name]]@metrics$metric
    metric["balance", date] <- metric["balance", Previous_date]
  }
  metric["interest", date] <- interest <- metric["balance", date] * ((1 + IR)^(1 / frequency) - 1)
  balance <- metric["balance", date]

  if (length(x@repayment_cash_annual) != 0 & balance >= 0) {
    repayment_cash <- x@repayment_cash_annual / frequency
  } else {
    if (length(x@repayment_percentage_annual) != 0 & balance >= 0) {
      repayment_cash <- metric["balance", date] * x@repayment_percentage_annual / frequency
    }
  }

  if (balance < 0) {
    interest <- 0
    repayment_cash <- balance
    balance <- 0
  } else {
    if (balance > 0) {
      if (repayment_cash > (balance + interest)) {
        repayment_cash <- balance + interest
      }
      balance <- max(0, balance + interest - repayment_cash)
    } else {
      if (balance == 0) {
        balance <- 0
        repayment_cash <- 0
      }
    }
  }

  metric["balance", date] <- balance
  metric["balance_reporting_currency", date] <- balance_reporting_currency <- FX * balance
  metric["interest", date] <- interest
  metric["interest_reporting_currency", date] <- interest_reporting_currency <- FX * interest
  metric["repayment_cash", date] <- repayment_cash
  metric["repayment_capital", date] <- repayment_capital <- repayment_cash - interest
  metric["repayment_interest", date] <- repayment_interest <- repayment_cash - repayment_capital
  metric["repayment_reporting_currency", date] <- repayment_reporting_currency <- FX * repayment_cash
  metric["repayment_capital_reporting_currency", date] <- repayment_capital_reporting_currency <- FX * repayment_capital
  metric["repayment_interest_reporting_currency", date] <- repayment_interest_reporting_currency <- FX * repayment_interest

  # store metric
  output$Portfolio$portfolio[[portfolio_name]]@metrics$metric <- metric

  # store income statement
  output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$interest_non_deductible[portfolio_name, date] %+=% repayment_interest_reporting_currency

  # store balance sheet
  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$liabilities$short_term_debt[portfolio_name, date] %+=% balance_reporting_currency
  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$cash[output$Portfolio$class$cash_account_class, date] %-=% repayment_reporting_currency
  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$equity$total[portfolio_name, date] %-=% balance_reporting_currency

  # store cash flow statement
  output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$short_term_debt_repayment[portfolio_name, date] %+=% repayment_capital_reporting_currency
  output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$interest_paid_non_deductible[portfolio_name, date] %+=% repayment_interest_reporting_currency
  output <- portfolio_to_statement(output = output, portfolio_name = portfolio_name, date = date)
  output$Portfolio$portfolio[[portfolio_name]] <- statement_total_program(X = output$Portfolio$portfolio[[portfolio_name]], reporting_date = date)

  return(output)
}
