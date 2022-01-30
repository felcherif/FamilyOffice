#' @title cash_account
#' @family cash flow
#' @description function that projects the debt in the financial statements
#' @param x annuity object name
#' @param output is object containing all data
#' @return this function returns a text file 'output' updated for cash account
#' @export

cash_account <- function(x, output) {

  "%+=%" <- roperators::`%+=%`
  "%-=%" <- roperators::`%-=%`
  "%*=%" <- roperators::`%*=%`

  # reporting calculation value needed
  date <- output$reporting$reporting_date
  reporting_dates <- as.character(output$reporting$reporting_dates)
  next_date <- which(reporting_dates == date) + 1
  previous_date <- which(reporting_dates == date) - 1
  Previous_date <- as.character(reporting_dates[previous_date])
  Next_date <- as.character(reporting_dates[next_date])
  portfolio_name <- output$reporting$portfolio_name
  frequency <- output$reporting$frequency_number
  reporting_currency <- output$reporting$reporting_currency
  interest <- 0
  CAD_FX <- output$economic_data$exchange_rate$CAD
  EUR_FX <- output$economic_data$exchange_rate$EUR
  USD_FX <- output$economic_data$exchange_rate$USD
  GBP_FX <- output$economic_data$exchange_rate$GBP
  KRW_FX <- output$economic_data$exchange_rate$KRW
  CHF_FX <- output$economic_data$exchange_rate$CHF

  if (date == as.character(output$reporting$reporting_dates[1])) {
    metric <- matrix(
      data = c(
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        CAD_FX,
        EUR_FX,
        USD_FX,
        GBP_FX,
        KRW_FX,
        CHF_FX
      ),
      nrow = 13,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(
          "interest",
          "CAD",
          "EUR",
          "USD",
          "GBP",
          "KRW",
          "CHF",
          "CAD_FX",
          "EUR_FX",
          "USD_FX",
          "GBP_FX",
          "KRW_FX",
          "CHF_FX"
        ),
        as.character(output$reporting$reporting_dates)
      )
    )
  } else {
    metric <- output$Portfolio$portfolio[[portfolio_name]]@metrics$metric
  }

  if (date == reporting_dates[1]) {
    money <- x@CAD * CAD_FX + x@GBP * GBP_FX + x@USD * USD_FX + x@EUR * EUR_FX + x@KRW * KRW_FX + x@CHF * CHF_FX
    output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$cash["total", date] %+=% money
    metric[reporting_currency, date] <- output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$cash["total", date]
  } else {
    money <- metric[reporting_currency, Previous_date]
    money %+=% output$statement$cash_flow_statement$total$increase_in_cash["increase_in_cash", Previous_date]
  }

  if (next_date <= length(reporting_dates) + 1) {
    if (money >= 0) {
      rate <- output$economic_data$cash_rate[[reporting_currency]][1]
    }
    if (money < 0) {
      rate <- output$economic_data$cash_rate[[reporting_currency]][2]
    }
    interest <- money * ((1 + rate / frequency)^(1 / frequency) - 1)
    money %+=% interest
    output$statement$cash_flow_statement$total$increase_in_cash["increase_in_cash", date] %+=% interest
    output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$income$interest_earned[output$Portfolio$class$cash_account_class, date] <- interest
    output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$investing_activities$interest_earned[portfolio_name, date] <- interest
    output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$total$interest_earned["interest_earned", date] <- interest
    metric["CAD", date] <- 0
    metric["EUR", date] <- 0
    metric["USD", date] <- 0
    metric["GBP", date] <- 0
    metric["KRW", date] <- 0
    metric["CHF", date] <- 0
    metric[reporting_currency, date] <- money
  }

  # tax calculation
  output <- tax_calculations(x = x, output = output, income = interest)

  output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$income_tax[portfolio_name, date] <- output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$income_tax["total", date]

  output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$income_taxes_paid[portfolio_name, date] <- output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$income_tax[portfolio_name, date]

  # store balance sheet
  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$cash[output$Portfolio$class$cash_account_class, date] <- money
  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$equity$total[portfolio_name, date] <- money

  # store metrics
  output$Portfolio$portfolio[[portfolio_name]]@metrics$metric <- metric
  output <- portfolio_to_statement(output = output, portfolio_name = portfolio_name, date = date)
  output$Portfolio$portfolio[[portfolio_name]] <- statement_total_program(X = output$Portfolio$portfolio[[portfolio_name]], reporting_date = date)

  if (next_date <= length(reporting_dates)) {
    output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$cash["total", Next_date] <- output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$cash["total", date]
  }

  return(output)
}
