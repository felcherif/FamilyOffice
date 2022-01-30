#' @title expenses
#' @description create cash flow for expenses
#' @family cash flow function
#' @param x expense object
#' @param output all in memory
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @export

expenses <- function(x, output) {

  "%+=%" <- roperators::`%+=%`
  "%-=%" <- roperators::`%-=%`
  "%*=%" <- roperators::`%*=%`

  # reporting calculation value needed
  date           <- output$reporting$reporting_date
  portfolio_name <- output$reporting$portfolio_name
  frequency      <- output$reporting$frequency_number
  FX             <- output$economic_data$exchange_rate[[x@currency]]

  reporting_dates <- as.character(output$reporting$reporting_dates)
  previous_date   <- which(reporting_dates == date) - 1
  Previous_date   <- as.character(reporting_dates[previous_date])

  expenses_names <- names(x@expenses)

  if (date == as.character(output$reporting$reporting_dates[1])) {
    metric <- c()
    for (exp in expenses_names) {
      if (is.element(el = exp, set = expenses_names)) {
        metric <- c(metric, x@expenses[[exp]] / frequency * FX)
      }
    }

    metric <- matrix(
      data = c(metric,
               0,
               FX,
               frequency),
      nrow = length(expenses_names) + 3,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(expenses_names,
          "total_expense",
          "FX",
          "frequency"),
        as.character(output$reporting$reporting_dates)))
  } else {
    metric <- output$Portfolio$portfolio[[portfolio_name]]@metrics$metric
  }

  # adjust for inflation
  if (date != output$reporting$reporting_dates[1]) {
    for (exp in expenses_names) {
      metric[exp, date] <- inflation_adjustment(amount = metric[exp, Previous_date], currency = x@currency, output = output)
    }
  }

  metric["total_expense", date] <- total_expense <- sum(metric[c(expenses_names), date])

  # store metric
  output$Portfolio$portfolio[[portfolio_name]]@metrics$metric <- metric

  # store income statements
  for (exp in expenses_names) {
    output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$personal_expenses[exp, date] <- metric[exp, date]
  }

  # store balance sheet
  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$cash[output$Portfolio$class$cash_account_class, date] %-=% total_expense

  # store cash flow statement
  for (expense in expenses_names) {
    output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$personal_expense[expense, date] <- metric[expense, date]
  }

  # sum single portfolio name impact into global statement
  output <- portfolio_to_statement(output = output, portfolio_name = portfolio_name, date = date)

  # store marginal impact of single portfolio name
  output$Portfolio$portfolio[[portfolio_name]] <- statement_total_program(X = output$Portfolio$portfolio[[portfolio_name]], reporting_date = date)

  return(output)
}

